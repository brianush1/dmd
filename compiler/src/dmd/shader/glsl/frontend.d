module dmd.shader.glsl.frontend;
import dmd.common.outbuffer;
import dmd.root.filename;
import dmd.root.file;
import dmd.visitor;
import dmd.astcodegen;
import dmd.astenums;
import dmd.tokens;
import dmd.root.rootobject;
import dmd.aggregate;
import dmd.aliasthis;
import dmd.arraytypes;
import dmd.attrib;
import dmd.cond;
import dmd.dclass;
import dmd.declaration;
import dmd.denum;
import dmd.dimport;
import dmd.dmodule;
import dmd.dstruct;
import dmd.dsymbol;
import dmd.dtemplate;
import dmd.dversion;
import dmd.expression;
import dmd.func;
import dmd.hdrgen;
import dmd.init;
import dmd.initsem;
import dmd.mtype;
import dmd.nspace;
import dmd.statement;
import dmd.staticassert;
import dmd.typesem;
import dmd.ctfeexpr;
import dmd.identifier;
import dmd.dmangle;
import dmd.errors;
import core.vararg;
import std.typecons : Nullable;
static import ir = dmd.shader.glsl.ir;

// TODO: replace all "internal compiler error"s with better messages

import std.stdio : stderr, writeln;

string STMTtoString(STMT stmt) {
	final switch (stmt)
	{
		case STMT.Error:         return "Error";
		case STMT.Scope:         return "Scope";
		case STMT.Exp:           return "Exp";
		case STMT.Compound:      return "Compound";
		case STMT.Return:        return "Return";
		case STMT.If:            return "If";
		case STMT.Conditional:   return "Conditional";
		case STMT.StaticForeach: return "StaticForeach";
		case STMT.Case:          return "Case";
		case STMT.Default:       return "Default";
		case STMT.Label:         return "Label";
		case STMT.Goto:          return "Goto";
		case STMT.GotoDefault:   return "GotoDefault";
		case STMT.GotoCase:      return "GotoCase";
		case STMT.Break:         return "Break";
		case STMT.DtorExp:       return "DtorExp";
		case STMT.Mixin:         return "Mixin";
		case STMT.Forwarding:    return "Forwarding";
		case STMT.Do:            return "Do";
		case STMT.While:         return "While";
		case STMT.For:           return "For";
		case STMT.Foreach:       return "Foreach";
		case STMT.Switch:        return "Switch";
		case STMT.Continue:      return "Continue";
		case STMT.With:          return "With";
		case STMT.TryCatch:      return "TryCatch";
		case STMT.Throw:         return "Throw";
		case STMT.Debug:         return "Debug";
		case STMT.TryFinally:    return "TryFinally";
		case STMT.ScopeGuard:    return "ScopeGuard";
		case STMT.SwitchError:   return "SwitchError";
		case STMT.UnrolledLoop:  return "UnrolledLoop";
		case STMT.ForeachRange:  return "ForeachRange";
		case STMT.CompoundDeclaration: return "CompoundDeclaration";
		case STMT.Peel:          return "Peel";
		case STMT.CompoundAsm:   return "CompoundAsm";
		case STMT.Pragma:        return "Pragma";
		case STMT.StaticAssert:  return "StaticAssert";
		case STMT.CaseRange:     return "CaseRange";
		case STMT.Synchronized:  return "Synchronized";
		case STMT.Asm:           return "Asm";
		case STMT.InlineAsm:     return "InlineAsm";
		case STMT.GccAsm:        return "GccAsm";
		case STMT.Import:        return "Import";
	}
}

Nullable!(ir.POD) TYtoPOD(TY ty) {
	switch (ty) {
	case TY.Tvoid:
		return typeof(return)(ir.POD.Void);
	case TY.Tbool:
		return typeof(return)(ir.POD.Bool);
	case TY.Tint32:
	case TY.Tint64:
		return typeof(return)(ir.POD.Int);
	case TY.Tuns32:
	case TY.Tuns64:
		return typeof(return)(ir.POD.UInt);
	case TY.Tfloat32:
		return typeof(return)(ir.POD.Float);
	case TY.Tfloat64:
	case TY.Tfloat80:
		return typeof(return)(ir.POD.Double);
	default:
		return typeof(return).init;
	}
}

final class SymbolGLSL {
	ir.Identifier name;
	Dsymbol sym;
	Dsymbol[] refs;
	ir.Declaration source;
}

enum NativeTypeBase {
	none,
	gvec2,
	gvec3,
	gvec4,
	gsampler1D,
	gsampler2D,
	gsampler3D,
}

struct NativeType {
	NativeTypeBase base;
	ir.POD pod;

	string toString() const {
		final switch (base) {
			case NativeTypeBase.none: return "none";
			case NativeTypeBase.gvec2: return ir.getPODPrefix(pod) ~ "vec2";
			case NativeTypeBase.gvec3: return ir.getPODPrefix(pod) ~ "vec3";
			case NativeTypeBase.gvec4: return ir.getPODPrefix(pod) ~ "vec4";
			case NativeTypeBase.gsampler1D: return ir.getPODPrefix(pod) ~ "sampler1D";
			case NativeTypeBase.gsampler2D: return ir.getPODPrefix(pod) ~ "sampler2D";
			case NativeTypeBase.gsampler3D: return ir.getPODPrefix(pod) ~ "sampler3D";
		}
	}
}

enum AttribGLSL {
	none = 0x000,

	vertexShader = 0x100,
	fragmentShader = 0x101,

	varying = 0x200,
	uniform = 0x201,
	flat = 0x202,
	output = 0x203,
}

extern (C++) final class TargetGLSL : Visitor
{
	alias visit = Visitor.visit;

	SymbolGLSL[void*] symbols;

	ir.Stmt[]* currentBody;

	SymbolGLSL currSymbol;
	SymbolGLSL[] symbolStack;

	void enterSymbol(Dsymbol sym, bool hidden = false) {
		symbolStack ~= currSymbol;
		currSymbol = new SymbolGLSL();
		if (!hidden)
			symbols[cast(void*) sym] = currSymbol;
		currSymbol.sym = sym;
		currSymbol.name = mangleOf(sym);
	}

	void exitSymbol() {
		currSymbol = symbolStack[$ - 1];
		symbolStack = symbolStack[0 .. $ - 1];
	}

	bool[string] nameSet;
	ir.Identifier[void*] nameMap;

	extern (D) NativeType nativeTypeOf(Expression exp) {
		if (TypeStruct s = exp.type.isTypeStruct) {
			return nativeTypeOf(s.sym);
		}
		else {
			return NativeType();
		}
	}

	extern (D) NativeType nativeTypeOf(Dsymbol sym) {
		string fqn = fqnOf(sym);
		if (StructDeclaration s = sym.isStructDeclaration) {
			ir.POD pod;
			if (!sym.parent)
				return NativeType();
			if (TemplateInstance ti = sym.parent.isTemplateInstance) {
				if (ti.tdtypes.length == 0)
					return NativeType();

				Type t = isType(ti.tdtypes[0]);
				if (!t || !t.isTypeBasic)
					return NativeType();

				TY ty = t.isTypeBasic.ty;
				auto p = TYtoPOD(ty);
				if (p.isNull)
					return NativeType();

				pod = p.get;
			}
			else {
				return NativeType();
			}

			if (fqn == "gd.math.vec.TVec2") {
				return NativeType(NativeTypeBase.gvec2, pod);
			}
			else if (fqn == "gd.math.vec.TVec3") {
				return NativeType(NativeTypeBase.gvec3, pod);
			}
			else if (fqn == "gd.math.vec.TVec4") {
				return NativeType(NativeTypeBase.gvec4, pod);
			}
			else if (fqn == "gd.shaders.TSampler1D") {
				return NativeType(NativeTypeBase.gsampler1D, pod);
			}
			else if (fqn == "gd.shaders.TSampler2D") {
				return NativeType(NativeTypeBase.gsampler2D, pod);
			}
			else if (fqn == "gd.shaders.TSampler3D") {
				return NativeType(NativeTypeBase.gsampler3D, pod);
			}
			else {
				return NativeType();
			}
		}
		else {
			return NativeType();
		}
	}

	ir.Identifier[void*] expVarNames;
	extern (D) ir.Identifier getVar(Expression exp) {
		if (cast(void*) exp !in expVarNames) {
			import std.conv : text;
			return new ir.Identifier("unbound(" ~ text(exp) ~ ")");
		}

		return expVarNames[cast(void*) exp];
	}

	extern (D) void rename(Expression e, ir.Identifier to) {
		expVarNames[cast(void*) e] = to;
	}

	extern (D) void rename(Expression e, Expression to) {
		rename(e, getVar(to));
	}

	extern (D) void expv(ir.Op node, Expression e, bool isRef) {
		if (cast(void*) e !in expVarNames) {
			expVarNames[cast(void*) e] = new ir.Identifier();
		}

		node.loc = e.loc;
		node.name = getVar(e);
		node.type = visitType(e.type);
		node.isRef = isRef;
	}

	extern (D) ir.Identifier mangleOf(Dsymbol value) {
		currSymbol.refs ~= value;

		if (cast(void*) value in nameMap)
			return nameMap[cast(void*) value];

		NativeType nativeType = nativeTypeOf(value);
		if (nativeType != NativeType()) {
			return nameMap[cast(void*) value] = new ir.Identifier(nativeType.toString(), true);
		}
		else {
			import dmd.dmangle : mangleToBuffer;

			// OutBuffer mangleBuf;
			// mangleToBuffer(shader.classSym, &mangleBuf);
			// string mangled = mangleBuf[].idup;
			// assert(mangled[0 .. 2] != "_D");

			string baseName;
			if (TemplateInstance ti = value.isTemplateInstance) {
				baseName = ti.name.toString.idup;
			}
			else {
				baseName = value.ident.toString.idup;
			}

			return nameMap[cast(void*) value] = new ir.Identifier(baseName);
		}
	}

	private enum UNAVAILABLE_MSG = " are not available in shader code";
	extern (D) void unavailable(Expression e, string thing) {
		ir.ErrorOp node = new ir.ErrorOp(thing ~ UNAVAILABLE_MSG);
		expv(node, e, false);
		*currentBody ~= node;
	}

	extern (D) ir.Identifier[] writeArgs(Expressions* args, Expression basis = null) {
		if (!args)
			return [];

		ir.Identifier[] result;
		foreach (i; 0 .. args.length) {
			if (i >= args.length || (*args)[i] is null) {
				basis.accept(this);
				result ~= getVar(basis);
			}
			else {
				(*args)[i].accept(this);
				result ~= getVar((*args)[i]);
			}
		}
		return result;
	}

	VarDeclaration vthis;

public:

	override void visit(Dsymbol sym) {}

	override void visit(AttribDeclaration d) {
		if (!d.decl)
			return;

		foreach (child; *d.decl)
			child.accept(this);
	}

	override void visit(UserAttributeDeclaration d) {
		visit(cast(AttribDeclaration) d);
	}

	override void visit(ScopeDsymbol sds) {
		assert(0, "unhandled ScopeDsymbol");
	}

	override void visit(Module mod) {
		mod.members.foreachDsymbol((s) {
			s.accept(this);
		});
	}

	override void visit(EnumDeclaration ed) {}

	private ir.Struct[void*] irStructs;
	private ir.Struct getIRStruct(StructDeclaration sd) {
		if (cast(void*) sd in irStructs) {
			return irStructs[cast(void*) sd];
		}

		ir.Struct result = new ir.Struct();
		result.loc = sd.loc;
		result.name = mangleOf(sd);
		result.originalSymbol = sd;
		return irStructs[cast(void*) sd] = result;
	}

	override void visit(StructDeclaration sd) {
		enterSymbol(sd);
		scope (exit)
			exitSymbol();

		NativeType nativeType = nativeTypeOf(sd);

		bool[void*] handled;

		if (nativeType == NativeType()) {
			ir.Struct node = getIRStruct(sd);
			currSymbol.source = node;

			foreach (i, VarDeclaration vd; sd.fields) {
				import std.conv : text;

				handled[cast(void*) vd] = true;

				// TODO: avoid hijacking the existing identifier if we can
				ir.Identifier id = mangleOf(vd);
				id.modify(text("f", i), true);

				node.fields ~= ir.Field(
					visitType(vd.type),
					mangleOf(vd),
				);
			}
		}

		void handleChild(Dsymbol s) {
			if (AttribDeclaration ad = s.isAttribDeclaration) {
				if (ad.decl is null)
					return;

				foreach (child; *ad.decl) {
					if (cast(void*) child !in handled)
						handleChild(child);
				}
			}
			else if (VarDeclaration vd = s.isVarDeclaration) {
				// TODO: why does this happen?
				// assert(cast(void*) vd in handled, vd.ident.toString.idup ~ " not handled");
			}
			else {
				s.accept(this);
			}
		}

		sd.members.foreachDsymbol(&handleChild);
	}

	override void visit(ClassDeclaration cd) {
		bool hidden = fqnOf(cd.baseClass) != "gd.shaders.Shader";
		enterSymbol(cd, hidden);
		scope (exit)
			exitSymbol();

		ir.ShaderClass node = new ir.ShaderClass();
		node.loc = cd.loc;
		node.name = mangleOf(cd);
		node.originalSymbol = cd;
		currSymbol.source = node;

		bool[void*] handled;

		foreach (vd; cd.fields) {
			handled[cast(void*) vd] = true;
			ir.GlobalQualifiers qualifiers = ir.GlobalQualifiers.Uniform;

			// FIXME: this doesn't seem to handle multiple UDAs on the same symbol
			AttribGLSL[] attribs;
			if (UserAttributeDeclaration uda = vd.userAttribDecl) {
				foreach (e; *uda.atts) {
					if (IntegerExp ie = e.isIntegerExp)
						if (TypeEnum t = ie.type.isTypeEnum)
							if (t.sym.ident.toString == "__glsl")
								attribs ~= cast(AttribGLSL) ie.toInteger;
				}
			}

			foreach (attrib; attribs) {
				if (attrib == AttribGLSL.varying)
					qualifiers = ir.GlobalQualifiers.Varying;
				else if (attrib == AttribGLSL.output)
					qualifiers = ir.GlobalQualifiers.Out;
			}

			node.fields ~= ir.ShaderField(
				vd.loc,
				qualifiers,
				visitType(vd.type),
				mangleOf(vd),
			);
		}

		void handleChild(Dsymbol s) {
			if (AttribDeclaration ad = s.isAttribDeclaration) {
				if (ad.decl is null)
					return;

				foreach (child; *ad.decl)
					if (cast(void*) child !in handled)
						handleChild(child);
			}
			else if (VarDeclaration vd = s.isVarDeclaration) {
				// assert(cast(void*) vd in handled);
			}
			else if (FuncDeclaration fd = s.isFuncDeclaration) {
				node.methods ~= mangleOf(fd);
				fd.accept(this);
			}
			else {
				s.accept(this);
			}
		}

		cd.members.foreachDsymbol(&handleChild);
	}

	override void visit(TemplateDeclaration td) {
		foreach (instance; td.instances.byKey) {
			// instance.ti.members.foreachDsymbol(s => s.accept(this));
			// instance.ti.accept(this);
		}
	}

	override void visit(TemplateInstance ti) {
		// writestring("// template ");
		// writestringln(mangleOf(ti));

		ti.members.foreachDsymbol(s => s.accept(this));
	}

	override void visit(Statement s) {
		// stderr.writeln("unknown statement ", STMTtoString(s.stmt));

		ir.ErrorStmt node = new ir.ErrorStmt("unknown statement " ~ STMTtoString(s.stmt));
		node.loc = s.loc;
		*currentBody ~= node;
	}

	override void visit(Import s) {}
	override void visit(ImportStatement s) {}

	override void visit(DeclarationExp e) {
		if (VarDeclaration vd = e.declaration.isVarDeclaration) {
			visit(vd);
		}
		else {
			e.declaration.accept(this);
		}
	}

	override void visit(ExpStatement s) {
		if (!s.exp)
			return;
		else if (DeclarationExp d = s.exp.isDeclarationExp)
			d.declaration.accept(this);
		else
			s.exp.accept(this);
	}

	override void visit(CompoundStatement s) {
		foreach (child; *s.statements)
			if (child)
				child.accept(this);
	}

	override void visit(IfStatement s) {
		ir.IfBlock node = new ir.IfBlock();
		node.loc = s.loc;

		if (Parameter p = s.prm) {
			// FIXME: this
		}

		s.condition.accept(this);

		if (s.condition.type && s.condition.type.isTypeBasic) {
			TypeBasic b = s.condition.type.isTypeBasic;
			switch (b.ty) {
			case TY.Tint8:
			case TY.Tint16:
			case TY.Tint32:
			case TY.Tint64:
			case TY.Tuns8:
			case TY.Tuns16:
			case TY.Tuns32:
			case TY.Tuns64:
				ir.CastOp op = new ir.CastOp();
				op.loc = s.loc;
				op.name = new ir.Identifier();
				op.type = new ir.ScalarType(ir.POD.Bool);
				op.value = getVar(s.condition);
				*currentBody ~= op;
				node.condition = op.name;
				break;
			case TY.Tbool:
				node.condition = getVar(s.condition);
				break;
			default:
				goto ErrorCase;
			}
		}
		else {
		ErrorCase:
			ir.ErrorOp op = new ir.ErrorOp("unknown type used in `if` statement");
			op.loc = s.loc;
			op.name = new ir.Identifier();
			op.type = visitType(s.condition.type);
			*currentBody ~= op;
			node.condition = op.name;
		}

		*currentBody ~= node;

		auto save = currentBody;

		currentBody = &node.body;
		s.ifbody.accept(this);
		currentBody = save;

		if (s.elsebody) {
			currentBody = &node.elseBody;
			s.elsebody.accept(this);
			currentBody = save;
		}
	}

	override void visit(UnrolledLoopStatement s) {
		foreach (child; *s.statements)
			if (child)
				child.accept(this);
	}

	private {
		ir.Identifier[ir.Identifier] breakMap;
		ir.Identifier[ir.Identifier] continueMap;
		ir.Identifier[string] labelMap;
		ir.Identifier currentLoop;
		string lastLabel;
	}

	override void visit(LabelStatement s) {
		lastLabel = s.ident.toString.idup;

		if (s.statement)
			s.statement.accept(this);
	}

	override void visit(BreakStatement s) {
		if (currentLoop is null) {
			ir.ErrorStmt node = new ir.ErrorStmt("internal compiler error");
			node.loc = s.loc;
			*currentBody ~= node;
		}
		else {
			ir.BreakStmt node = new ir.BreakStmt();
			node.loc = s.loc;
			if (s.ident) {
				node.label = breakMap[labelMap[s.ident.toString.idup]];
			}
			else {
				node.label = breakMap[currentLoop];
			}
			*currentBody ~= node;
		}
	}

	override void visit(ContinueStatement s) {
		if (currentLoop is null) {
			ir.ErrorStmt node = new ir.ErrorStmt("internal compiler error");
			node.loc = s.loc;
			*currentBody ~= node;
		}
		else {
			ir.BreakStmt node = new ir.BreakStmt();
			node.loc = s.loc;
			if (s.ident) {
				node.label = continueMap[labelMap[s.ident.toString.idup]];
			}
			else {
				node.label = continueMap[currentLoop];
			}
			*currentBody ~= node;
		}
	}

	override void visit(ForStatement s) {
		if (s._init)
			visit(s._init);

		ir.LoopBlock node = new ir.LoopBlock();
		node.loc = s.loc;
		node.label = new ir.Identifier();
		*currentBody ~= node;

		breakMap[node.label] = node.label;

		ir.Identifier saveLoop = currentLoop;
		scope (exit)
			currentLoop = saveLoop;
		currentLoop = node.label;

		if (lastLabel) {
			labelMap[lastLabel] = node.label;
			lastLabel = null;
		}

		auto save = currentBody;

		currentBody = &node.body;

		if (s.condition) {
			s.condition.accept(this);

			ir.UnaOp negate = new ir.UnaOp();
			negate.loc = s.condition.loc;
			negate.name = new ir.Identifier();
			negate.type = new ir.ScalarType(ir.POD.Bool);
			negate.op = "!";
			negate.value = getVar(s.condition);
			*currentBody ~= negate;

			ir.IfBlock condHandler = new ir.IfBlock();
			condHandler.loc = s.condition.loc;
			condHandler.condition = negate.name;

			ir.BreakStmt breakStmt = new ir.BreakStmt();
			breakStmt.loc = s.condition.loc;
			breakStmt.label = node.label;
			condHandler.body ~= breakStmt;

			*currentBody ~= condHandler;
		}

		ir.LoopBlock continueWrapper = new ir.LoopBlock();
		continueWrapper.loc = s.loc;
		continueWrapper.label = new ir.Identifier();
		*currentBody ~= continueWrapper;

		currentBody = &continueWrapper.body;
		continueMap[node.label] = continueWrapper.label;
		s._body.accept(this);

		ir.BreakStmt breakNode = new ir.BreakStmt();
		breakNode.loc = s.loc;
		breakNode.label = continueWrapper.label;
		*currentBody ~= breakNode;

		currentBody = &node.body;

		if (s.increment)
			s.increment.accept(this);
		currentBody = save;
	}

	override void visit(TryCatchStatement s) {
		ir.ErrorStmt node = new ir.ErrorStmt("exceptions" ~ UNAVAILABLE_MSG);
		node.loc = s.loc;
		*currentBody ~= node;
	}

	override void visit(TryFinallyStatement s) {
		ir.FinalizerBlock node = new ir.FinalizerBlock();
		node.loc = s.loc;

		auto save = currentBody;

		currentBody = &node.body;
		s._body.accept(this);

		currentBody = &node.finalizer;
		s.finalbody.accept(this);

		currentBody = save;

		*currentBody ~= node;
	}

	override void visit(ReturnStatement s) {
		ir.ReturnStmt node = new ir.ReturnStmt();
		node.loc = s.loc;

		if (s.exp) {
			s.exp.accept(this);
			node.value = getVar(s.exp);
		}

		*currentBody ~= node;
	}

	override void visit(ThrowStatement s) {
		ir.ThrowStmt node = new ir.ThrowStmt();
		node.loc = s.loc;
		*currentBody ~= node;
	}

	override void visit(Type t) {
		assert(0);
	}

	override void visit(Expression e) {
		// stderr.writeln("unknown exp ", EXPtoString(e.op));

		ir.ErrorOp node = new ir.ErrorOp("unknown exp " ~ EXPtoString(e.op));
		expv(node, e, false);
		*currentBody ~= node;
	}

	override void visit(NotExp e) {
		e.e1.accept(this);

		ir.UnaOp node = new ir.UnaOp();
		expv(node, e, false);
		node.op = "!";
		node.value = getVar(e.e1);
		*currentBody ~= node;
	}

	override void visit(TypeExp e) {
		e.type.accept(this);
	}

	override void visit(NewExp e) {
		unavailable(e, "classes");
	}

	override void visit(StringExp e) {
		unavailable(e, "strings");
	}

	override void visit(BinExp e) {
		e.e1.accept(this);
		e.e2.accept(this);

		ir.BinOp node = new ir.BinOp();
		expv(node, e, false);
		// TODO: avoid EXPtoString
		node.op = EXPtoString(e.op);
		node.lhs = getVar(e.e1);
		node.rhs = getVar(e.e2);
		*currentBody ~= node;
	}

	override void visit(AssignExp e) {
		if (e.e1.type && e.e1.type.isTypeStruct && e.e2.isIntegerExp) {
			assert(e.e2.isIntegerExp.toInteger == 0);

			e.e1.accept(this);

			ir.InitOp op = new ir.InitOp();
			op.loc = e.e1.loc;
			op.name = new ir.Identifier();
			op.isRef = false;
			op.type = visitType(e.e1.type);
			*currentBody ~= op;

			ir.AssignStmt node = new ir.AssignStmt();
			node.loc = e.loc;
			node.lhs = getVar(e.e1);
			node.rhs = op.name;
			*currentBody ~= node;

			rename(e, e.e1);
		}
		else {
			e.e1.accept(this);
			e.e2.accept(this);

			ir.AssignStmt node = new ir.AssignStmt();
			node.loc = e.loc;
			node.lhs = getVar(e.e1);
			node.rhs = getVar(e.e2);
			*currentBody ~= node;

			rename(e, e.e1);
		}
	}

	override void visit(BinAssignExp e) {
		e.e1.accept(this);
		e.e2.accept(this);

		ir.BinOp binNode = new ir.BinOp();
		expv(binNode, e, false);
		// TODO: avoid EXPtoString
		binNode.op = EXPtoString(e.op);
		binNode.lhs = getVar(e.e1);
		binNode.rhs = getVar(e.e2);
		*currentBody ~= binNode;

		ir.AssignStmt assignNode = new ir.AssignStmt();
		assignNode.loc = e.loc;
		assignNode.lhs = getVar(e.e1);
		assignNode.rhs = binNode.name;
		*currentBody ~= assignNode;

		rename(e, e.e1);
	}

	override void visit(CommaExp e) {
		e.e1.accept(this);
		e.e2.accept(this);
		rename(e, e.e2);
	}

	override void visit(IndexExp e) {
		e.e1.accept(this);
		e.e2.accept(this);

		ir.IndexOp node = new ir.IndexOp();
		expv(node, e, true);
		node.base = getVar(e.e1);
		node.index = getVar(e.e2);
		*currentBody ~= node;
	}

	override void visit(CondExp e) {
		e.econd.accept(this);

		ir.LoadOp node = new ir.LoadOp();
		expv(node, e, false);
		*currentBody ~= node;

		ir.IfBlock block = new ir.IfBlock();
		block.loc = e.loc;
		block.condition = getVar(e.econd);
		*currentBody ~= block;

		auto save = currentBody;

		currentBody = &block.body;
		e.e1.accept(this);
		ir.AssignStmt assignSuccess = new ir.AssignStmt();
		assignSuccess.loc = e.loc;
		assignSuccess.lhs = node.name;
		assignSuccess.rhs = getVar(e.e1);
		*currentBody ~= assignSuccess;

		currentBody = &block.elseBody;
		e.e2.accept(this);
		ir.AssignStmt assignFail = new ir.AssignStmt();
		assignFail.loc = e.loc;
		assignFail.lhs = node.name;
		assignFail.rhs = getVar(e.e2);
		*currentBody ~= assignFail;

		currentBody = save;
	}

	override void visit(PtrExp e) {
		// TODO: maybe support *(a ? &b : c ? &d : &e)
		// which gets autogenerated by a ? b : c ? d : e in ref context

		if (FuncExp fe = e.e1.isFuncExp) {
			fe.fd.accept(this);
			rename(e, mangleOf(fe.fd));
		}
		else {
			unavailable(e, "pointers");
		}
	}

	override void visit(FuncExp e) {
		unavailable(e, "function pointers");
	}

	override void visit(PreExp e) {
		e.e1.accept(this);

		ir.UnaOp node = new ir.UnaOp();
		expv(node, e, false);
		// TODO: avoid EXPtoString
		node.op = EXPtoString(e.op);
		node.value = getVar(e.e1);
		*currentBody ~= node;
	}

	override void visit(PostExp e) {
		e.e1.accept(this);

		switch (e.op) {
		case EXP.plusPlus:
		case EXP.minusMinus:
			ir.Op op;

			ir.Type t = visitType(deEnum(e.e1.type));
			if (ir.ScalarType st = cast(ir.ScalarType) t) {
				ir.BinOp node = new ir.BinOp();
				expv(node, e, false);
				node.op = "+";
				node.lhs = getVar(e.e1);
				node.rhs = numericLiteral(st.pod, 1);

				op = node;
			}
			else {
				ir.ErrorOp node = new ir.ErrorOp(null);
				expv(node, e, false);

				assert(cast(ir.ErrorType) node.type);

				op = node;
			}

			*currentBody ~= op;

			ir.AssignStmt assign = new ir.AssignStmt();
			assign.loc = e.loc;
			assign.lhs = getVar(e.e1);
			assign.rhs = op.name;
			*currentBody ~= assign;

			break;
		default:
			ir.UnaOp node = new ir.UnaOp();
			expv(node, e, false);
			// TODO: avoid EXPtoString
			node.op = EXPtoString(e.op);
			node.value = getVar(e.e1);
			*currentBody ~= node;
			break;
		}
	}

	override void visit(SliceExp e) {
		unavailable(e, "slices");
	}

	override void visit(ArrayLengthExp e) {
		unavailable(e, "slices");
	}

	override void visit(AssertExp e) {
		// TODO: how does this work as an expression?

		ir.AssertStmt node = new ir.AssertStmt();
		node.loc = e.loc;

		e.e1.accept(this);

		node.condition = getVar(e.e1);

		*currentBody ~= node;
	}

	override void visit(DelegateExp e) {
		unavailable(e, "delegates");
	}

	override void visit(CastExp e) {
		e.e1.accept(this);

		ir.CastOp node = new ir.CastOp();
		expv(node, e, false);
		node.value = getVar(e.e1);
		*currentBody ~= node;
	}

	private static Type deEnum(Type type) {
		while (type) {
			if (TypeEnum e = type.isTypeEnum) {
				type = e.memType;
			}
			else {
				break;
			}
		}

		return type;
	}

	override void visit(IntegerExp e) {
		import std.conv : to;

		ir.NumericLiteralOp node = new ir.NumericLiteralOp();
		expv(node, e, false);

		if (e.type) {
			if (TypeBasic b = deEnum(e.type).isTypeBasic) {
				if (b.ty == TY.Tuns8 || b.ty == TY.Tuns16 || b.ty == TY.Tuns32 || b.ty == TY.Tuns64) {
					node.value = (cast(ulong) e.toInteger()).to!string ~ "u";
				}
				else if (b.ty == TY.Tbool) {
					node.value = e.toInteger() ? "true" : "false";
				}
			}
		}

		if (node.value is null)
			node.value = (cast(long) e.toInteger()).to!string;

		*currentBody ~= node;
	}

	override void visit(RealExp e) {
		ir.NumericLiteralOp node = new ir.NumericLiteralOp();
		expv(node, e, false);
		*currentBody ~= node;

		if (TypeBasic b = deEnum(e.type).isTypeBasic) {
			if (b.ty == TY.Tfloat32) {
				float val = cast(float) e.toReal();

				// nans show up in auto-initializations
				// but nans aren't legal in all versions of GLSL, so we just
				// blindly replace them with 0
				if (val != val)
					val = 0;

				foreach (int i; 0 .. 100) {
					OutBuffer formatBuf;
					formatBuf.printf("%%.%df", i);
					formatBuf.writeByte(0);

					OutBuffer buf;
					buf.printf(formatBuf[].ptr, val);

					import std.conv : to;

					float reproduced = buf[].to!float;

					if (val == reproduced || i == 99) {
						node.value = buf[].idup ~ (i == 0 ? "." : "");
						return;
					}
				}
			}
			else if (b.ty == TY.Tfloat64 || b.ty == TY.Tfloat80) {
				double val = cast(double) e.toReal();
				if (val != val)
					val = 0;
				foreach (int i; 0 .. 100) {
					OutBuffer formatBuf;
					formatBuf.printf("%%.%df", i);
					formatBuf.writeByte(0);

					OutBuffer buf;
					buf.printf(formatBuf[].ptr, val);

					import std.conv : to;

					double reproduced = buf[].to!double;

					if (val == reproduced || i == 99) {
						node.value = buf[].idup ~ "lf";
						return;
					}
				}
			}
		}
		else {
			assert(0, "real exp should have basic type");
		}
	}

	override void visit(VarExp e) {
		if (SymbolDeclaration sd = e.var.isSymbolDeclaration) {
			ir.InitOp node = new ir.InitOp();
			expv(node, e, false);
			*currentBody ~= node;
		}
		else {
			rename(e, mangleOf(e.var));
		}
	}

	override void visit(IdentifierExp e) {
		ir.ErrorOp node = new ir.ErrorOp("unresolved identifier");
		expv(node, e, false);
		*currentBody ~= node;
	}

	override void visit(TemplateParameter) { assert(0); }
	override void visit(Condition) { assert(0); }

	override void visit(ThisExp e) {
		VarDeclaration vt = vthis;
		if (vt is null) {
			// stderr.writeln(e.loc);
			// error(e.loc, "shit's wack bro");
			// assert(0);
			ir.ErrorOp node = new ir.ErrorOp("unresolved `this`");
			expv(node, e, false);
			*currentBody ~= node;
		}
		else if (isClassThis(vt)) {
			unavailable(e, "classes");
		}
		else {
			rename(e, mangleOf(vt));
		}
	}

	override void visit(SuperExp e) {
		unavailable(e, "classes");
	}

	override void visit(NullExp e) {
		if (e.type && e.type.isTypeDArray) {
			unavailable(e, "slices");
		}
		else if (e.type && e.type.isTypeClass) {
			unavailable(e, "classes");
		}
		else {
			unavailable(e, "pointers");
		}
	}

	override void visit(DotIdExp e) {
		ir.ErrorOp node = new ir.ErrorOp("unresolved identifier");
		expv(node, e, false);
		*currentBody ~= node;
	}

	override void visit(DotVarExp e) {
		NativeType t = nativeTypeOf(e.e1);
		final switch (t.base) {
		case NativeTypeBase.gvec2:
		case NativeTypeBase.gvec3:
		case NativeTypeBase.gvec4:
			e.e1.accept(this);
			rename(e, getVar(e.e1));
			assert(e.var.ident.toString == "components");
			break;
		case NativeTypeBase.gsampler1D:
		case NativeTypeBase.gsampler2D:
		case NativeTypeBase.gsampler3D:
			ir.ErrorOp node = new ir.ErrorOp("internal compiler error");
			expv(node, e, false);
			*currentBody ~= node;
			break;
		case NativeTypeBase.none:
			if (ThisExp te = e.e1.isThisExp) {
				if (isClassThis(te.var)) {
					ir.LoadOp node = new ir.LoadOp();
					expv(node, e, true);
					node.var = mangleOf(e.var);
					*currentBody ~= node;
					break;
				}
			}

			e.e1.accept(this);

			ir.DotOp node = new ir.DotOp();
			expv(node, e, true);
			node.base = getVar(e.e1);
			node.index = mangleOf(e.var);
			*currentBody ~= node;

			break;
		}
	}

	override void visit(ArrayLiteralExp e) {
		ir.ArrayOp node = new ir.ArrayOp();
		expv(node, e, false);
		node.members = writeArgs(e.elements, e.basis);
		*currentBody ~= node;
	}

	private ir.Identifier numericLiteral(ir.POD pod, int v) {
		import std.conv : to;

		ir.NumericLiteralOp op = new ir.NumericLiteralOp();
		op.name = new ir.Identifier();
		op.type = new ir.ScalarType(pod);
		*currentBody ~= op;

		final switch (pod) {
		case ir.POD.Void:
			assert(0);
		case ir.POD.Bool:
			op.value = v ? "true" : "false";
			break;
		case ir.POD.Int:
			op.value = v.to!string;
			break;
		case ir.POD.UInt:
			op.value = v.to!string ~ "u";
			break;
		case ir.POD.Float:
			op.value = v.to!string ~ ".";
			break;
		case ir.POD.Double:
			op.value = v.to!string ~ "lf";
			break;
		}

		return op.name;
	}

	override void visit(StructLiteralExp e) {
		// TODO: handle CTFE

		NativeType native = nativeTypeOf(e.sd);

		final switch (native.base) {
		case NativeTypeBase.gvec2:
		case NativeTypeBase.gvec3:
		case NativeTypeBase.gvec4:
			ArrayLiteralExp arr = (*e.elements)[0].isArrayLiteralExp;
			assert(arr);

			ir.CallOp node = new ir.CallOp();
			expv(node, e, false);
			node.func = mangleOf(e.sd);
			node.args = writeArgs(arr.elements, arr.basis);
			*currentBody ~= node;
			return;
		case NativeTypeBase.gsampler1D:
		case NativeTypeBase.gsampler2D:
		case NativeTypeBase.gsampler3D:
			ir.ErrorOp node = new ir.ErrorOp("internal compiler error");
			expv(node, e, false);
			*currentBody ~= node;
			return;
		case NativeTypeBase.none:
			break;
		}

		// CTFE can generate struct literals that contain an AddrExp pointing
		// to themselves, need to avoid infinite recursion:
		// struct S { this(int){ this.s = &this; } S* s; }
		// const foo = new S(0);
		Expression[] exps;
		foreach (i; 0 .. e.sd.fields.length) {
			VarDeclaration field = e.sd.fields[i];
			Expression exp = i >= e.elements.length ? null : (*e.elements)[i];
			exps ~= exp;
			if (exp is null) {
				// TODO: why does this happen
			}
			else {
				exp.accept(this);
			}
		}

		ir.CallOp node = new ir.CallOp();
		expv(node, e, false);
		node.func = mangleOf(e.sd);
		foreach (i; 0 .. e.sd.fields.length) {
			node.args ~= getVar(exps[i]);
		}
		*currentBody ~= node;
	}

	override void visit(ClassReferenceExp e) {
		unavailable(e, "classes");
	}

	private static extern (D) string fqnOf(Dsymbol v) {
		string[] parts;
		while (v !is null) {
			if (v.ident)
				parts = v.ident.toString.idup ~ parts;
			v = v.parent;
		}

		string fqn;
		foreach (i, part; parts) {
			if (i > 0)
				fqn ~= ".";
			fqn ~= part;
		}

		return fqn;
	}

	override void visit(CallExp e) {
		// TODO: implement this
		if (e.e1.op == EXP.type) {
			ir.ErrorOp node = new ir.ErrorOp("unimplemented feature (type call); open an issue");
			expv(node, e, false);
			*currentBody ~= node;
			return;
		}

		ir.Identifier builtin;

		void checkBuiltin(Declaration vd) {
			string fqn = fqnOf(vd);
			foreach (string[2] coreFn; [
				["core.math.cos", "cos"],
				["core.math.sin", "sin"],
				["core.math.rndtol", "rndtol"],
				["core.math.sqrt", "sqrt"],
				["core.math.ldexp", "ldexp"],
				["core.math.rint", "rint"],
				["core.math.yl2x", "yl2x"],
				["core.math.yl2xp1", "yl2xp1"],
				["gd.math.vec.TVec2.magnitude", "length"],
				["gd.math.vec.TVec3.magnitude", "length"],
				["gd.math.vec.TVec4.magnitude", "length"],
				["gd.math.vec.TVec2.normalize", "normalize"],
				["gd.math.vec.TVec3.normalize", "normalize"],
				["gd.math.vec.TVec4.normalize", "normalize"],
				["gd.shaders.TSampler1D.get", "texture"],
				["gd.shaders.TSampler2D.get", "texture"],
				["gd.shaders.TSampler3D.get", "texture"],
			]) {
				if (fqn == coreFn[0]) {
					builtin = new ir.Identifier(coreFn[1], true);
					break;
				}
			}
		}

		if (DotVarExp d = e.e1.isDotVarExp) {
			if (FuncDeclaration f = d.var.isFuncDeclaration) {
				if (f.needThis) {
					ir.Identifier[] args;

					if (!isClassThis(f.vthis)) {
						d.e1.accept(this);
						args ~= getVar(d.e1);
					}

					args ~= writeArgs(e.arguments);

					bool isRef = false;
					if (f.type)
						if (TypeFunction tf = f.type.isTypeFunction)
							if (tf.isref)
								isRef = true;

					checkBuiltin(d.var);

					ir.CallOp node = new ir.CallOp();
					expv(node, e, isRef);
					node.func = builtin ? builtin : mangleOf(d.var);
					node.args = args;
					*currentBody ~= node;

					return;
				}
				else {
					// TODO: this
				}
			}
		}

		bool isRef = false;
		if (e.e1.type)
			if (TypeFunction tf = e.e1.type.isTypeFunction)
				if (tf.isref)
					isRef = true;

		if (VarExp ve = e.e1.isVarExp) {
			checkBuiltin(ve.var);
		}

		if (!builtin)
			e.e1.accept(this);

		ir.CallOp node = new ir.CallOp();
		expv(node, e, isRef);
		node.func = builtin ? builtin : getVar(e.e1);
		node.args = writeArgs(e.arguments);
		*currentBody ~= node;
	}

	override void visit(UnitTestDeclaration f) {}

	private static bool isClassThis(VarDeclaration vthis) {
		return vthis && vthis.type.isTypeClass !is null;
	}

	override void visit(FuncDeclaration f) {
		enterSymbol(f);
		scope (exit)
			exitSymbol();

		ir.Function node = new ir.Function();
		node.loc = f.loc;
		node.originalSymbol = f;

		auto type = cast(TypeFunction) f.type;
		node.name = mangleOf(f);
		node.isRef = type ? type.isref : false;
		node.returnType = type ? visitType(type.next) : visitType(null);
		currSymbol.source = node;

		VarDeclaration save = vthis;
		scope (exit)
			vthis = save;

		VarDeclaration[] params;

		if (f.needThis && f.vthis !is null) {
			if (!isClassThis(f.vthis))
				params ~= f.vthis;

			vthis = f.vthis;
		}

		if (f.parameters != null)
			params ~= (*f.parameters)[];

		foreach (i, v; params) {
			ir.Param param;
			param.isRef = (v.storage_class & (STC.ref_ | STC.in_ | STC.out_)) != 0;
			param.type = visitType(v.type);
			param.name = mangleOf(v);
			node.params ~= param;
		}

		foreach (v; f.outerVars) {
			ir.Param param;
			param.isRef = true;
			param.type = visitType(v.type);
			param.name = mangleOf(v);
			node.outerVars ~= param;
		}

		auto saveBody = currentBody;
		currentBody = &node.body;

		if (f.fbody)
			f.fbody.accept(this);

		currentBody = saveBody;
	}

	ir.Type visitType(Type type) {
		type = deEnum(type);

		if (!type) {
			return new ir.ErrorType("unfinished semantic analysis");
		}
		else if (TypeSArray t = type.isTypeSArray) {
			ir.Type element = visitType(t.next);
			ir.ArrayType node = new ir.ArrayType();

			int dim = 0;
			if (IntegerExp ie = t.dim.isIntegerExp) {
				dim = cast(int) ie.toInteger;
			}
			else {
				// TODO: figure this out
			}

			if (ir.ArrayType elementArray = cast(ir.ArrayType) element) {
				node.element = elementArray.element;
				node.dims = dim ~ elementArray.dims;
			}
			else {
				node.element = element;
				node.dims = [dim];
			}

			return node;
		}
		else if (TypeBasic t = type.isTypeBasic) {
			auto pod = TYtoPOD(t.ty);
			return pod.isNull
				? new ir.ErrorType("type `" ~ t.toString.idup ~ "` is not available in shader code")
				: new ir.ScalarType(pod.get);
		}
		else if (TypeStruct t = type.isTypeStruct) {
			NativeType native = nativeTypeOf(t.sym);

			final switch (native.base) {
			case NativeTypeBase.gvec2: return new ir.VectorType(native.pod, 2);
			case NativeTypeBase.gvec3: return new ir.VectorType(native.pod, 3);
			case NativeTypeBase.gvec4: return new ir.VectorType(native.pod, 4);
			case NativeTypeBase.gsampler1D: return new ir.OpaqueType(ir.getPODPrefix(native.pod) ~ "sampler1D");
			case NativeTypeBase.gsampler2D: return new ir.OpaqueType(ir.getPODPrefix(native.pod) ~ "sampler2D");
			case NativeTypeBase.gsampler3D: return new ir.OpaqueType(ir.getPODPrefix(native.pod) ~ "sampler3D");
			case NativeTypeBase.none:
				ir.StructType node = new ir.StructType();
				node.declaration = getIRStruct(t.sym);
				return node;
			}
		}
		else if (TypeClass t = type.isTypeClass) {
			return new ir.ErrorType("classes" ~ UNAVAILABLE_MSG);
		}
		else if (TypeNoreturn t = type.isTypeNoreturn) {
			return new ir.ScalarType(ir.POD.Void);
		}
		else if (TypeDArray t = type.isTypeDArray) {
			return new ir.ErrorType("slices" ~ UNAVAILABLE_MSG ~ "; use static arrays");
		}
		else if (TypePointer t = type.isTypePointer) {
			return new ir.ErrorType("pointers" ~ UNAVAILABLE_MSG);
		}
		else {
			return new ir.ErrorType("unknown type encountered");
		}
	}

	override void visit(VarDeclaration v) {
		// enums aren't visible in the compiled source
		if (v.storage_class & STC.manifest)
			return;

		if (currentBody is null || (v.storage_class & STC.static_) || (v.storage_class & STC.gshared)) {
			// assert(!v.parent || (!v.parent.isFuncDeclaration && !v.parent.isAggregateDeclaration));

			enterSymbol(v);
			scope (exit)
				exitSymbol();

			ir.Global node = new ir.Global();
			node.loc = v.loc;
			node.originalSymbol = v;

			assert((v.storage_class & STC.ref_) == 0);

			node.name = mangleOf(v);
			node.type = visitType(v.type);

			currSymbol.source = node;
		}
		else {
			void writeOut(Expression value, bool defaultInit) {
				ir.LoadOp node = new ir.LoadOp();
				node.loc = v.loc;
				node.name = mangleOf(v);
				node.isRef = (v.storage_class & STC.ref_) != 0;
				node.type = visitType(v.type);

				if ((value && v.type && v.type.isTypeStruct && value.isIntegerExp) || defaultInit) {
					assert(!value || value.isIntegerExp.toInteger == 0);

					ir.InitOp op = new ir.InitOp();
					op.loc = v.loc;
					op.name = new ir.Identifier();
					op.isRef = false;
					op.type = visitType(v.type);
					*currentBody ~= op;

					node.var = op.name;
					*currentBody ~= node;
				}
				else if (value) {
					*currentBody ~= node;

					value.accept(this);

					ir.AssignStmt assign = new ir.AssignStmt();
					assign.loc = v.loc;
					assign.isRef = node.isRef;
					assign.lhs = node.name;
					assign.rhs = getVar(value);
					*currentBody ~= assign;
				}
				else {
					*currentBody ~= node;
				}
			}

			Initializer i = v._init;
			if (!i) {
				writeOut(null, true);
			}
			else if (i.isVoidInitializer) {
				writeOut(null, false);
			}
			else if (ExpInitializer ie = i.isExpInitializer) {
				if (ie.exp.op == EXP.construct || ie.exp.op == EXP.blit) {
					Expression e2 = (cast(AssignExp) ie.exp).e2;
					writeOut(e2, false);
				}
				else {
					// TODO: when does this happen?
					writeOut(ie.exp, false);
				}
			}
			else if (StructInitializer si = i.isStructInitializer) {
				// TODO: when does this happen?
				writeOut(null, true);
				// writestringln("struct init");
			}
			else if (ArrayInitializer ai = i.isArrayInitializer) {
				// TODO: when does this happen?
				writeOut(null, true);
				// writestringln("array init");
			}
			else {
				assert(0);
			}
		}
	}

	override void visit(MixinDeclaration m) {
		if (!m.compiled) {
			// TODO: why does this happen
		}
		else {
			foreach (s; *m.decl)
				s.accept(this);
		}
	}

	override void visit(StaticForeachDeclaration m) {
		assert(m.cached);
		if (m.cache)
			foreach (s; *m.cache)
				s.accept(this);
	}

	override void visit(ConditionalDeclaration m) {
		final switch (m.condition.inc) {
		case Include.notComputed:
			// TODO: why does this happen
			break;
		case Include.yes:
			foreach (s; *m.decl)
				s.accept(this);
			break;
		case Include.no:
			if (m.elsedecl)
				foreach (s; *m.elsedecl)
					s.accept(this);
			break;
		}
	}

	override void visit(ScopeStatement s) {
		if (s.statement)
			s.statement.accept(this);
	}

	override void visit(ScopeExp e) {
		// if (e.sds.isTemplateInstance()) {
			// e.sds.dsymbolToBuffer(buf, hgs);
			// writestring(mangleOf(e.sds));
		// }
		// else {
		// 	// fixes bug 6491
		// 	if (auto m = e.sds.isModule())
		// 		writestring(m.md.toChars());
		// 	else
		// 		writestring(e.sds.toChars());
		// }
		// else {
		// 	writestring(e.sds.kind());
		// 	writeByte(' ');
		// 	writestring(e.sds.toChars());
		// }
	}

}
