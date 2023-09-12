module dmd.shader.glsl.compiler;
static import ir = dmd.shader.glsl.ir;
public import dmd.shader.glsl.ir : Identifier, POD, GlobalQualifiers, ShaderType;
import dmd.arsd.mvd;
import dmd.errors;
import dmd.location;
import std.conv;
import std.algorithm : all;

abstract class ASTBase {
	Loc loc;
}

abstract class Type : ASTBase {}

class ScalarType : Type {
	POD pod;

	this(Loc loc, POD pod) {
		this.loc = loc;
		this.pod = pod;
	}
}

class VectorType : Type {
	POD pod;
	int dim;

	this(Loc loc, POD pod, int dim) {
		this.loc = loc;
		this.pod = pod;
		this.dim = dim;
	}
}

class OpaqueType : Type {
	string name;

	this(Loc loc, string name) {
		this.loc = loc;
		this.name = name;
	}
}

class ArrayType : Type {
	Type element;
	immutable(int)[] dims;

	this(Loc loc, Type element, immutable(int)[] dims) {
		this.loc = loc;
		this.element = element;
		this.dims = dims;
	}
}

class StructType : Type {
	Identifier name;
	Struct declaration;

	this(Loc loc, Identifier name, Struct declaration) {
		this.loc = loc;
		this.name = name;
		this.declaration = declaration;
	}
}

abstract class Declaration : ASTBase {
	Identifier name;
}

struct Field {
	Type type;
	Identifier name;
}

class Struct : Declaration {
	Field[] fields;
}

struct Param {
	bool isInout;
	Type type;
	Identifier name;
}

class Function : Declaration {
	Type returnType;
	Field[] locals;
	Param[] params;
	Stmt[] body;
}

class Global : Declaration {
	GlobalQualifiers qualifiers;
	Type type;
}

abstract class Stmt : ASTBase {}

abstract class RefPathSegment {}

class Dot : RefPathSegment {
	Identifier id;

	this(Identifier id) {
		this.id = id;
	}
}

class Index : RefPathSegment {
	Exp exp;

	this(Exp exp) {
		this.exp = exp;
	}
}

class AssignStmt : Stmt {
	Identifier base;
	RefPathSegment[] path;
	Exp rhs;
}

class ExpStmt : Stmt {
	Exp value;
}

class ReturnStmt : Stmt {
	/** nullable */
	Exp value;
}

class DiscardStmt : Stmt {}

class BreakStmt : Stmt {}

abstract class Block : Stmt {
	Stmt[] body;
}

class IfBlock : Block {
	Exp condition;
	Stmt[] elseBody;
}

class WhileBlock : Block {
	Exp condition;
}

abstract class Exp : ASTBase {

	Type type;

	// TODO: figure out a better name for this method
	// basically checks if this expression doesn't depend on any variables
	abstract bool isPODConst();

}

/** also used for bools */
class NumericLiteral : Exp {

	string value;

	this(Loc loc, Type type, string value) {
		this.loc = loc;
		this.type = type;
		this.value = value;
	}

	override bool isPODConst() { return true; }

}

class VarExp : Exp {

	Identifier id;

	this(Loc loc, Identifier id) {
		this.loc = loc;
		this.id = id;
	}

	override bool isPODConst() { return false; }

}

class BinExp : Exp {

	string op;
	Exp lhs, rhs;

	this(Loc loc, Type type, string op, Exp lhs, Exp rhs) {
		this.loc = loc;
		this.type = type;
		this.op = op;
		this.lhs = lhs;
		this.rhs = rhs;
	}

	override bool isPODConst() { return lhs.isPODConst && rhs.isPODConst; }

}

class UnaExp : Exp {

	string op;
	Exp value;

	this(Loc loc, Type type, string op, Exp value) {
		this.loc = loc;
		this.type = type;
		this.op = op;
		this.value = value;
	}

	override bool isPODConst() { return value.isPODConst; }

}

class CastExp : Exp {

	Exp value;

	this(Loc loc, Type type, Exp value) {
		this.loc = loc;
		this.type = type;
		this.value = value;
	}

	override bool isPODConst() { return value.isPODConst; }

}

class IndexExp : Exp {

	Exp base, index;

	this(Loc loc, Type type, Exp base, Exp index) {
		this.loc = loc;
		this.type = type;
		this.base = base;
		this.index = index;
	}

	override bool isPODConst() { return base.isPODConst && index.isPODConst; }

}

class DotExp : Exp {

	Exp base;
	Identifier index;

	this(Loc loc, Type type, Exp base, Identifier index) {
		this.loc = loc;
		this.type = type;
		this.base = base;
		this.index = index;
	}

	override bool isPODConst() { return base.isPODConst; }

}

class ArrayExp : Exp {

	Exp[] members;

	this(Loc loc, Type type, Exp[] members) {
		this.loc = loc;
		this.type = type;
		this.members = members;
	}

	override bool isPODConst() { return members.all!(s => s.isPODConst); }

}

class CallExp : Exp {

	Identifier func;
	Exp[] args;

	this(Loc loc, Type type, Identifier func, Exp[] args) {
		this.loc = loc;
		this.type = type;
		this.func = func;
		this.args = args;
	}

	override bool isPODConst() { return false; }

}

class Shader : ASTBase {
	ShaderType shaderType;
	Declaration[] declarations;
}

Shader compileShader(ir.Shader shader) {
	return cast(Shader) shader.accept(new Compiler());
}

Exp createInitializer(Type type) { return mvd!createInitializerImpl(type); }

NumericLiteral createInitializerImpl(ScalarType node) {
	final switch (node.pod) {
	case POD.Void:
		assert(0);
	case POD.Bool:
		return new NumericLiteral(node.loc, node, "false");
	case POD.Int:
		return new NumericLiteral(node.loc, node, "0");
	case POD.UInt:
		return new NumericLiteral(node.loc, node, "0u");
	case POD.Float:
		return new NumericLiteral(node.loc, node, "0.");
	case POD.Double:
		return new NumericLiteral(node.loc, node, "0lf");
	}
}

CallExp createInitializerImpl(VectorType node) {
	import std.conv : text;

	return new CallExp(node.loc, node,
		new Identifier(text(ir.getPODPrefix(node.pod), "vec", node.dim), true),
		[createInitializer(new ScalarType(node.loc, node.pod))]
	);
}

Exp createInitializerImpl(OpaqueType node) {
	assert(0);
}

ArrayExp createInitializerImpl(ArrayType node) {
	Type childType =
		node.dims.length == 1 ? node.element
		: new ArrayType(node.loc, node.element, node.dims[1 .. $]);

	Exp[] members;

	foreach (i; 0 .. node.dims.length) {
		members ~= createInitializer(childType);
	}

	return new ArrayExp(node.loc, node, members);
}

CallExp createInitializerImpl(StructType node) {
	// TODO: proper initialization, instead of zero initialization
	Exp[] args;
	foreach (field; node.declaration.fields) {
		args ~= createInitializer(field.type);
	}
	return new CallExp(node.loc, node, node.name, args);
}

private class Compiler : ir.Visitor!ASTBase {
	alias visit = ir.Visitor!ASTBase.visit;

	private Function currentFunction;

	private Type typec(ir.Type type) {
		return cast(Type) type.accept(this);
	}

	override ScalarType visit(ir.ScalarType node) {
		return new ScalarType(node.loc, node.pod);
	}

	override VectorType visit(ir.VectorType node) {
		return new VectorType(node.loc, node.pod, node.dim);
	}

	override OpaqueType visit(ir.OpaqueType node) {
		return new OpaqueType(node.loc, node.name);
	}

	override ArrayType visit(ir.ArrayType node) {
		return new ArrayType(node.loc, typec(node.element), node.dims);
	}

	Struct[ir.Struct] structMap;

	private Struct getStruct(ir.Struct s) {
		if (s !in structMap)
			structMap[s] = new Struct();
		return structMap[s];
	}

	override StructType visit(ir.StructType node) {
		return new StructType(node.loc, node.declaration.name, getStruct(node.declaration));
	}

	override Type visit(ir.ErrorType node) {
		assert(0);
	}

	override Struct visit(ir.Struct node) {
		Struct res = getStruct(node);
		res.loc = node.loc;
		res.name = node.name;

		foreach (field; node.fields)
			res.fields ~= Field(typec(field.type), field.name);

		return res;
	}

	override ASTBase visit(ir.ShaderClass node) {
		assert(0);
	}

	void visitBlock(ref Stmt[] body, ir.Stmt[] source) {
		foreach (s; source) {
			ASTBase transformed = s.accept(this);
			if (transformed !is null)
				body ~= cast(Stmt) transformed;
		}
	}

	override Function visit(ir.Function node) {
		Function res = new Function();
		res.loc = node.loc;
		res.name = node.name;
		res.returnType = typec(node.returnType);
		assert(!node.isRef);

		foreach (param; node.outerVars ~ node.params)
			res.params ~= Param(
				param.isRef,
				typec(param.type),
				param.name,
			);

		currentFunction = res;

		visitBlock(res.body, node.body);

		return res;
	}

	override Global visit(ir.Global node) {
		Global res = new Global();
		res.loc = node.loc;
		res.name = node.name;
		res.qualifiers = node.qualifiers;
		res.type = typec(node.type);

		return res;
	}

	override AssignStmt visit(ir.AssignStmt node) {
		assert(!node.isRef);

		AssignStmt res = new AssignStmt();
		res.loc = node.loc;
		res.base = node.lhs;

		// TODO: resolve types at a later stage
		res.rhs = new VarExp(node.loc, node.rhs);

		return res;
	}

	override AssignStmt visit(ir.ResolvedAssignStmt node) {
		AssignStmt res = new AssignStmt();
		res.loc = node.loc;
		res.base = node.base;

		foreach (seg; node.path) {
			final switch (seg.type) {
			case ir.RefPathSegmentType.dot:
				res.path ~= new Dot(seg.id);
				break;
			case ir.RefPathSegmentType.index:
				res.path ~= new Index(new VarExp(node.loc, seg.id));
				break;
			}
		}

		res.rhs = new VarExp(node.loc, node.rhs);

		return res;
	}

	override ReturnStmt visit(ir.ReturnStmt node) {
		ReturnStmt res = new ReturnStmt();
		res.loc = node.loc;
		res.value = new VarExp(node.loc, node.value);
		return res;
	}

	override DiscardStmt visit(ir.DiscardStmt node) {
		DiscardStmt res = new DiscardStmt();
		res.loc = node.loc;
		return res;
	}

	override ASTBase visit(ir.ThrowStmt node) {
		assert(0);
	}

	override BreakStmt visit(ir.BreakStmt node) {
		BreakStmt res = new BreakStmt();
		res.loc = node.loc;
		return res;
	}

	override ASTBase visit(ir.AssertStmt node) {
		assert(0);
	}

	override ASTBase visit(ir.ErrorStmt node) {
		assert(0);
	}

	override IfBlock visit(ir.IfBlock node) {
		IfBlock res = new IfBlock();
		res.loc = node.loc;
		res.condition = new VarExp(node.loc, node.condition);

		visitBlock(res.body, node.body);
		visitBlock(res.elseBody, node.elseBody);

		return res;
	}

	override WhileBlock visit(ir.LoopBlock node) {
		WhileBlock res = new WhileBlock();
		res.loc = node.loc;
		res.condition = new NumericLiteral(node.loc, new ScalarType(node.loc, POD.Bool), "true");

		visitBlock(res.body, node.body);

		return res;
	}

	override ASTBase visit(ir.FinalizerBlock node) {
		assert(0);
	}

	override AssignStmt visit(ir.Op node) {
		currentFunction.locals ~= Field(typec(node.type), node.name);

		AssignStmt res = new AssignStmt();
		res.loc = node.loc;
		res.base = node.name;
		return res;
	}

	override AssignStmt visit(ir.NumericLiteralOp node) {
		AssignStmt res = visit(cast(ir.Op) node);
		res.rhs = new NumericLiteral(node.loc, typec(node.type), node.value);
		return res;
	}

	override AssignStmt visit(ir.BinOp node) {
		AssignStmt res = visit(cast(ir.Op) node);
		res.rhs = new BinExp(node.loc, typec(node.type), node.op,
			new VarExp(node.loc, node.lhs),
			new VarExp(node.loc, node.rhs),
		);
		return res;
	}

	override AssignStmt visit(ir.UnaOp node) {
		AssignStmt res = visit(cast(ir.Op) node);
		res.rhs = new UnaExp(node.loc, typec(node.type), node.op, new VarExp(node.loc, node.value));
		return res;
	}

	override AssignStmt visit(ir.LoadOp node) {
		AssignStmt res = visit(cast(ir.Op) node);

		// we need to visit the ir.Op component to add the node to the scope
		if (!node.var)
			return null;

		res.rhs = new VarExp(node.loc, node.var);

		return res;
	}

	override AssignStmt visit(ir.InitOp node) {
		AssignStmt res = visit(cast(ir.Op) node);
		res.rhs = createInitializer(typec(node.type));
		return res;
	}

	override AssignStmt visit(ir.CastOp node) {
		AssignStmt res = visit(cast(ir.Op) node);
		res.rhs = new CastExp(node.loc, typec(node.type), new VarExp(node.loc, node.value));
		return res;
	}

	override AssignStmt visit(ir.IndexOp node) {
		AssignStmt res = visit(cast(ir.Op) node);
		res.rhs = new IndexExp(node.loc, typec(node.type),
			new VarExp(node.loc, node.base),
			new VarExp(node.loc, node.index),
		);
		return res;
	}

	override AssignStmt visit(ir.DotOp node) {
		AssignStmt res = visit(cast(ir.Op) node);
		res.rhs = new DotExp(node.loc, typec(node.type),
			new VarExp(node.loc, node.base),
			node.index,
		);
		return res;
	}

	override AssignStmt visit(ir.DerefOp node) {
		AssignStmt res = visit(cast(ir.Op) node);
		res.rhs = new VarExp(node.loc, node.base);

		// TODO: do we wanna fill in the interior types, too?

		foreach (seg; node.path) {
			final switch (seg.type) {
			case ir.RefPathSegmentType.dot:
				res.rhs = new DotExp(node.loc, null, res.rhs, seg.id);
				break;
			case ir.RefPathSegmentType.index:
				res.rhs = new IndexExp(node.loc, null, res.rhs, new VarExp(node.loc, seg.id));
				break;
			}
		}

		res.rhs.type = typec(node.type);

		return res;
	}

	override AssignStmt visit(ir.ArrayOp node) {
		AssignStmt res = visit(cast(ir.Op) node);

		Exp[] members;
		foreach (m; node.members)
			members ~= new VarExp(node.loc, m);

		res.rhs = new ArrayExp(node.loc, typec(node.type), members);
		return res;
	}

	override Stmt visit(ir.CallOp node) {
		Exp[] args;
		foreach (arg; node.args)
			args ~= new VarExp(node.loc, arg);

		CallExp call = new CallExp(node.loc, typec(node.type),
			node.func,
			args,
		);

		if (ir.ScalarType scalar = cast(ir.ScalarType) node.type) {
			if (scalar.pod == POD.Void) {
				ExpStmt e = new ExpStmt();
				e.loc = node.loc;
				e.value = call;
				return e;
			}
		}

		AssignStmt res = visit(cast(ir.Op) node);
		res.rhs = call;
		return res;
	}

	override AssignStmt visit(ir.ErrorOp node) {
		assert(0);
	}

	override Shader visit(ir.Shader node) {
		Shader res = new Shader();
		res.loc = node.loc;
		res.shaderType = node.shaderType;

		foreach (decl; node.declarations)
			res.declarations ~= cast(Declaration) decl.accept(this);

		return res;
	}

}
