module dmd.shader.glsl.ir;
import dmd.errors;
import dmd.location;
import dmd.dsymbol;

bool[string] nameSet;
bool minifiedIdentifiers;
int identifierCounter;

class Identifier {
	immutable(string) name;
	immutable(bool) raw;

	this(string name = null, bool raw = false) {
		this.name = name;
		this.raw = raw;

		if (raw) {
			nameSet[name] = true;
			m_unique = name;
		}
	}

	// TODO: avoid using this
	void modify(string name, bool raw) {
		*cast(string*) &this.name = name;
		*cast(bool*) &this.raw = raw;

		if (raw) {
			nameSet[name] = true;
			m_unique = name;
		}
	}

	private static string encode(int v) {
		import std.range : retro;
		import std.conv : to;

		v += 1;

		string res;
		while (v > 0) {
			if (v % 26 == 0) {
				res ~= 'Z';
				v /= 26;
				v -= 1;
			}
			else {
				res ~= cast(char)('A' + v % 26 - 1);
				v /= 26;
			}
		}

		return res.retro.to!string;
	}

	private string m_unique;
	string unique() {
		import std.conv : text;
		import std.random : uniform;

		if (m_unique !is null)
			return m_unique;

		if (minifiedIdentifiers) // TODO: avoid accidentally generating reserved name
			return m_unique = encode(identifierCounter++);

		string pureName;
		char last = 0;
		foreach (char ch; name is null ? "tmp" : "d_" ~ name) {
			if (ch == last && ch == '_')
				continue;
			last = ch;
			pureName ~= ch;
		}

		for (int i = uniform!"[]"(0, 100_000);; i++) {
			string candidate = i == 0 ? pureName : text(pureName, i);
			if (candidate !in nameSet) {
				nameSet[candidate] = true;
				m_unique = candidate;
				break;
			}
		}

		return m_unique;
	}
}

// TODO: change all visitors to use arsd.mvd
abstract class Visitor(T = void) {

	T visit(IRBase node) { assert(0); }
	T visit(Type node) { return visit(cast(IRBase) node); }
	abstract T visit(ScalarType node) { return visit(cast(Type) node); }
	abstract T visit(VectorType node) { return visit(cast(Type) node); }
	abstract T visit(OpaqueType node) { return visit(cast(Type) node); }
	abstract T visit(ArrayType node) { return visit(cast(Type) node); }
	abstract T visit(StructType node) { return visit(cast(Type) node); }
	abstract T visit(ErrorType node) { return visit(cast(Type) node); }
	T visit(Declaration node) { return visit(cast(IRBase) node); }
	abstract T visit(Struct node) { return visit(cast(Declaration) node); }
	abstract T visit(ShaderClass node) { return visit(cast(Declaration) node); }
	abstract T visit(Function node) { return visit(cast(Declaration) node); }
	abstract T visit(Global node) { return visit(cast(Declaration) node); }
	T visit(Stmt node) { return visit(cast(IRBase) node); }
	abstract T visit(AssignStmt node) { return visit(cast(Stmt) node); }
	abstract T visit(ResolvedAssignStmt node) { return visit(cast(Stmt) node); }
	abstract T visit(ReturnStmt node) { return visit(cast(Stmt) node); }
	abstract T visit(DiscardStmt node) { return visit(cast(Stmt) node); }
	abstract T visit(ThrowStmt node) { return visit(cast(Stmt) node); }
	abstract T visit(BreakStmt node) { return visit(cast(Stmt) node); }
	abstract T visit(AssertStmt node) { return visit(cast(Stmt) node); }
	abstract T visit(ErrorStmt node) { return visit(cast(Stmt) node); }
	T visit(Block node) { return visit(cast(Stmt) node); }
	abstract T visit(IfBlock node) { return visit(cast(Block) node); }
	abstract T visit(LoopBlock node) { return visit(cast(Block) node); }
	abstract T visit(FinalizerBlock node) { return visit(cast(Block) node); }
	T visit(Op node) { return visit(cast(Stmt) node); }
	abstract T visit(NumericLiteralOp node) { return visit(cast(Op) node); }
	abstract T visit(BinOp node) { return visit(cast(Op) node); }
	abstract T visit(UnaOp node) { return visit(cast(Op) node); }
	abstract T visit(LoadOp node) { return visit(cast(Op) node); }
	abstract T visit(InitOp node) { return visit(cast(Op) node); }
	abstract T visit(CastOp node) { return visit(cast(Op) node); }
	abstract T visit(IndexOp node) { return visit(cast(Op) node); }
	abstract T visit(DotOp node) { return visit(cast(Op) node); }
	abstract T visit(DerefOp node) { return visit(cast(Op) node); }
	abstract T visit(ArrayOp node) { return visit(cast(Op) node); }
	abstract T visit(CallOp node) { return visit(cast(Op) node); }
	abstract T visit(ErrorOp node) { return visit(cast(Op) node); }
	abstract T visit(Shader node) { return visit(cast(IRBase) node); }

}

abstract class IRBase {
	Loc loc;

	IRBase clone() { assert(0); }
	protected IRBase cloneInto(IRBase target) { target.loc = loc; return target; }

	T accept(T)(Visitor!T visitor) {
		if (auto node = cast(ScalarType) this) return visitor.visit(node);
		if (auto node = cast(VectorType) this) return visitor.visit(node);
		if (auto node = cast(OpaqueType) this) return visitor.visit(node);
		if (auto node = cast(ArrayType) this) return visitor.visit(node);
		if (auto node = cast(StructType) this) return visitor.visit(node);
		if (auto node = cast(ErrorType) this) return visitor.visit(node);
		if (auto node = cast(Type) this) return visitor.visit(node);
		if (auto node = cast(Struct) this) return visitor.visit(node);
		if (auto node = cast(ShaderClass) this) return visitor.visit(node);
		if (auto node = cast(Function) this) return visitor.visit(node);
		if (auto node = cast(Global) this) return visitor.visit(node);
		if (auto node = cast(Declaration) this) return visitor.visit(node);
		if (auto node = cast(NumericLiteralOp) this) return visitor.visit(node);
		if (auto node = cast(BinOp) this) return visitor.visit(node);
		if (auto node = cast(UnaOp) this) return visitor.visit(node);
		if (auto node = cast(LoadOp) this) return visitor.visit(node);
		if (auto node = cast(InitOp) this) return visitor.visit(node);
		if (auto node = cast(CastOp) this) return visitor.visit(node);
		if (auto node = cast(IndexOp) this) return visitor.visit(node);
		if (auto node = cast(DotOp) this) return visitor.visit(node);
		if (auto node = cast(DerefOp) this) return visitor.visit(node);
		if (auto node = cast(ArrayOp) this) return visitor.visit(node);
		if (auto node = cast(CallOp) this) return visitor.visit(node);
		if (auto node = cast(ErrorOp) this) return visitor.visit(node);
		if (auto node = cast(Op) this) return visitor.visit(node);
		if (auto node = cast(AssignStmt) this) return visitor.visit(node);
		if (auto node = cast(ResolvedAssignStmt) this) return visitor.visit(node);
		if (auto node = cast(ReturnStmt) this) return visitor.visit(node);
		if (auto node = cast(DiscardStmt) this) return visitor.visit(node);
		if (auto node = cast(ThrowStmt) this) return visitor.visit(node);
		if (auto node = cast(BreakStmt) this) return visitor.visit(node);
		if (auto node = cast(AssertStmt) this) return visitor.visit(node);
		if (auto node = cast(ErrorStmt) this) return visitor.visit(node);
		if (auto node = cast(IfBlock) this) return visitor.visit(node);
		if (auto node = cast(LoopBlock) this) return visitor.visit(node);
		if (auto node = cast(FinalizerBlock) this) return visitor.visit(node);
		if (auto node = cast(Block) this) return visitor.visit(node);
		if (auto node = cast(Stmt) this) return visitor.visit(node);
		if (auto node = cast(Shader) this) return visitor.visit(node);

		import std.conv : text;
		assert(0, "unhandled " ~ text(this));
	}
}

private mixin template cloneMixin(T) {
	override T clone() {
		static if (__traits(isAbstractClass, T))
			assert(0);
		else
			return cloneInto(new T());
	}

	protected override T cloneInto(IRBase into_) {
		T into = cast(T) into_;

		super.cloneInto(into_);

		import std.traits : FieldNameTuple;

		static foreach (field; FieldNameTuple!T) {{
			static if (is(typeof(__traits(getMember, this, field)) : IRBase)) {
				__traits(getMember, into, field) = __traits(getMember, this, field).clone();
			}
			else static if (is(typeof(__traits(getMember, this, field)) == K[], K)) {
				static if (is(K : IRBase)) {
					foreach (member; __traits(getMember, this, field)) {
						__traits(getMember, into, field) ~= member.clone();
					}
				}
				else {
					foreach (member; __traits(getMember, this, field)) {
						__traits(getMember, into, field) ~= member;
					}
				}
			}
			else {
				__traits(getMember, into, field) = __traits(getMember, this, field);
			}
		}}

		return into;
	}
}

enum POD {
	Void,
	Bool,
	Int,
	UInt,
	Float,
	Double,
}

string getPODPrefix(POD pod) {
	final switch (pod) {
	case POD.Void:
		assert(0);
	case POD.Bool:
		return "b";
	case POD.Int:
		return "i";
	case POD.UInt:
		return "u";
	case POD.Float:
		return "";
	case POD.Double:
		return "d";
	}
}

abstract class Type : IRBase {
	mixin cloneMixin!(typeof(this));
}

class ScalarType : Type {
	mixin cloneMixin!(typeof(this));

	POD pod;

	this(POD pod = POD.init) {
		this.pod = pod;
	}
}

class VectorType : Type {
	mixin cloneMixin!(typeof(this));

	POD pod;
	int dim;

	private this() {}

	this(POD pod, int dim) {
		this.pod = pod;
		this.dim = dim;
	}
}

class OpaqueType : Type {
	mixin cloneMixin!(typeof(this));

	string name;

	private this() {}

	this(string name) {
		this.name = name;
	}
}

class ArrayType : Type {
	mixin cloneMixin!(typeof(this));

	Type element;
	immutable(int)[] dims;
}

class StructType : Type {
	mixin cloneMixin!(typeof(this));

	Struct declaration;
}

class ErrorType : Type {
	mixin cloneMixin!(typeof(this));

	string msg;

	private this() {}

	this(string msg) {
		this.msg = msg;
	}
}

abstract class Declaration : IRBase {
	mixin cloneMixin!(typeof(this));

	Identifier name;
	Dsymbol originalSymbol;
}

struct Field {
	Type type;
	Identifier name;
}

class Struct : Declaration {
	mixin cloneMixin!(typeof(this));

	Field[] fields;
}

enum GlobalQualifiers {
	None = 0,
	Uniform = 1,
	Varying = 2,
	In = 4,
	Out = 8,
	Flat = 16,
}

struct ShaderField {
	Loc loc;
	GlobalQualifiers qualifiers;
	Type type;
	Identifier name;
}

class ShaderClass : Declaration {
	mixin cloneMixin!(typeof(this));

	ShaderField[] fields;
	Identifier[] methods;
}

struct Param {
	bool isRef;
	Type type;
	Identifier name;
}

class Function : Declaration {
	mixin cloneMixin!(typeof(this));

	bool isRef;

	Type returnType;

	/// outer variables referenced by this closure; always marked ref
	Param[] outerVars;

	Param[] params;

	Stmt[] body;
}

class Global : Declaration {
	mixin cloneMixin!(typeof(this));

	GlobalQualifiers qualifiers;
	Type type;
}

abstract class Stmt : IRBase {
	mixin cloneMixin!(typeof(this));
}

class AssignStmt : Stmt {
	mixin cloneMixin!(typeof(this));

	Identifier lhs, rhs;
	bool isRef;
}

enum RefPathSegmentType {
	dot,
	index,
}

struct RefPathSegment {
	RefPathSegmentType type;
	Identifier id;
}

class ResolvedAssignStmt : Stmt {
	mixin cloneMixin!(typeof(this));

	Identifier base;
	RefPathSegment[] path;
	Identifier rhs;
}

class ReturnStmt : Stmt {
	mixin cloneMixin!(typeof(this));

	/** nullable */
	Identifier value;
}

class DiscardStmt : Stmt {
	mixin cloneMixin!(typeof(this));
}

class ThrowStmt : Stmt {
	mixin cloneMixin!(typeof(this));
}

class BreakStmt : Stmt {
	mixin cloneMixin!(typeof(this));

	Identifier label;
}

class AssertStmt : Stmt {
	mixin cloneMixin!(typeof(this));

	Identifier condition;
}

class ErrorStmt : Stmt {
	mixin cloneMixin!(typeof(this));

	string msg;

	private this() {}

	this(string msg) {
		this.msg = msg;
	}
}

abstract class Block : Stmt {
	mixin cloneMixin!(typeof(this));

	Stmt[] body;
}

class IfBlock : Block {
	mixin cloneMixin!(typeof(this));

	Identifier condition;
	Stmt[] elseBody;
}

class LoopBlock : Block {
	mixin cloneMixin!(typeof(this));

	Identifier label;
}

class FinalizerBlock : Block {
	mixin cloneMixin!(typeof(this));

	Stmt[] finalizer;
}

abstract class Op : Stmt {
	mixin cloneMixin!(typeof(this));

	/++
	The only operations that can be ref are:
	- DotOp (always)
	- IndexOp (always)
	- LoadOp (sometimes)
	- CallOp (sometimes)
	+/
	bool isRef;

	Type type;
	Identifier name;
}

class NumericLiteralOp : Op {
	mixin cloneMixin!(typeof(this));

	string value;
}

class BinOp : Op {
	mixin cloneMixin!(typeof(this));

	string op;
	Identifier lhs, rhs;
}

class UnaOp : Op {
	mixin cloneMixin!(typeof(this));

	string op;
	Identifier value;
}

class LoadOp : Op {
	mixin cloneMixin!(typeof(this));

	/** nullable */
	Identifier var;
}

class InitOp : Op {
	mixin cloneMixin!(typeof(this));
}

class CastOp : Op {
	mixin cloneMixin!(typeof(this));

	Identifier value;
}

class IndexOp : Op {
	mixin cloneMixin!(typeof(this));

	Identifier base, index;
}

class DotOp : Op {
	mixin cloneMixin!(typeof(this));

	Identifier base, index;
}

class DerefOp : Op {
	mixin cloneMixin!(typeof(this));

	Identifier base;
	RefPathSegment[] path;
}

class ArrayOp : Op {
	mixin cloneMixin!(typeof(this));

	Identifier[] members;
}

class CallOp : Op {
	mixin cloneMixin!(typeof(this));

	Identifier func;
	Identifier[] args;
}

class ErrorOp : Op {
	mixin cloneMixin!(typeof(this));

	string msg;

	private this() {}

	this(string msg) {
		this.msg = msg;
	}
}

enum ShaderType {
	Vertex,
	Fragment,
}

class Shader : IRBase {
	mixin cloneMixin!(typeof(this));

	ShaderType shaderType;
	Declaration[] declarations;
}

void trim(Shader shader, Declaration[] roots) {
	bool[Identifier] usedSymbols;

	void mark(Identifier id) {
		if (id !in usedSymbols)
			usedSymbols[id] = true;
	}

	class TrimVisitor : Visitor!void {
		alias visit = Visitor!void.visit;

		override void visit(ScalarType node) { }
		override void visit(VectorType node) { }
		override void visit(OpaqueType node) { }

		override void visit(ArrayType node) {
			node.element.accept(this);
		}

		override void visit(StructType node) {
			mark(node.declaration.name);
		}

		override void visit(ErrorType node) {}

		override void visit(Struct node) {
			mark(node.name);
			foreach (field; node.fields) {
				field.type.accept(this);
				mark(field.name);
			}
		}

		override void visit(ShaderClass node) {
			mark(node.name);
			foreach (field; node.fields) {
				field.type.accept(this);
				mark(field.name);
			}
		}

		override void visit(Function node) {
			mark(node.name);
			node.returnType.accept(this);
			foreach (param; node.outerVars ~ node.params) {
				param.type.accept(this);
				mark(param.name);
			}
			foreach (i, stmt; node.body) {
				stmt.accept(this);
			}
		}

		override void visit(Global node) {
			mark(node.name);
			node.type.accept(this);
		}

		override void visit(AssignStmt node) {
			mark(node.lhs);
			mark(node.rhs);
		}

		override void visit(ResolvedAssignStmt node) {
			mark(node.base);

			foreach (seg; node.path) {
				mark(seg.id);
			}

			mark(node.rhs);
		}

		override void visit(ReturnStmt node) {
			if (node.value)
				mark(node.value);
		}

		override void visit(DiscardStmt node) {}

		override void visit(ThrowStmt node) {}

		override void visit(BreakStmt node) {
			mark(node.label);
		}

		override void visit(AssertStmt node) {
			mark(node.condition);
		}

		override void visit(ErrorStmt node) {}

		override void visit(IfBlock node) {
			mark(node.condition);
			foreach (stmt; node.body)
				stmt.accept(this);
			foreach (stmt; node.elseBody)
				stmt.accept(this);
		}

		override void visit(LoopBlock node) {
			mark(node.label);
			foreach (stmt; node.body)
				stmt.accept(this);
		}

		override void visit(FinalizerBlock node) {
			foreach (stmt; node.body)
				stmt.accept(this);
			foreach (stmt; node.finalizer)
				stmt.accept(this);
		}

		override void visit(Op node) {
			if (node.type)
				node.type.accept(this);
			mark(node.name);
		}

		override void visit(NumericLiteralOp node) {
			visit(cast(Op) node);
		}

		override void visit(BinOp node) {
			visit(cast(Op) node);
			mark(node.lhs);
			mark(node.rhs);
		}

		override void visit(UnaOp node) {
			visit(cast(Op) node);
			mark(node.value);
		}

		override void visit(LoadOp node) {
			visit(cast(Op) node);
			if (node.var)
				mark(node.var);
		}

		override void visit(InitOp node) {
			visit(cast(Op) node);
		}

		override void visit(CastOp node) {
			visit(cast(Op) node);
			mark(node.value);
		}

		override void visit(IndexOp node) {
			visit(cast(Op) node);
			mark(node.base);
			mark(node.index);
		}

		override void visit(DotOp node) {
			visit(cast(Op) node);
			mark(node.base);
			mark(node.index);
		}

		override void visit(DerefOp node) {
			visit(cast(Op) node);
			mark(node.base);

			foreach (seg; node.path) {
				mark(seg.id);
			}
		}

		override void visit(ArrayOp node) {
			visit(cast(Op) node);
			foreach (member; node.members) {
				mark(member);
			}
		}

		override void visit(CallOp node) {
			visit(cast(Op) node);
			mark(node.func);
			foreach (arg; node.args) {
				mark(arg);
			}
		}

		override void visit(ErrorOp node) {
			visit(cast(Op) node);
		}

		override void visit(Shader node) {}

	}

	TrimVisitor trimmer = new TrimVisitor();

	bool[Declaration] visited;

	foreach (root; roots) {
		visited[root] = true;
		root.accept(trimmer);
	}

	Declaration[] keptDeclarations;

	// TODO: avoid walking over the whole declarations array every time we go deeper
	while (true) {
		bool changes = false;

		foreach (d; keptDeclarations) {
			if (d !in visited) {
				visited[d] = true;
				d.accept(trimmer);
			}
		}

		foreach (d; shader.declarations) {
			if (d.name in usedSymbols && usedSymbols[d.name]) {
				keptDeclarations ~= d;
				usedSymbols[d.name] = false;
				changes = true;
			}
		}

		if (!changes)
			break;
	}

	// maintain order:
	keptDeclarations = [];

	foreach (d; shader.declarations) {
		if (d.name in usedSymbols) {
			keptDeclarations ~= d;
		}
	}

	shader.declarations = keptDeclarations;

}
