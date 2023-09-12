module dmd.shader.glsl.optimizer;
import dmd.shader.glsl.compiler;
import dmd.arsd.mvd;

Shader optimizeShader(Shader node) {
	foreach (level; 0 .. 30) {
		ValuePropagation prop = new ValuePropagation();
		node = cast(Shader) prop.visit(node);

		if (prop.changes == 0)
			break;
	}

	return node;
}

private:

class RefCounter {

	final void visit(ASTBase node) { return mvdObj!visitImpl(this, node); }

	int[Identifier] counter;

	void mark(Identifier id) {
		if (id !in counter)
			counter[id] = 0;

		counter[id] += 1;
	}

	void visitBlock(ref Stmt[] body) {
		foreach (s; body)
			visit(s);
	}

	void visitImpl(Function node) {
		visitBlock(node.body);
	}

	void visitImpl(AssignStmt node) {
		if (node.rhs is null)
			return;

		mark(node.base);

		foreach (ref seg; node.path)
			if (Index i = cast(Index) seg)
				visit(i.exp);

		visit(node.rhs);
	}

	void visitImpl(ReturnStmt node) {
		visit(node.value);
	}

	void visitImpl(ExpStmt node) {
		visit(node.value);
	}

	void visitImpl(DiscardStmt node) {}

	void visitImpl(BreakStmt node) {}

	void visitImpl(IfBlock node) {
		visit(node.condition);
		visitBlock(node.body);
		visitBlock(node.elseBody);
	}

	void visitImpl(WhileBlock node) {
		visit(node.condition);
		visitBlock(node.body);
	}

	void visitImpl(NumericLiteral node) {}

	void visitImpl(VarExp node) {
		mark(node.id);
	}

	void visitImpl(BinExp node) {
		visit(node.lhs);
		visit(node.rhs);
	}

	void visitImpl(UnaExp node) {
		visit(node.value);
	}

	void visitImpl(CastExp node) {
		visit(node.value);
	}

	void visitImpl(IndexExp node) {
		visit(node.base);
		visit(node.index);
	}

	void visitImpl(DotExp node) {
		visit(node.base);
	}

	void visitImpl(ArrayExp node) {
		foreach (m; node.members)
			visit(m);
	}

	void visitImpl(CallExp node) {
		foreach (arg; node.args)
			visit(arg);
	}

}

class ValuePropagation {

	int changes;

	final ASTBase visit(ASTBase node) { return mvdObj!visitImpl(this, node); }

	void visitBlock(ref Stmt[] body) {
		import std.range : iota, retro;
		import std.algorithm : remove;

		resetAll();
		foreach (ref s; body)
			s = cast(Stmt) visit(s);
		foreach (i; iota(0, body.length).retro) {
			auto s = body[i];
			if (s is null || s in toRemove) {
				body = body.remove(i);
				toRemove.remove(s);
			}
		}
	}

	struct PropagationInfo {
		Exp value;
		Stmt assign;
	}

	PropagationInfo[Identifier] prop;

	RefCounter refCounter;
	Function currentFunction;
	bool[Stmt] toRemove;

	void resetAll() {
		prop = prop.init;
	}

	void reset(Identifier id) {
		prop.remove(id);
	}

	Struct visitImpl(Struct node) { return node; }
	Global visitImpl(Global node) { return node; }

	Function visitImpl(Function node) {
		refCounter = new RefCounter();
		refCounter.visit(node);
		currentFunction = node;
		visitBlock(node.body);
		return node;
	}

	AssignStmt visitImpl(AssignStmt node) {
		foreach (ref seg; node.path) {
			if (Index i = cast(Index) seg) {
				i.exp = cast(Exp) visit(i.exp);
			}
		}

		node.rhs = cast(Exp) visit(node.rhs);

		if (node.path.length == 0)
			if (VarExp v = cast(VarExp) node.rhs)
				if (node.base is v.id)
					return null;

		if (node.path.length == 0 && refCounter.counter[node.base] == 2) {
			prop[node.base] = PropagationInfo(node.rhs, node);
		}
		else {
			reset(node.base);
		}

		return node;
	}

	ReturnStmt visitImpl(ReturnStmt node) {
		node.value = cast(Exp) visit(node.value);
		return node;
	}

	ExpStmt visitImpl(ExpStmt node) {
		node.value = cast(Exp) visit(node.value);
		return node;
	}

	DiscardStmt visitImpl(DiscardStmt node) {
		return node;
	}

	BreakStmt visitImpl(BreakStmt node) {
		return node;
	}

	Block visitImpl(IfBlock node) {
		node.condition = cast(Exp) visit(node.condition);
		visitBlock(node.body);
		visitBlock(node.elseBody);
		return node;
	}

	Block visitImpl(WhileBlock node) {
		node.condition = cast(Exp) visit(node.condition);
		visitBlock(node.body);
		return node;
	}

	NumericLiteral visitImpl(NumericLiteral node) {
		return node;
	}

	Exp visitImpl(VarExp node) {
		if (node.id in prop) {
			changes += 1;
			Exp res = prop[node.id].value;
			toRemove[prop[node.id].assign] = true;
			foreach (i, local; currentFunction.locals) {
				import std.algorithm : remove;

				if (local.name == node.id) {
					currentFunction.locals = currentFunction.locals.remove(i);
					break;
				}
			}
			reset(node.id);
			return res;
		}
		else {
			return node;
		}
	}

	BinExp visitImpl(BinExp node) {
		node.lhs = cast(Exp) visit(node.lhs);
		node.rhs = cast(Exp) visit(node.rhs);
		return node;
	}

	UnaExp visitImpl(UnaExp node) {
		node.value = cast(Exp) visit(node.value);
		return node;
	}

	CastExp visitImpl(CastExp node) {
		node.value = cast(Exp) visit(node.value);
		return node;
	}

	IndexExp visitImpl(IndexExp node) {
		node.base = cast(Exp) visit(node.base);
		node.index = cast(Exp) visit(node.index);
		return node;
	}

	DotExp visitImpl(DotExp node) {
		node.base = cast(Exp) visit(node.base);
		return node;
	}

	ArrayExp visitImpl(ArrayExp node) {
		foreach (ref m; node.members)
			m = cast(Exp) visit(m);
		return node;
	}

	CallExp visitImpl(CallExp node) {
		foreach (ref arg; node.args)
			arg = cast(Exp) visit(arg);

		resetAll();

		return node;
	}

	Shader visitImpl(Shader node) {
		foreach (ref decl; node.declarations) {
			decl = cast(Declaration) visit(decl);
		}

		return node;
	}

}
