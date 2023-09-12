module dmd.shader.glsl.irdump;
import dmd.shader.glsl.ir;
import dmd.arsd.mvd;
import dmd.errors;
import dmd.location;
import std.conv;

class IRDumper {

	private {
		string buffer;

		int indent = 0;
		bool startOfLine = false;

		void newline() {
			write("\n");
		}

		void write(string s) {
			foreach (char ch; s) {
				if (ch == '\n') {
					buffer ~= '\n';
					startOfLine = true;
				}
				else {
					if (startOfLine)
						foreach (i; 0 .. indent)
							buffer ~= '\t';
					buffer ~= ch;
					startOfLine = false;
				}
			}
		}

		void writeln(string s) {
			write(s);
			write("\n");
		}
	}

	string result() const @property { return buffer; }

	bool[IRBase] visitedSet;
	void visit(IRBase node) {
		if (node in visitedSet) {
			write("(% ");
			scope (exit)
				write(" %)");
			return mvdObj!visitImpl(this, node);
		}
		else {
			visitedSet[node] = true;
			return mvdObj!visitImpl(this, node);
		}
	}

	void visitImpl(ScalarType node) {
		final switch (node.pod) {
			case POD.Void: write("void"); break;
			case POD.Bool: write("bool"); break;
			case POD.Int: write("int"); break;
			case POD.UInt: write("uint"); break;
			case POD.Float: write("float"); break;
			case POD.Double: write("double"); break;
		}
	}

	void visitImpl(VectorType node) {
		write(getPODPrefix(node.pod));
		write("vec");
		write(text(node.dim));
	}

	void visitImpl(OpaqueType node) {
		write(node.name);
	}

	void visitImpl(ArrayType node) {
		visit(node.element);
		foreach (dim; node.dims) {
			write(text("[", dim, "]"));
		}
	}

	void visitImpl(StructType node) {
		write(node.declaration.name.unique);
	}

	void visitImpl(ErrorType node) {
		// error(node.loc, "error type encountered in GLSL code");
		write("error-type");
	}

	void visitImpl(Struct node) {
		write("struct ");
		write(node.name.unique);
		writeln(" {");

		indent += 1;
		foreach (field; node.fields) {
			visit(field.type);
			write(" ");
			write(field.name.unique);
			writeln(";");
		}
		indent -= 1;

		writeln("};");
	}

	void visitImpl(ShaderClass node) {
		assert(0);
	}

	void visitImpl(Function node) {
		if (node.isRef)
			write("ref ");
		visit(node.returnType);
		write(" ");
		write(node.name.unique);
		write("(");
		foreach (i, param; node.outerVars ~ node.params) {
			if (i > 0)
				write(", ");
			if (param.isRef)
				write("inout ");
			visit(param.type);
			write(" ");
			write(param.name.unique);
		}
		writeln(") {");

		indent += 1;
		foreach (stmt; node.body) {
			visit(stmt);
		}
		indent -= 1;

		writeln("}");
	}

	void visitImpl(Global node) {
		visit(node.type);
		write(" ");
		write(node.name.unique);
		writeln(";");
	}

	void visitImpl(AssignStmt node) {
		write(node.lhs.unique);
		write(" = ");
		if (node.isRef)
			write("ref ");
		write(node.rhs.unique);
		writeln(";");
	}

	void visitImpl(ResolvedAssignStmt node) {
		write(node.base.unique);
		foreach (seg; node.path) {
			final switch (seg.type) {
			case RefPathSegmentType.dot:
				write(".");
				write(seg.id.unique);
				break;
			case RefPathSegmentType.index:
				write("[");
				write(seg.id.unique);
				write("]");
				break;
			}
		}
		write(" = ");
		write(node.rhs.unique);
		writeln(";");
	}

	void visitImpl(ReturnStmt node) {
		if (node.value) {
			write("return ");
			write(node.value.unique);
			writeln(";");
		}
		else {
			writeln("return;");
		}
	}

	void visitImpl(DiscardStmt node) {
		writeln("discard;");
	}

	void visitImpl(ThrowStmt node) {
		write("throw from ");
		write(text(node.loc));
		writeln(";");
	}

	void visitImpl(BreakStmt node) {
		if (node.label) {
			write("break ");
			write(node.label.unique);
			writeln(";");
		}
		else {
			writeln("break;");
		}
	}

	void visitImpl(AssertStmt node) {
		write("assert ");
		write(node.condition.unique);
		writeln(";");
	}

	void visitImpl(ErrorStmt node) {
		writeln("error-stmt;");
	}

	void visitImpl(IfBlock node) {
		write("if (");
		write(node.condition.unique);
		writeln(") {");
		indent += 1;
		foreach (stmt; node.body)
			visit(stmt);
		indent -= 1;
		writeln("}");
		if (node.elseBody.length != 0) {
			writeln("else {");
			indent += 1;
			foreach (stmt; node.elseBody)
				visit(stmt);
			indent -= 1;
			writeln("}");
		}
	}

	void visitImpl(LoopBlock node) {
		if (node.label) {
			write(node.label.unique);
			write(": ");
		}
		writeln("while (true) {");
		indent += 1;
		foreach (stmt; node.body)
			visit(stmt);
		indent -= 1;
		writeln("}");
	}

	void visitImpl(FinalizerBlock node) {
		writeln("try {");
		indent += 1;
		foreach (stmt; node.body)
			visit(stmt);
		indent -= 1;
		writeln("}");
		writeln("finally {");
		indent += 1;
		foreach (stmt; node.finalizer)
			visit(stmt);
		indent -= 1;
		writeln("}");
	}

	void visitImpl(Op node) {
		if (node.isRef)
			write("ref ");

		visit(node.type);
		write(" ");
		write(node.name.unique);
		write(" = ");
	}

	void visitImpl(NumericLiteralOp node) {
		visitImpl(cast(Op) node);
		write(node.value);
		writeln(";");
	}

	void visitImpl(BinOp node) {
		visitImpl(cast(Op) node);
		write(node.lhs.unique);
		write(" " ~ node.op ~ " ");
		write(node.rhs.unique);
		writeln(";");
	}

	void visitImpl(UnaOp node) {
		visitImpl(cast(Op) node);
		write(node.op);
		write(node.value.unique);
		writeln(";");
	}

	void visitImpl(LoadOp node) {
		if (node.isRef)
			write("ref ");

		visit(node.type);
		write(" ");
		write(node.name.unique);
		if (node.var) {
			write(" = ");
			write(node.var.unique);
		}
		writeln(";");
	}

	void visitImpl(InitOp node) {
		visit(node.type);
		write(" ");
		write(node.name.unique);
		writeln(" = init;");
	}

	void visitImpl(CastOp node) {
		visitImpl(cast(Op) node);
		visit(node.type);
		write("(");
		write(node.value.unique);
		writeln(");");
	}

	void visitImpl(IndexOp node) {
		visitImpl(cast(Op) node);
		write(node.base.unique);
		write("[");
		write(node.index.unique);
		writeln("];");
	}

	void visitImpl(DotOp node) {
		visitImpl(cast(Op) node);
		write(node.base.unique);
		write(".");
		write(node.index.unique);
		writeln(";");
	}

	void visitImpl(DerefOp node) {
		visitImpl(cast(Op) node);
		write(node.base.unique);
		foreach (seg; node.path) {
			final switch (seg.type) {
			case RefPathSegmentType.dot:
				write(".");
				write(seg.id.unique);
				break;
			case RefPathSegmentType.index:
				write("[");
				write(seg.id.unique);
				write("]");
				break;
			}
		}
		writeln(";");
	}

	void visitImpl(ArrayOp node) {
		visitImpl(cast(Op) node);
		visit(node.type);
		write("(");
		foreach (i, member; node.members) {
			if (i > 0)
				write(", ");
			write(member.unique);
		}
		writeln(");");
	}

	void visitImpl(CallOp node) {
		visitImpl(cast(Op) node);
		write(node.func.unique);
		write("(");
		foreach (i, arg; node.args) {
			if (i > 0)
				write(", ");
			write(arg.unique);
		}
		writeln(");");
	}

	void visitImpl(ErrorOp node) {
		visitImpl(cast(Op) node);
		writeln("error-value;");
	}

	void visitImpl(Shader node) {
		foreach (d; node.declarations) {
			visit(d);
		}
	}

}
