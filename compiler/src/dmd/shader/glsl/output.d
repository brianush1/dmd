module dmd.shader.glsl.output;
static import ir = dmd.shader.glsl.ir;
import dmd.shader.glsl.compiler;
import dmd.arsd.mvd;

string generateOutput(ASTBase node, bool minified) {
	ir.minifiedIdentifiers = minified;
	OutputGenerator gen = new OutputGenerator();
	gen.minified = minified;
	gen.visit(node);
	return gen.result;
}

private class OutputGenerator {

	bool minified;
	int identifierCounter;
	ShaderType shaderType;

	private {
		string buffer;

		int indent = 0;
		bool startOfLine = true;
		bool space = false;

		void newline() {
			write("\n");
		}

		static bool isAlphanumeric(char ch) {
			return (ch >= 'a' && ch <= 'z')
				|| (ch >= 'A' && ch <= 'Z')
				|| (ch >= '0' && ch <= '9')
				|| ch == '_';
		}

		void write(string s) {
			foreach (char ch; s) {
				if (ch == '\n') {
					if (!minified)
						buffer ~= '\n';
					startOfLine = true;
				}
				else if (ch == ' ' && minified) {
					space = true;
				}
				else {
					if (startOfLine && !minified)
						foreach (i; 0 .. indent)
							buffer ~= '\t';

					// FIXME: also avoid operator grouping, i.e.
					// 2 + +3 (valid) turning into 2++3 (invalid)
					if (space && buffer.length > 0
							&& isAlphanumeric(buffer[$ - 1])
							&& isAlphanumeric(ch)) {
						buffer ~= ' ';
					}

					buffer ~= ch;
					space = false;

					startOfLine = false;
				}
			}
		}

		void writeln(string s) {
			write(s);
			write("\n");
		}
	}

	string encodeId(Identifier id) {
		return id.unique;
	}

	string result() const @property { return buffer; }

	void visit(ASTBase node) { return mvdObj!visitImpl(this, node); }

	void visitImpl(ASTBase node) { assert(0); }

	void visitImpl(Type node) { assert(0); }

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
		import std.conv : text;
		import dmd.shader.glsl.ir : getPODPrefix;

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
			import std.conv : text;

			write(text("[", dim, "]"));
		}
	}

	void visitImpl(StructType node) {
		write(encodeId(node.name));
	}

	void visitImpl(Declaration node) { assert(0); }

	void visitImpl(Struct node) {
		write("struct ");
		write(encodeId(node.name));
		writeln(" {");

		indent += 1;
		foreach (field; node.fields) {
			visit(field.type);
			write(" ");
			write(encodeId(field.name));
			writeln(";");
		}
		indent -= 1;

		writeln("};");
	}

	void visitImpl(Function node) {
		visit(node.returnType);
		write(" ");
		write(encodeId(node.name));
		write("(");
		foreach (i, param; node.params) {
			if (i > 0)
				write(", ");
			if (param.isInout)
				write("inout ");
			visit(param.type);
			write(" ");
			write(encodeId(param.name));
		}
		writeln(") {");

		indent += 1;

		struct SField {
			string type;
			Identifier name;
		}

		SField[] locals;
		foreach (local; node.locals) {
			// TODO: save more of the state, just to be more robust
			string save = buffer;
			buffer = "";
			visit(local.type);
			string typeStr = buffer;
			buffer = save;

			locals ~= SField(typeStr, local.name);
		}

		import std.algorithm : sort, SwapStrategy;

		locals.sort!((a, b) => a.type < b.type, SwapStrategy.stable);
		string prevType;
		foreach (i, local; locals) {
			if (prevType != local.type) {
				if (prevType !is null) writeln(";");
				write(local.type);
				write(" ");
			}
			else {
				write(", ");
			}
			write(encodeId(local.name));
			prevType = local.type;
		}
		if (locals.length > 0)
			writeln(";");

		foreach (stmt; node.body) {
			visit(stmt);
		}
		indent -= 1;

		writeln("}");
	}

	void visitImpl(Global node) {
		if (node.qualifiers & GlobalQualifiers.Flat)
			write("flat ");
		if (node.qualifiers & GlobalQualifiers.Uniform)
			write("uniform ");
		if (node.qualifiers & GlobalQualifiers.Varying)
			write("varying ");
		if (node.qualifiers & GlobalQualifiers.In)
			write("in ");
		if (node.qualifiers & GlobalQualifiers.Out)
			write("out ");

		visit(node.type);
		write(" ");
		write(encodeId(node.name));
		writeln(";");
	}

	void visitImpl(Stmt node) { assert(0); }

	void visitImpl(AssignStmt node) {
		if (!node.rhs)
			return;

		write(encodeId(node.base));
		foreach (seg; node.path) {
			if (Dot d = cast(Dot) seg) {
				write(".");
				write(encodeId(d.id));
			}
			else if (Index i = cast(Index) seg) {
				write("[");
				visit(i.exp);
				write("]");
			}
		}
		write(" = ");
		visit(node.rhs);
		writeln(";");
	}

	void visitImpl(ReturnStmt node) {
		if (node.value) {
			write("return ");
			visit(node.value);
			writeln(";");
		}
		else {
			writeln("return;");
		}
	}

	void visitImpl(ExpStmt node) {
		visit(node.value);
		writeln(";");
	}

	void visitImpl(DiscardStmt node) {
		writeln("discard;");
	}

	void visitImpl(BreakStmt node) {
		writeln("break;");
	}

	void visitImpl(Block node) { assert(0); }

	void visitImpl(IfBlock node) {
		write("if (");
		visit(node.condition);
		writeln(") {");
		indent += 1;
		foreach (s; node.body)
			visit(s);
		indent -= 1;
		writeln("}");

		if (node.elseBody.length > 0) {
			writeln("else {");
			indent += 1;
			foreach (s; node.elseBody)
				visit(s);
			indent -= 1;
			writeln("}");
		}
	}

	void visitImpl(WhileBlock node) {
		write("while (");
		visit(node.condition);
		writeln(") {");
		indent += 1;
		foreach (s; node.body)
			visit(s);
		indent -= 1;
		writeln("}");
	}

	void visitImpl(Exp node) { assert(0); }

	void visitImpl(NumericLiteral node) {
		write(node.value);
	}

	void visitImpl(VarExp node) {
		write(encodeId(node.id));
	}

	void visitImpl(BinExp node) {
		write("(");
		visit(node.lhs);
		write(" ");
		write(node.op);
		write(" ");
		visit(node.rhs);
		write(")");
	}

	void visitImpl(UnaExp node) {
		write("(");
		write(node.op);
		visit(node.value);
		write(")");
	}

	void visitImpl(CastExp node) {
		visit(node.type);
		write("(");
		visit(node.value);
		write(")");
	}

	void visitImpl(IndexExp node) {
		visit(node.base);
		write("[");
		visit(node.index);
		write("]");
	}

	void visitImpl(DotExp node) {
		visit(node.base);
		write(".");
		write(encodeId(node.index));
	}

	void visitImpl(ArrayExp node) {
		visit(node.type);
		write("(");
		foreach (i, member; node.members) {
			if (i > 0)
				write(", ");
			visit(member);
		}
		write(")");
	}

	void visitImpl(CallExp node) {
		write(encodeId(node.func));
		write("(");
		foreach (i, arg; node.args) {
			if (i > 0)
				write(", ");
			visit(arg);
		}
		write(")");
	}

	void visitImpl(Shader node) {
		shaderType = node.shaderType;
		foreach (decl; node.declarations) {
			visit(decl);
		}
	}

}
