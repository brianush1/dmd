module dmd.shader;
import dmd.shader.glsl.frontend;
import dmd.shader.glsl.irdump;
import dmd.shader.glsl.inliner;
static import ir = dmd.shader.glsl.ir;
static import compiler = dmd.shader.glsl.compiler;
import dmd.shader.glsl.optimizer;
import dmd.shader.glsl.output;
import dmd.common.outbuffer;
import dmd.root.filename;
import dmd.root.file;
import dmd.expression;
import dmd.dmodule;
import dmd.arraytypes;
import dmd.dsymbol;
import std.string : toStringz;

struct ShaderTraits {
	char[][][string] sources;
	char[][][string] renames;
}

ShaderTraits[string] shaderTraits;
char[][][string] globalTraits;

void writeShaderFiles(ref Modules modules) {
	TargetGLSL target = new TargetGLSL();

	bool[void*] modulesDone;

	foreach (mod; modules) {
		modulesDone[cast(void*) mod] = true;
		mod.accept(target);
	}

	ir.Declaration[] declarations;
	ir.Declaration[ir.Identifier] declarationsByName;

	foreach (sym; target.symbols.byValue) {
		if (sym.source) {
			declarations ~= sym.source;
			declarationsByName[sym.name] = sym.source;
		}
	}

	ir.ShaderClass[] shaderClasses;
	foreach (decl; declarations) {
		if (ir.ShaderClass sc = cast(ir.ShaderClass) decl) {
			shaderClasses ~= sc;
		}
	}

	final class ModuleShader {
		Dsymbol classSym;
		string[ir.ShaderType] sources;
		ir.Identifier[string] interfaceRenames;
	}

	struct ModuleShaders {
		Module mod;
		ModuleShader[] shaders;
	}

	ModuleShaders[void*] compiled;

	foreach (shaderClass; shaderClasses) {
		ShaderProgram program = new ShaderProgram();
		ModuleShader moduleShader = new ModuleShader();

		foreach (member; [ir.ShaderType.Vertex, ir.ShaderType.Fragment]) {
			ir.Shader shader = new ir.Shader();
			shader.declarations = declarations.dup;
			shader.shaderType = member;

			string methodName, ext;
			final switch (shader.shaderType) {
				case ir.ShaderType.Vertex: methodName = "processVertex", ext = "vert.glsl"; break;
				case ir.ShaderType.Fragment: methodName = "processFragment", ext = "frag.glsl"; break;
			}

			ir.Declaration[] roots;
			foreach (name; shaderClass.methods)
				if (name.name == methodName)
					roots ~= declarationsByName[name];

			// TODO: proper error reporting
			assert(roots.length == 1);

			foreach (decl; shaderClass.fields) {
				ir.Global global = new ir.Global();
				global.loc = decl.loc;
				global.qualifiers = decl.qualifiers;
				global.type = decl.type;
				global.name = decl.name;
				shader.declarations ~= global;
				roots ~= global;
			}

			ir.trim(shader, roots);
			foreach (ref decl; shader.declarations) {
				ir.Declaration cloned = decl.clone();
				foreach (ref root; roots)
					if (root is decl)
						root = cloned;
				decl = cloned;
			}

			bool noErrors = checkErrors(shader);
			if (!noErrors)
				continue;
			reorder(shader);
			replaceBuiltins(shader);
			doInline(shader);
			resolveReferences(shader);
			removeBreakLabels(shader);
			resolveFinalizers(shader);
			resolveNameConflicts(shader);
			removeAssertsAndThrows(shader);
			ir.Function mainFunc = addMainFunction(program, shader, cast(ir.Function) roots[0]);

			ir.trim(shader, [mainFunc]);

			compiler.Shader sh = compiler.compileShader(shader);
			sh = optimizeShader(sh);

			moduleShader.sources[sh.shaderType] = generateOutput(sh, false);

			foreach (i, decl; shaderClass.fields) {
				ir.Global renamedGlobal = cast(ir.Global) roots[i + 1];
				ir.Identifier name = renamedGlobal.name;
				if (name in program.renamedGlobals)
					name = program.renamedGlobals[name];
				moduleShader.interfaceRenames[decl.name.name] = name;
			}
		}

		Module mod = shaderClass.originalSymbol.getModule;
		if (cast(void*) mod !in compiled)
			compiled[cast(void*) mod] = ModuleShaders(mod);

		moduleShader.classSym = shaderClass.originalSymbol;

		compiled[cast(void*) mod].shaders ~= moduleShader;
	}

	import dmd.globals : global;
	import dmd.errors : fatal;
	if (global.errors)
		fatal();

	void setData(char[] buffer, char[] res) {
		buffer[] = '\0';
		buffer[0 .. res.length] = res[];
	}

	foreach (name, id; builtinGlobals) {
		if (name in globalTraits) {
			foreach (rec; globalTraits[name]) {
				char[] res = id.unique.dup;
				setData(rec, res);
			}
		}
	}

	foreach (modPtr, info; compiled) {
		Module mod = info.mod;
		string fileName = mod.srcfile.toString.idup;

		if (auto e = FileName.ext(fileName))
			fileName = fileName[0 .. $ - e.length - 1];

		fileName ~= "__shadersource.d";

		string source;

		OutBuffer fqnBuf;
		mod.fullyQualifiedName(fqnBuf);
		source ~= "module " ~ fqnBuf[] ~ "__shadersource;\n";

		foreach (name, id; builtinGlobals) {
			source ~= "template Global(string name) if (name == \"" ~ name;
			source ~= "\") { enum Global = \"";
			source ~= id.unique;
			source ~= "\"; }\n";
		}

		foreach (shader; info.shaders) {
			import dmd.dmangle : mangleToBuffer;

			OutBuffer mangleBuf;
			mangleToBuffer(shader.classSym, &mangleBuf);
			string mangled = mangleBuf[].idup;
			assert(mangled[0 .. 2] != "_D");

			ShaderTraits* traits = mangled in shaderTraits ? &shaderTraits[mangled] : null;

			source ~= "struct C" ~ mangled ~ " {\n";

			foreach (type, shaderSource; shader.sources) {
				import std.conv : text;

				string stringType = text(type) ~ "Source";
				source ~= "\tenum " ~ stringType ~ " = \"";
				source ~= shaderSource;
				source ~= "\";\n";

				if (traits && stringType in traits.sources) {
					foreach (rec; traits.sources[stringType]) {
						char[] res = shaderSource.dup;
						setData(rec, res);
					}
				}
			}

			foreach (from, to; shader.interfaceRenames) {
				source ~= "\ttemplate Rename(string name) if (name == \"" ~ from;
				source ~= "\") { enum Rename = \"";
				source ~= to.unique;
				source ~= "\"; }\n";

				if (traits && from in traits.renames) {
					foreach (rec; traits.renames[from]) {
						char[] res = to.unique.dup;
						setData(rec, res);
					}
				}
			}

			source ~= "}\n";
		}

		File.write(fileName.toStringz, source);
	}
}
