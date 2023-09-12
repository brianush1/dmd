module dmd.shader.glsl.inliner;
import dmd.shader.glsl.ir;
import dmd.errors;
import dmd.location;
import dmd.arsd.mvd;
import dmd.dsymbol;
import std.string : toStringz;
import std.algorithm : min;
import std.range : retro;
import std.typecons : Tuple, tuple;
import std.conv;

// TODO: structure the code for the stages so it looks more similar

// grep 'STAGE'

private:

string fqnOf(Declaration decl) {
	Dsymbol v = decl.originalSymbol;

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

// STAGE 0: error checking

public bool checkErrors(IRBase node) {
	return mvd!checkErrorsImpl(node);
}

bool checkErrors(Stmt[] body) {
	bool result = true;
	foreach (s; body)
		result = result && checkErrors(s);
	return result;
}

bool checkErrorsImpl(Shader shader) {
	bool result = true;
	foreach (d; shader.declarations)
		result = result && checkErrors(d);
	return result;
}

int countReturns;

bool checkErrorsImpl(Function func) {
	bool result = true;
	result = result && checkErrors(func.returnType);
	foreach (param; func.outerVars ~ func.params) {
		if (param.type.loc == Loc.initial)
			param.type.loc = func.loc;
		result = result && checkErrors(param.type);
	}
	countReturns = 0;
	result = result && checkErrors(func.body);

	if (func.isRef && countReturns != 1) {
		error(func.loc, "`ref` functions in shader code must have exactly one return statement");
	}

	return result;
}

bool checkErrorsImpl(Struct s) {
	bool result = true;
	foreach (field; s.fields)
		result = result && checkErrors(field.type);
	return result;
}

bool checkErrorsImpl(Global global) {
	bool result = true;
	if (global.type.loc == Loc.initial)
		global.type.loc = global.loc;
	result = result && checkErrors(global.type);
	return result;
}

bool checkErrorsImpl(Type type) { return true; }
bool checkErrorsImpl(Stmt stmt) { return true; }

bool checkErrorsImpl(ReturnStmt stmt) {
	countReturns += 1;
	return true;
}

bool checkErrorsImpl(Op op) {
	if (op.type.loc == Loc.initial)
		op.type.loc = op.loc;

	bool result = true;
	result = result && checkErrors(op.type);
	return result;
}

bool checkErrorsImpl(Block block) { assert(0, "unimplemented block"); }

bool checkErrorsImpl(IfBlock block) {
	bool result = true;
	result = result && checkErrors(block.body);
	result = result && checkErrors(block.elseBody);
	return result;
}

bool checkErrorsImpl(LoopBlock block) {
	bool result = true;
	result = result && checkErrors(block.body);
	return result;
}

bool checkErrorsImpl(FinalizerBlock block) {
	bool result = true;
	result = result && checkErrors(block.body);
	result = result && checkErrors(block.finalizer);
	return result;
}

bool[Tuple!(Loc, string)] errorSet;

bool checkErrorsImpl(ErrorType err) {
	if (err.msg is null)
		return false;

	auto t = tuple(err.loc, err.msg);
	if (t !in errorSet) {
		errorSet[t] = true;
		error(err.loc, "%s", err.msg.toStringz);
	}

	return false;
}

bool checkErrorsImpl(ErrorStmt err) {
	if (err.msg is null)
		return false;

	auto t = tuple(err.loc, err.msg);
	if (t !in errorSet) {
		errorSet[t] = true;
		error(err.loc, "%s", err.msg.toStringz);
	}

	return false;
}

bool checkErrorsImpl(ErrorOp err) {
	if (err.msg is null)
		return false;

	auto t = tuple(err.loc, err.msg);
	if (t !in errorSet) {
		errorSet[t] = true;
		error(err.loc, "%s", err.msg.toStringz);
	}

	return false;
}

// STAGE 1: reordering

public void reorder(Shader shader) {
	Struct[] structs;
	Global[] globals;
	Function[Identifier] functions;
	foreach (d; shader.declarations) {
		if (Function f = cast(Function) d) {
			functions[f.name] = f;
		}
		else if (Struct s = cast(Struct) d) {
			structs ~= s;
		}
		else if (Global g = cast(Global) d) {
			globals ~= g;
		}
	}

	Function[][Function] depList;
	int[Function] dependentCount;

	foreach (f; functions.byValue)
		dependentCount[f] = 0;

	void processBlock(ref Function[] deps, Stmt[] body) {
		foreach (s; body) {
			if (IfBlock b = cast(IfBlock) s) {
				processBlock(deps, b.body);
				processBlock(deps, b.elseBody);
			}
			else if (LoopBlock b = cast(LoopBlock) s) {
				processBlock(deps, b.body);
			}
			else if (FinalizerBlock b = cast(FinalizerBlock) s) {
				processBlock(deps, b.body);
				processBlock(deps, b.finalizer);
			}
			else if (Block b = cast(Block) s) {
				assert(0);
			}
			else if (CallOp call = cast(CallOp) s) {
				if (call.func !in functions)
					continue;
				Function calledFunc = functions[call.func];
				deps ~= calledFunc;
				dependentCount[calledFunc] += 1;
			}
		}
	}

	foreach (f; functions.byValue) {
		Function[] deps;
		processBlock(deps, f.body);
		depList[f] = deps;
	}

	Function[] orderedFunctions;
	Function[Function] parents;
	int[Function] visited;

	void topSort(Function curr, Function parent) {
		if (curr in visited) {
			if (visited[curr] == 1) {
				error(curr.loc, "recursion is not available in shader code");

				Function otherParent = parents[curr];
				if (otherParent)
					errorSupplemental(otherParent.loc, "%s ->", toStringz(fqnOf(otherParent)));
				if (parent !is curr)
					errorSupplemental(curr.loc, "%s -> [...] ->", toStringz(fqnOf(curr)));
				errorSupplemental(parent.loc, "%s ->", toStringz(fqnOf(parent)));
				errorSupplemental(curr.loc, "%s", toStringz(fqnOf(curr)));
			}

			return;
		}

		visited[curr] = 1;
		parents[curr] = parent;

		foreach (d; depList[curr])
			topSort(d, curr);

		visited[curr] = 2;

		orderedFunctions ~= curr;
	}

	// try to visit functions without dependents first
	foreach (f, c; dependentCount)
		if (c == 0)
			topSort(f, null);

	// but if every function is part of a cycle, just visit the rest
	foreach (f, c; dependentCount)
		if (f !in visited)
			topSort(f, null);

	// TODO: order structs based on dependencies
	Struct[] orderedStructs = structs;

	shader.declarations =
		cast(Declaration[]) orderedStructs
		~ cast(Declaration[]) globals
		~ cast(Declaration[]) orderedFunctions;
}

// STAGE 2: replace builtin functions

public Identifier[string] builtinGlobals;
bool[Identifier] builtinGlobalSet;

Identifier getBuiltinGlobal(string s) {
	if (s !in builtinGlobals) {
		builtinGlobals[s] = new Identifier();
		builtinGlobalSet[builtinGlobals[s]] = true;
	}
	return builtinGlobals[s];
}

public void replaceBuiltins(Shader shader) {
	import std.range : iota;
	import std.algorithm : find;

	foreach (i; iota(0, shader.declarations.length).retro) {
		Declaration decl = shader.declarations[i];
		if (Function func = cast(Function) decl) {
			string fqn = fqnOf(func);

			if (fqn == "gd.shaders.Shader.viewport") {
				func.body.length = 0;

				Struct rect = (cast(StructType) func.returnType).declaration;

				Global viewportPos = new Global();
				viewportPos.loc = func.loc;
				viewportPos.qualifiers = GlobalQualifiers.Uniform;
				viewportPos.name = getBuiltinGlobal("viewportPos");
				viewportPos.type = new VectorType(POD.Int, 2);

				Global viewportSize = new Global();
				viewportSize.loc = func.loc;
				viewportSize.qualifiers = GlobalQualifiers.Uniform;
				viewportSize.name = getBuiltinGlobal("viewportSize");
				viewportSize.type = new VectorType(POD.Int, 2);

				shader.declarations = shader.declarations[0 .. i]
					~ viewportPos
					~ viewportSize
					~ shader.declarations[i .. $];

				CallOp call = new CallOp();
				call.loc = func.loc;
				call.func = rect.name;
				call.name = new Identifier();
				call.type = func.returnType;
				call.args ~= viewportPos.name;
				call.args ~= viewportSize.name;
				func.body ~= call;

				ReturnStmt ret = new ReturnStmt();
				ret.loc = func.loc;
				ret.value = call.name;
				func.body ~= ret;
			}
			else if (fqn == "gd.shaders.Shader.instanceID") {
				func.body.length = 0;

				ReturnStmt ret = new ReturnStmt();
				ret.loc = func.loc;

				final switch (shader.shaderType) {
				case ShaderType.Vertex:
					ret.value = new Identifier("gl_InstanceID", true);
					break;
				case ShaderType.Fragment:
					Global instanceID = new Global();
					instanceID.loc = func.loc;
					instanceID.qualifiers = GlobalQualifiers.In | GlobalQualifiers.Flat;
					instanceID.name = getBuiltinGlobal("instanceID");
					instanceID.type = new ScalarType(POD.Int);

					shader.declarations = shader.declarations[0 .. i]
						~ instanceID
						~ shader.declarations[i .. $];

					ret.value = getBuiltinGlobal("instanceID");
					break;
				}

				varTypes[ret.value] = new ScalarType(POD.Int);
				nameMap[ret.value] = ret.value;
				func.body ~= ret;
			}
			else if (fqn == "gd.shaders.Shader.fragCoord") {
				func.body.length = 0;

				// TODO: error if this is used in vertex shader

				ReturnStmt ret = new ReturnStmt();
				ret.loc = func.loc;
				ret.value = new Identifier("gl_FragCoord", true);
				varTypes[ret.value] = new ScalarType(POD.Bool);
				nameMap[ret.value] = ret.value;
				func.body ~= ret;
			}
			else if (fqn == "gd.shaders.Shader.isFrontFacing") {
				func.body.length = 0;

				// TODO: error if this is used in vertex shader

				ReturnStmt ret = new ReturnStmt();
				ret.loc = func.loc;
				ret.value = new Identifier("gl_FrontFacing", true);
				varTypes[ret.value] = new ScalarType(POD.Bool);
				nameMap[ret.value] = ret.value;
				func.body ~= ret;
			}
			else if (fqn == "gd.shaders.Shader.discard") {
				func.body.length = 0;

				DiscardStmt discard = new DiscardStmt();
				discard.loc = func.loc;
				func.body ~= discard;
			}
		}
	}
}

// STAGE 3: inlining

public void doInline(Shader shader) {
	Inliner inliner = new Inliner();
	inliner.processShader(shader);
}

final class Inliner {

	Declaration[Identifier] declarationMap;

	struct ReturnInfo {
		Identifier returnId, exitId;
		bool isRef;
	}

	Stmt[] replaceReturn(ReturnInfo info, Stmt[] body) {
		Stmt[] result;

		foreach (s; body) {
			if (Block b = cast(Block) s) {
				if (IfBlock ifBlock = cast(IfBlock) b) {
					IfBlock node = new IfBlock();
					node.loc = ifBlock.loc;
					node.condition = ifBlock.condition;
					node.body = replaceReturn(info, ifBlock.body);
					node.elseBody = replaceReturn(info, ifBlock.elseBody);
					result ~= node;
				}
				else if (LoopBlock loopBlock = cast(LoopBlock) b) {
					LoopBlock node = new LoopBlock();
					node.loc = loopBlock.loc;
					node.label = loopBlock.label;
					node.body = replaceReturn(info, loopBlock.body);
					result ~= node;
				}
				else if (FinalizerBlock finalizerBlock = cast(FinalizerBlock) b) {
					FinalizerBlock node = new FinalizerBlock();
					node.loc = finalizerBlock.loc;
					node.body = replaceReturn(info, finalizerBlock.body);

					// returns are not allowed in finalizers, but we use this opportunity to do a deep copy
					node.finalizer = replaceReturn(info, finalizerBlock.finalizer);

					result ~= node;
				}
				else {
					assert(0);
				}
			}
			else if (ReturnStmt r = cast(ReturnStmt) s) {
				if (r.value && info.returnId) {
					AssignStmt assign = new AssignStmt();
					assign.loc = r.loc;
					assign.isRef = info.isRef;
					assign.lhs = info.returnId;
					assign.rhs = r.value;
					result ~= assign;
				}

				BreakStmt jump = new BreakStmt();
				jump.loc = r.loc;
				jump.label = info.exitId;
				result ~= jump;
			}
			else {
				result ~= s.clone;
			}
		}

		return result;
	}

	Stmt[] inlineBody(Stmt[] body) {
		Stmt[] result;

		foreach (s; body) {
			if (Block b = cast(Block) s) {
				if (IfBlock ifBlock = cast(IfBlock) b) {
					IfBlock node = new IfBlock();
					node.loc = ifBlock.loc;
					node.condition = ifBlock.condition;
					node.body = inlineBody(ifBlock.body);
					node.elseBody = inlineBody(ifBlock.elseBody);
					result ~= node;
				}
				else if (LoopBlock loopBlock = cast(LoopBlock) b) {
					LoopBlock node = new LoopBlock();
					node.loc = loopBlock.loc;
					node.label = loopBlock.label;
					node.body = inlineBody(loopBlock.body);
					result ~= node;
				}
				else if (FinalizerBlock finalizerBlock = cast(FinalizerBlock) b) {
					FinalizerBlock node = new FinalizerBlock();
					node.loc = finalizerBlock.loc;
					node.body = inlineBody(finalizerBlock.body);
					node.finalizer = inlineBody(finalizerBlock.finalizer);
					result ~= node;
				}
				else {
					assert(0);
				}
			}
			else if (CallOp call = cast(CallOp) s) {
				Function func = call.func in declarationMap
					? cast(Function) declarationMap[call.func]
					: null;

				bool needsInlining = true; //func.isRef || func.outerVars.length > 0;

				if (!func || !needsInlining) {
					result ~= s;
					continue;
				}

				/+if (!func.isRef && func.outerVars.length > 0) {
					Identifier[] prepend;
					foreach (i, v; func.outerVars) {
						prepend ~= v.name;
					}

					call.args = prepend ~ call.args;
					result ~= call;
					continue;
				}+/

				Identifier returnId = null; // void functions don't have a return variable
				Identifier exitId = new Identifier();

				ScalarType scalarReturn = cast(ScalarType) func.returnType;
				if (!(scalarReturn && scalarReturn.pod == POD.Void)) {
					returnId = call.name;

					LoadOp returnDef = new LoadOp();
					returnDef.loc = call.loc;
					returnDef.isRef = func.isRef;
					returnDef.type = func.returnType;
					returnDef.name = returnId;
					result ~= returnDef;
				}

				foreach (i, arg; call.args) {
					Param param = func.params[i];
					LoadOp argLoad = new LoadOp();
					argLoad.loc = call.loc;
					argLoad.isRef = param.isRef;
					argLoad.type = param.type;
					argLoad.name = param.name;
					argLoad.var = arg;
					result ~= argLoad;
				}

				LoopBlock loop = new LoopBlock();
				loop.loc = call.loc;
				loop.label = exitId;
				loop.body = replaceReturn(ReturnInfo(returnId, exitId, func.isRef), func.body);
				result ~= loop;

				BreakStmt breakGuard = new BreakStmt();
				breakGuard.loc = call.loc;
				breakGuard.label = exitId;
				loop.body ~= breakGuard;
			}
			else {
				result ~= s;
			}
		}

		return result;
	}

	void processShader(Shader shader) {
		foreach (d; shader.declarations) {
			declarationMap[d.name] = d;
		}

		foreach (d; shader.declarations) {
			if (Function func = cast(Function) d) {
				func.body = inlineBody(func.body);
			}
		}
	}

}

// STAGE 4: ref removal

public void resolveReferences(Shader shader) {
	ReferenceResolver resolver = new ReferenceResolver();
	resolver.processShader(shader);
}

final class ReferenceResolver {

	struct RefPath {
		Identifier base;
		RefPathSegment[] path;

		RefPath add(RefPathSegment seg) {
			RefPath res = this;
			res.path ~= seg;
			return res;
		}
	}

	RefPath[Identifier] refs;
	Type[Identifier] types;
	Stmt[] currentScope;

	RefPath getRef(Identifier id) {
		if (id in refs) {
			return refs[id];
		}
		else {
			return RefPath(id);
		}
	}

	Identifier deref(Identifier id) {
		if (id in refs) {
			RefPath path = refs[id];

			Identifier temp = new Identifier();
			DerefOp op = new DerefOp();
			op.name = temp;
			op.type = types[id];
			op.base = path.base;
			op.path = path.path;
			currentScope ~= op;

			return temp;
		}
		else {
			return id;
		}
	}

	void dump(Identifier id) {
		/+RefReturnDebug node = new RefReturnDebug();
		node.lhs = id;
		node.base = refs[id].base;
		node.path = refs[id].path;
		currentScope ~= node;+/
	}

	Stmt[] resolveRefs(Stmt[] body) {
		Stmt[] save = currentScope;
		scope (exit)
			currentScope = save;
		currentScope = [];

		foreach (s; body) {
			if (Op op = cast(Op) s) {
				types[op.name] = op.type;
			}

			if (AssignStmt assign = cast(AssignStmt) s) {
				if (assign.isRef) {
					refs[assign.lhs] = assign.rhs in refs ? refs[assign.rhs] : RefPath(assign.rhs);

					dump(assign.lhs);
				}
				else {
					auto resolved = assign.lhs in refs ? refs[assign.lhs] : RefPath(assign.lhs);

					ResolvedAssignStmt node = new ResolvedAssignStmt();
					node.loc = assign.loc;
					node.base = resolved.base;
					node.path = resolved.path;
					node.rhs = deref(assign.rhs);
					currentScope ~= node;
				}
			}
			else if (ReturnStmt stmt = cast(ReturnStmt) s) {
				stmt.value = deref(stmt.value);
				currentScope ~= stmt;
			}
			else if (AssertStmt stmt = cast(AssertStmt) s) {
				stmt.condition = deref(stmt.condition);
				currentScope ~= stmt;
			}
			else if (IfBlock ifBlock = cast(IfBlock) s) {
				ifBlock.body = resolveRefs(ifBlock.body);
				ifBlock.elseBody = resolveRefs(ifBlock.elseBody);

				currentScope ~= ifBlock;
			}
			else if (LoopBlock loopBlock = cast(LoopBlock) s) {
				loopBlock.body = resolveRefs(loopBlock.body);

				currentScope ~= loopBlock;
			}
			else if (FinalizerBlock finalizerBlock = cast(FinalizerBlock) s) {
				finalizerBlock.body = resolveRefs(finalizerBlock.body);
				finalizerBlock.finalizer = resolveRefs(finalizerBlock.finalizer);

				currentScope ~= finalizerBlock;
			}
			else if (Block b = cast(Block) s) {
				assert(0, "unhandled block type");
			}
			else if (BinOp op = cast(BinOp) s) {
				op.lhs = deref(op.lhs);
				op.rhs = deref(op.rhs);
				currentScope ~= op;
			}
			else if (UnaOp op = cast(UnaOp) s) {
				op.value = deref(op.value);
				currentScope ~= op;
			}
			else if (LoadOp op = cast(LoadOp) s) {
				if (op.isRef) {
					if (!op.var) {
						// ref function return value; handled in AssignStmt
						continue;
					}

					refs[op.name] = getRef(op.var);

					dump(op.name);
				}
				else {
					op.var = deref(op.var);
					currentScope ~= op;
				}
			}
			else if (CastOp op = cast(CastOp) s) {
				op.value = deref(op.value);
				currentScope ~= op;
			}
			else if (IndexOp op = cast(IndexOp) s) {
				assert(op.index in types, "cannot resolve " ~ op.index.unique);

				Identifier copiedIndex = new Identifier();
				LoadOp copyIndex = new LoadOp();
				copyIndex.name = copiedIndex;
				copyIndex.type = types[op.index];
				copyIndex.var = deref(op.index);
				currentScope ~= copyIndex;

				refs[op.name] = getRef(op.base).add(RefPathSegment(
					RefPathSegmentType.index,
					copiedIndex,
				));

				dump(op.name);
			}
			else if (DotOp op = cast(DotOp) s) {
				refs[op.name] = getRef(op.base).add(RefPathSegment(
					RefPathSegmentType.dot,
					op.index,
				));

				dump(op.name);
			}
			else if (DerefOp op = cast(DerefOp) s) {
				assert(0);
			}
			else if (ArrayOp op = cast(ArrayOp) s) {
				foreach (ref member; op.members) {
					member = deref(member);
				}
				currentScope ~= op;
			}
			else if (CallOp op = cast(CallOp) s) {
				assert(!op.isRef, "inliner must be called before reference resolution: " ~ op.func.unique);

				foreach (ref arg; op.args) {
					arg = deref(arg);
				}
				currentScope ~= op;
			}
			else {
				currentScope ~= s;
			}
		}

		return currentScope;
	}

	void processShader(Shader shader) {
		foreach (d; shader.declarations) {
			if (Function func = cast(Function) d) {
				refs = refs.init;
				foreach (param; func.outerVars ~ func.params) {
					types[param.name] = param.type;
				}
				func.body = resolveRefs(func.body);
			}
		}
	}

}

// STAGE 5: remove `break` labels

struct BLRes {
	int minBreakOut = int.max;

	static BLRes combine(BLRes[] args...) {
		BLRes result;

		foreach (arg; args) {
			result.minBreakOut = min(result.minBreakOut, arg.minBreakOut);
		}

		return result;
	}
}

public BLRes removeBreakLabels(IRBase node) {
	return mvd!removeBreakLabelsImpl(node);
}

BLRes removeBreakLabelsImpl(Shader shader) {
	foreach (d; shader.declarations) {
		if (Function f = cast(Function) d) {
			cast(void) removeBreakLabels(f);
		}
	}

	return BLRes(-1);
}

enum MAX_NESTED_LOOPS = 1024;
int nestingLevel = 0;
Identifier actualNestingLevelId;
int[Identifier] labelLevels;
int countBreaks;

Identifier nestingLevelId() {
	if (actualNestingLevelId is null)
		actualNestingLevelId = new Identifier("nestingLevel");
	return actualNestingLevelId;
}

BLRes removeBreakLabelsImpl(Function func) {
	actualNestingLevelId = null;

	cast(void) removeBreakLabels(func.body);

	if (actualNestingLevelId) {
		NumericLiteralOp op = new NumericLiteralOp();
		op.type = new ScalarType(POD.Int);
		op.name = actualNestingLevelId;
		op.value = text(MAX_NESTED_LOOPS);
		func.body = op ~ func.body;
	}

	return BLRes(-1);
}

BLRes removeBreakLabelsImpl(Stmt stmt) { return BLRes(); }

BLRes removeBreakLabelsImpl(Block block) { assert(0); }

BLRes removeBreakLabelsImpl(IfBlock s) {
	BLRes b = removeBreakLabels(s.body);
	BLRes e = removeBreakLabels(s.elseBody);
	return BLRes.combine(b, e);
}

BLRes removeBreakLabelsImpl(FinalizerBlock s) {
	BLRes b = removeBreakLabels(s.body);
	BLRes f = removeBreakLabels(s.finalizer);
	return BLRes.combine(b, f);
}

BLRes removeBreakLabels(ref Stmt[] body) {
	BLRes blres;
	Stmt[] result;
	foreach (s; body) {
		if (LoopBlock loop = cast(LoopBlock) s) {
			auto save = countBreaks;
			scope (exit)
				countBreaks = save;

			labelLevels[loop.label] = nestingLevel;
			loop.label = null;

			Stmt[] bodySubset = loop.body;
			foreach (i; 0 .. loop.body.length) {
				if (BreakStmt breakStmt = cast(BreakStmt) loop.body[i]) {
					bodySubset = loop.body[0 .. i + 1];
					break;
				}
			}

			countBreaks = 0;

			nestingLevel += 1;
			BLRes inner = removeBreakLabels(bodySubset);
			nestingLevel -= 1;

			if (countBreaks == 1 && cast(BreakStmt) bodySubset[$ - 1]) {
				result ~= bodySubset[0 .. $ - 1];
			}
			else {
				loop.body = bodySubset;
				result ~= loop;
			}

			if (inner.minBreakOut < nestingLevel && nestingLevel > 0) {
				NumericLiteralOp myLvl = new NumericLiteralOp();
				myLvl.type = new ScalarType(POD.Int);
				myLvl.name = new Identifier();
				myLvl.value = text(nestingLevel);
				result ~= myLvl;

				NumericLiteralOp maxLvl = new NumericLiteralOp();
				maxLvl.type = new ScalarType(POD.Int);
				maxLvl.name = new Identifier();
				maxLvl.value = text(MAX_NESTED_LOOPS);
				result ~= maxLvl;

				BinOp lt = new BinOp();
				lt.type = new ScalarType(POD.Bool);
				lt.name = new Identifier();
				lt.op = "<";
				lt.lhs = nestingLevelId;
				lt.rhs = myLvl.name;
				result ~= lt;

				IfBlock b = new IfBlock();
				b.condition = lt.name;
				b.body ~= new BreakStmt();

				AssignStmt assign = new AssignStmt();
				assign.lhs = nestingLevelId;
				assign.rhs = maxLvl.name;
				b.elseBody ~= assign;

				result ~= b;
			}

			blres = BLRes.combine(blres, inner);
		}
		else if (BreakStmt stmt = cast(BreakStmt) s) {
			int toLevel = labelLevels[stmt.label];

			countBreaks += 1;

			if (toLevel != nestingLevel - 1) {
				NumericLiteralOp op = new NumericLiteralOp();
				op.type = new ScalarType(POD.Int);
				op.name = new Identifier();
				op.value = text(toLevel);
				result ~= op;

				AssignStmt assign = new AssignStmt();
				assign.lhs = nestingLevelId;
				assign.rhs = op.name;
				result ~= assign;
			}

			stmt.label = null;
			result ~= stmt;

			blres = BLRes.combine(blres, BLRes(toLevel));
		}
		else {
			blres = BLRes.combine(blres, removeBreakLabels(s));
			result ~= s;
		}
	}

	body = result;

	return blres;
}

// STAGE 6: resolve finalizers

FinalizerBlock[] currentLoopFinalizers, currentFunctionFinalizers;
Type[Identifier] varTypes;

public void resolveFinalizers(IRBase node) {
	return mvd!resolveFinalizersImpl(node);
}

void resolveFinalizersImpl(Shader shader) {
	foreach (d; shader.declarations)
		if (Global g = cast(Global) d)
			varTypes[g.name] = g.type;
	foreach (d; shader.declarations)
		if (Function f = cast(Function) d)
			resolveFinalizers(f);
}

void resolveFinalizersImpl(Function func) {
	foreach (d; func.outerVars ~ func.params)
		varTypes[d.name] = d.type;
	resolveFinalizers(func.body);
}

void resolveFinalizersImpl(Stmt stmt) {}

void resolveFinalizersImpl(Block block) { assert(0); }

void resolveFinalizersImpl(IfBlock s) {
	resolveFinalizers(s.body);
	resolveFinalizers(s.elseBody);
}

void resolveFinalizers(ref Stmt[] body) {
	Stmt[] result;
	foreach (s; body) {
		if (FinalizerBlock b = cast(FinalizerBlock) s) {
			currentLoopFinalizers ~= b;
			currentFunctionFinalizers ~= b;
			resolveFinalizers(b.body);
			currentLoopFinalizers = currentLoopFinalizers[0 .. $ - 1];
			currentFunctionFinalizers = currentFunctionFinalizers[0 .. $ - 1];
			resolveFinalizers(b.finalizer);
			result ~= b.body;
			result ~= b.finalizer;
		}
		else if (LoopBlock b = cast(LoopBlock) s) {
			auto save = currentLoopFinalizers;
			scope (exit)
				currentLoopFinalizers = save;
			currentLoopFinalizers = [];
			resolveFinalizers(b.body);
			result ~= b;
		}
		else if (BreakStmt b = cast(BreakStmt) s) {
			foreach (f; currentLoopFinalizers.retro) {
				result ~= f.finalizer;
			}
			result ~= b;
		}
		else if (ReturnStmt b = cast(ReturnStmt) s) {
			assert(b.value in varTypes);

			LoadOp op = new LoadOp();
			op.type = varTypes[b.value];
			op.name = new Identifier();
			op.var = b.value;
			b.value = op.name;
			result ~= op;

			foreach (f; currentFunctionFinalizers.retro) {
				result ~= f.finalizer;
			}

			result ~= b;
		}
		else {
			if (Op op = cast(Op) s) {
				varTypes[op.name] = op.type;
			}
			resolveFinalizers(s);
			result ~= s;
		}
	}

	body = result;
}

// STAGE 7: resolve name conflicts

Identifier[Identifier] nameMap;

public void resolveNameConflicts(IRBase node) {
	return mvd!resolveNameConflictsImpl(node);
}

Identifier resolveName(Identifier id) {
	if (id in nameMap)
		return nameMap[id];
	else
		return new Identifier("<unresolved " ~ id.name ~ ">", true);
}

Identifier assignNewName(Identifier id) {
	return nameMap[id] = new Identifier(id.name, id.raw);
}

void resolveNameConflictsImpl(Shader shader) {
	foreach (d; shader.declarations) {
		if (Global g = cast(Global) d) {
			nameMap[g.name] = g.name;
		}
		else if (Function f = cast(Function) d) {
			resolveNameConflicts(f);
		}
	}
}

void resolveNameConflictsImpl(Function func) {
	foreach (ref d; func.outerVars)
		d.name = assignNewName(d.name);
	foreach (ref d; func.params)
		d.name = assignNewName(d.name);
	resolveNameConflicts(func.body);
}

void resolveNameConflictsImpl(Stmt stmt) { assert(0); }

void resolveNameConflictsImpl(AssignStmt stmt) {
	stmt.lhs = resolveName(stmt.lhs);
	stmt.rhs = resolveName(stmt.rhs);
}

void resolveNameConflictsImpl(ResolvedAssignStmt stmt) {
	stmt.base = resolveName(stmt.base);
	foreach (ref seg; stmt.path)
		if (seg.type == RefPathSegmentType.index)
			seg.id = resolveName(seg.id);
	stmt.rhs = resolveName(stmt.rhs);
}

void resolveNameConflictsImpl(ReturnStmt stmt) {
	if (stmt.value)
		stmt.value = resolveName(stmt.value);
}

void resolveNameConflictsImpl(DiscardStmt stmt) {}
void resolveNameConflictsImpl(ThrowStmt stmt) {}
void resolveNameConflictsImpl(BreakStmt stmt) {}

void resolveNameConflictsImpl(AssertStmt stmt) {
	stmt.condition = resolveName(stmt.condition);
}

void resolveNameConflictsImpl(Block block) { assert(0); }

void resolveNameConflictsImpl(IfBlock s) {
	s.condition = resolveName(s.condition);
	resolveNameConflicts(s.body);
	resolveNameConflicts(s.elseBody);
}

void resolveNameConflictsImpl(LoopBlock s) {
	resolveNameConflicts(s.body);
}

void resolveNameConflictsImpl(Op op) { assert(0); }

void resolveNameConflictsOp(Op op) {
	op.name = assignNewName(op.name);
}

void resolveNameConflictsImpl(NumericLiteralOp op) {
	resolveNameConflictsOp(op);
}

void resolveNameConflictsImpl(BinOp op) {
	resolveNameConflictsOp(op);
	op.lhs = resolveName(op.lhs);
	op.rhs = resolveName(op.rhs);
}

void resolveNameConflictsImpl(UnaOp op) {
	resolveNameConflictsOp(op);
	op.value = resolveName(op.value);
}

void resolveNameConflictsImpl(LoadOp op) {
	resolveNameConflictsOp(op);
	if (op.var)
		op.var = resolveName(op.var);
}

void resolveNameConflictsImpl(InitOp op) {
	resolveNameConflictsOp(op);
}

void resolveNameConflictsImpl(CastOp op) {
	resolveNameConflictsOp(op);
	op.value = resolveName(op.value);
}

void resolveNameConflictsImpl(IndexOp op) {
	resolveNameConflictsOp(op);
	op.base = resolveName(op.base);
	op.index = resolveName(op.index);
}

void resolveNameConflictsImpl(DotOp op) {
	resolveNameConflictsOp(op);
	op.base = resolveName(op.base);
}

void resolveNameConflictsImpl(DerefOp op) {
	resolveNameConflictsOp(op);
	op.base = resolveName(op.base);
	foreach (ref seg; op.path)
		if (seg.type == RefPathSegmentType.index)
			seg.id = resolveName(seg.id);
}

void resolveNameConflictsImpl(ArrayOp op) {
	resolveNameConflictsOp(op);
	foreach (ref m; op.members)
		m = resolveName(m);
}

void resolveNameConflictsImpl(CallOp op) {
	resolveNameConflictsOp(op);
	foreach (ref m; op.args)
		m = resolveName(m);
}

void resolveNameConflicts(ref Stmt[] body) {
	Stmt[] result;
	foreach (s; body) {
		resolveNameConflicts(s);
		result ~= s;
	}

	body = result;
}

// STAGE 8: remove asserts and throws

public void removeAssertsAndThrows(IRBase node) {
	return mvd!removeAssertsAndThrowsImpl(node);
}

void removeAssertsAndThrowsImpl(Shader shader) {
	foreach (d; shader.declarations) {
		if (Function f = cast(Function) d)
			removeAssertsAndThrows(f);
	}
}

void removeAssertsAndThrowsImpl(Function func) {
	removeAssertsAndThrows(func.body);

	foreach (s; func.body)
		if (cast(ReturnStmt) s || cast(DiscardStmt) s)
			return;

	// if we get here, there are no guaranteed returns in the function

	if (ScalarType sc = cast(ScalarType) func.returnType)
		if (sc.pod == POD.Void)
			return;

	// and if it's not a void function, we need to fix this

	LoadOp load = new LoadOp();
	load.loc = func.loc;
	load.type = func.returnType;
	load.name = new Identifier();
	func.body ~= load;

	ReturnStmt ret = new ReturnStmt();
	ret.loc = func.loc;
	ret.value = load.name;
	func.body ~= ret;
}

void removeAssertsAndThrowsImpl(Stmt stmt) {}

void removeAssertsAndThrowsImpl(Block block) { assert(0); }

void removeAssertsAndThrowsImpl(IfBlock s) {
	removeAssertsAndThrows(s.body);
	removeAssertsAndThrows(s.elseBody);
}

void removeAssertsAndThrowsImpl(LoopBlock s) {
	removeAssertsAndThrows(s.body);
}

void removeAssertsAndThrows(ref Stmt[] body) {
	Stmt[] result;
	foreach (s; body) {
		if (cast(ThrowStmt) s || cast(AssertStmt) s)
			continue;
		removeAssertsAndThrows(s);
		result ~= s;
	}

	body = result;
}

// STAGE 9: add boilerplate and main function

public class ShaderProgram {
	Identifier[Identifier] renamedGlobals;
}

public Function addMainFunction(ShaderProgram program, Shader shader, Function entryPoint) {
	import std.range : iota;
	import std.algorithm : remove;

	struct Rename {
		Identifier from, to;
	}
	Rename[] renamedInputs, renamedOutputs;

	foreach (i; iota(0, shader.declarations.length).retro) {
		Declaration decl = shader.declarations[i];
		if (Global g = cast(Global) decl) {
			if (g.qualifiers & GlobalQualifiers.Varying) {
				g.qualifiers &= ~GlobalQualifiers.Varying;
				final switch (shader.shaderType) {
				case ShaderType.Vertex:
					g.qualifiers |= GlobalQualifiers.Out;
					break;
				case ShaderType.Fragment:
					g.qualifiers |= GlobalQualifiers.In;
					break;
				}
			}
			else if (g.qualifiers & GlobalQualifiers.Out) {
				final switch (shader.shaderType) {
				case ShaderType.Vertex:
					shader.declarations = shader.declarations.remove(i);
					break;
				case ShaderType.Fragment:
					break;
				}
			}
		}
	}

	foreach (i; iota(0, shader.declarations.length).retro) {
		Declaration decl = shader.declarations[i];
		if (Global g = cast(Global) decl) {
			bool isInput, isOutput;

			if ((g.qualifiers & GlobalQualifiers.Uniform) || (g.qualifiers & GlobalQualifiers.In)) {
				isInput = true;
			}
			else if (g.qualifiers & GlobalQualifiers.Out) {
				isOutput = true;
			}

			if (isInput || isOutput) {
				// rename input variables so we can also write to them

				if (OpaqueType nt = cast(OpaqueType) g.type)
					continue; // except opaque types, which must be stored in a uniform

				if (g.name in builtinGlobalSet)
					continue; // also, builtin globals don't need renaming

				Global r = new Global();
				r.loc = g.loc;
				r.qualifiers = g.qualifiers;
				r.type = g.type;
				r.name = g.name in program.renamedGlobals
					? program.renamedGlobals[g.name]
					: (program.renamedGlobals[g.name] = new Identifier());
				shader.declarations = shader.declarations[0 .. i] ~ r ~ shader.declarations[i .. $];

				g.qualifiers = GlobalQualifiers.None;

				(isInput ? renamedInputs : renamedOutputs) ~= Rename(g.name, r.name);
			}
		}
	}

	Function mainFunc = new Function();
	mainFunc.returnType = new ScalarType(POD.Void);
	mainFunc.name = new Identifier("main", true);

	foreach (r; renamedInputs) {
		AssignStmt assign = new AssignStmt();
		assign.lhs = r.from;
		assign.rhs = r.to;
		mainFunc.body ~= assign;
	}

	final switch (shader.shaderType) {
	case ShaderType.Vertex:
		Global instanceID = new Global();
		instanceID.qualifiers = GlobalQualifiers.Out | GlobalQualifiers.Flat;
		instanceID.name = getBuiltinGlobal("instanceID");
		instanceID.type = new ScalarType(POD.Int);
		shader.declarations ~= instanceID;

		AssignStmt instanceIDAssign = new AssignStmt();
		instanceIDAssign.lhs = getBuiltinGlobal("instanceID");
		instanceIDAssign.rhs = new Identifier("gl_InstanceID", true);
		mainFunc.body ~= instanceIDAssign;

		void walkVertexType(Identifier base, RefPathSegment[] path, Type type) {
			if (StructType s = cast(StructType) type) {
				foreach (field; s.declaration.fields) {
					walkVertexType(
						base,
						path ~ RefPathSegment(RefPathSegmentType.dot, field.name),
						field.type,
					);
				}
			}
			else {
				Global r = new Global();
				r.qualifiers = GlobalQualifiers.In;
				r.type = type;
				r.name = new Identifier();
				shader.declarations ~= r;

				ResolvedAssignStmt assign = new ResolvedAssignStmt();
				assign.base = base;
				assign.path = path.dup;
				assign.rhs = r.name;
				mainFunc.body ~= assign;
			}
		}

		assert(entryPoint.params.length == 1);
		Type vertexType = entryPoint.params[0].type;

		LoadOp load = new LoadOp();
		load.type = vertexType;
		load.name = new Identifier();
		mainFunc.body ~= load;

		walkVertexType(load.name, [], vertexType);

		CallOp call = new CallOp();
		call.type = entryPoint.returnType;
		call.name = new Identifier();
		call.func = entryPoint.name;
		call.args ~= load.name;
		mainFunc.body ~= call;

		DotOp glPos = new DotOp();
		glPos.type = new VectorType(POD.Float, 4);
		glPos.name = new Identifier();
		glPos.base = call.name;
		glPos.index = (cast(StructType) entryPoint.returnType).declaration.fields[0].name;
		// glPos.index = new Identifier("whatthefuck", true);
		mainFunc.body ~= glPos;

		AssignStmt glPosAssign = new AssignStmt();
		glPosAssign.lhs = new Identifier("gl_Position", true);
		glPosAssign.rhs = glPos.name;
		mainFunc.body ~= glPosAssign;

		break;
	case ShaderType.Fragment:
		CallOp call = new CallOp();
		call.type = new ScalarType(POD.Void);
		call.name = new Identifier();
		call.func = entryPoint.name;
		mainFunc.body ~= call;
		break;
	}

	foreach (r; renamedOutputs) {
		AssignStmt assign = new AssignStmt();
		assign.lhs = r.to;
		assign.rhs = r.from;
		mainFunc.body ~= assign;
	}

	shader.declarations ~= mainFunc;
	return mainFunc;
}
