//===-- Duetto/Writer.h - The Duetto JavaScript generator -----------------===//
//
//                     Duetto: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _DUETTO_WRITER_H
#define _DUETTO_WRITER_H

#include "llvm/Duetto/GlobalDepsAnalyzer.h"
#include "llvm/Duetto/Inliner.h"
#include "llvm/Duetto/NameGenerator.h"
#include "llvm/Duetto/PointerAnalyzer.h"
#include "llvm/Duetto/SourceMaps.h"
#include "llvm/Duetto/Utility.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Support/FormattedStream.h"
#include <set>
#include <map>
#include <type_traits>

namespace duetto
{

class NewLineHandler
{
friend llvm::raw_ostream& operator<<(llvm::raw_ostream& s, const NewLineHandler& handler);
private:
	SourceMapGenerator& sourceMapGenerator;
public:
	NewLineHandler(SourceMapGenerator& s):sourceMapGenerator(s)
	{
	}
};

inline llvm::raw_ostream& operator<<(llvm::raw_ostream& s, const NewLineHandler& handler)
{
	s << '\n';
	handler.sourceMapGenerator.finishLine();
	return s;
}

/**
 * Black magic to conditionally enable indented output
 */
class ostream_proxy
{
public:
	ostream_proxy( llvm::raw_ostream & s, bool readableOutput = false ) : 
		stream(s),
		readableOutput(readableOutput)
	{}

	friend ostream_proxy& operator<<( ostream_proxy & os, char c )
	{
		os.write_indent(c);
		os.stream << c;
		return os;
	}

	friend ostream_proxy& operator<<( ostream_proxy & os, llvm::StringRef s )
	{
		os.write_indent(s);
		os.stream << s;
		return os;
	}

	friend ostream_proxy& operator<<( ostream_proxy & os, const NewLineHandler& handler)
	{
		os.stream << handler;
		os.newLine = true;
		return os;
	}

	template<class T>
	friend typename std::enable_if<
		!std::is_convertible<T&&, llvm::StringRef>::value, // Use this only if T is not convertible to StringRef
		ostream_proxy&>::type operator<<( ostream_proxy & os, T && t )
	{
		os.newLine = false;
		os.stream << std::forward<T>(t);
		return os;
	}

	template<class T1, class T2>
	void write( T1 && t1, T2 && t2 )
	{
		stream.write( std::forward<T1>(t1), std::forward<T2>(t2) );
	}

private:

	static int countIndent( char c ) {
		if ( c == '{') return 1;
		else if ( c == '}') return -1;
		return 0;
	}

	static int countIndent( llvm::StringRef s) {
		int ans = 0;
		for ( char c : s ) ans+= countIndent(c);
		return ans;
	}

	template<class T>
	void write_indent(T && t) 
	{
		if ( readableOutput )
		{
			int ni = countIndent(t);
			
			if (ni < 0)
				indentLevel+=ni;
			
			if ( newLine )
			{
				for ( int i = 0; i < indentLevel; i++ )
					stream << '\t';
			}
			
			if (ni > 0)
				indentLevel+=ni;
		}
		
		newLine = false;
	}

	llvm::raw_ostream & stream;
	bool readableOutput;
	bool newLine = false;
	int indentLevel = 0;
};

class DuettoWriter
{
	llvm::Module& module;
	llvm::DataLayout targetData;
	const llvm::Function* currentFun;
	
	TypeSupport types;
	Inliner inliner;
	GlobalDepsAnalyzer globalDeps;
	NameGenerator namegen;
	DuettoPointerAnalyzer analyzer;
	std::set<const llvm::GlobalVariable *> compiledGVars;
	
	// Support for source maps
	SourceMapGenerator sourceMapGenerator;
	std::string sourceMapName;
	const NewLineHandler NewLine;

	ostream_proxy stream;

	/**
	 * \defgroup Instruction Functions used to compile an llvm::Instruction
	 * @{
	 */
	
	// COMPILE_ADD_SELF is returned by AllocaInst when a self pointer must be added to the returned value
	// COMPILE_EMPTY is returned if there is no need to add a ;\n to end the line
	enum COMPILE_INSTRUCTION_FEEDBACK { COMPILE_OK = 0, COMPILE_UNSUPPORTED, COMPILE_ADD_SELF, COMPILE_EMPTY };
	
	/**
	 * Compiles a terminator instruction
	 * 
	 * Handles:
	 *   ret, invoke, resume, br, switch, unreachable
	 * 
	 * \return true if successful, false if the terminatorinst is not supported.
	 *   In this case we print a warning message
	 */
	bool compileTerminatorInstruction(const llvm::TerminatorInst & I);
	
	/**
	 * Compiles an inlineable instruction
	 * 
	 * Handles:
	 *  load, bitcast, fptosi, fptoui, sitofp, uitofp, getelementptr, 
	 *  add, fadd, sub, fsub, zext, sdiv, udiv, srem, urem, fdiv, mul, fmul,
	 *  icmp, fcmp, and, lshr, ashr, shl, or, xor, trunc, sext, select, extractvalue,
	 *  fpext, fptrunc, ptrtoint, vaarg, 
	 * 
	 * \return true if successful, false if the instruction is not supported.
	 *   In this case we print a warning message
	 */
	bool compileInlineableInstruction(const llvm::Instruction & I);
	
	/**
	 * Compiles a not inlineable instruction
	 * 
	 * Handles:
	 *  alloca, call, landingpad, insertvalue, store.
	 * 
	 * If not forwards to compileInlineableInstruction
	 *   
	 * \pre inliner.isInlined(&I) returns false
	 * \return COMPILE_ADD_SELF if it is necessary to add a self member to the generated object.
	 */
	COMPILE_INSTRUCTION_FEEDBACK compileNotInlineableInstruction(const llvm::Instruction & I);
	
	/**
	 * Compile a argument list, wrapped within '(' and ')'.
	 * 
	 * \param it,itE the range of arguments to be compiled.
	 * \param f If not null, the called function. This is used in direct calls to pass non-regular pointers.
	 * 
	 */
	void compileMethodArgs(const llvm::User::const_op_iterator it, const llvm::User::const_op_iterator itE, const llvm::Function * f = nullptr);
	
	/** @} */
	
	/**
	 * \defgroup Opcodes Function used to compile arithmetic instructions
	 * @{
	 * 
	 * These functions are implemented in Opcodes.cpp
	 */
	
	void compileSignedInteger(const llvm::Value* v);
	void compileUnsignedInteger(const llvm::Value* v);
	void compilePredicate(llvm::CmpInst::Predicate p);
	void compileOperandForIntegerPredicate(const llvm::Value* v, llvm::CmpInst::Predicate p);
	void compileIntegerComparison(const llvm::Value* lhs, const llvm::Value* rhs, llvm::CmpInst::Predicate p);
	void compilePtrToInt(const llvm::Value* v);
	void compileSubtraction(const llvm::Value* lhs, const llvm::Value* rhs);
	
	/** @} */
	
	/**
	 * \defgroup Memfuncs Functions used to implement memcpy/malloc et similia
	 * @{
	 */
	
	void compileAllocation(const DynamicAllocInfo & info);
	void compileMove(const llvm::Value* dest, const llvm::Value* src, const llvm::Value* size);
	
	enum COPY_DIRECTION { FORWARD=0, BACKWARD, RESET };
	
	void compileMemFunc(const llvm::Value* dest, const llvm::Value* src, const llvm::Value* size,
			COPY_DIRECTION copyDirection);
	
	void compileCopyRecursive(const std::string& baseName, const llvm::Value* baseDest,
		const llvm::Value* baseSrc, llvm::Type* currentType, const char* namedOffset);
	
	void compileResetRecursive(const std::string& baseName, const llvm::Value* baseDest,
		const llvm::Value* resetValue, llvm::Type* currentType, const char* namedOffset);

	void compileFree(const llvm::Value* obj);
	
	/** @} */
	
	/**
	 * \defgroup Pointers Functions used to compile pointers operations
	 */

	/**
	 * Compiles a pointer operand using the given kind.
	 * 
	 * This function can syntetize a COMPLETE_OBJECT, COMPLETE_ARRAY or REGULAR
	 * no matter what the actual kind of the passed value is, provided that such
	 * demotion/promotion is actually possible.
	 * 
	 * \pre v->getType()->isPointerTy();
	 */
	void compilePointer(const llvm::Value* v, POINTER_KIND kind);
	
	enum COMPILE_FLAG { NORMAL = 0, DRY_RUN = 1, GEP_DIRECT = 2 };

	llvm::Type* compileObjectForPointer(const llvm::Value* val, COMPILE_FLAG flag);
	void compileDereferencePointer(const llvm::Value* v, const llvm::Value* offset, const char* namedOffset = NULL);
	bool compileOffsetForPointer(const llvm::Value* val, llvm::Type* lastType);
	llvm::Type* compileObjectForPointerGEP(const llvm::Value* val, const llvm::Use* it,
			const llvm::Use* const itE, COMPILE_FLAG flag);
	bool compileOffsetForPointerGEP(const llvm::Value* val, const llvm::Use* it, const llvm::Use* const itE,
			llvm::Type* lastType);	
	
	void compileGEP(const llvm::Value* val, const llvm::Use* it, const llvm::Use* const itE);

	void compileEqualPointersComparison(const llvm::Value* lhs, const llvm::Value* rhs, llvm::CmpInst::Predicate p);

	llvm::Type* compileRecursiveAccessToGEP(llvm::Type* curType, const llvm::Use* it,
			const llvm::Use* const itE, COMPILE_FLAG flag);

	/** @} */
	
	/**
	 * \defgroup GlobalVars Functions used to compile functions and global variables
	 * 
	 * These are the top level functions, called directly from makeJS
	 * @{
	 */
	
	void compileMethod(const llvm::Function& F);
	void compileGlobal(const llvm::GlobalVariable& G);
	void compileNullPtrs();
	void compileCreateClosure();
	void compileHandleVAArg();	

	/** @} */
	
	/**
	 * \defgroup Constant Functions used to compile constants and constantexpr
	 * @{
	 */

	void compileConstantExpr(const llvm::ConstantExpr* ce);
	void compileConstant(const llvm::Constant* c );

	/** @} */
	
	/** 
	 * \defgroup Typefunc Functions used to compile classes and typed arrays, and implement inheritance
	 * 
	 * These functions are implemented in Types.cpp
	 * @{
	 */

	enum COMPILE_TYPE_STYLE { LITERAL_OBJ=0, THIS_OBJ };

	void compileTypedArrayType(llvm::Type* t);
	void compileTypeImpl(llvm::Type* t, COMPILE_TYPE_STYLE style);	
	void compileType(llvm::Type* t, COMPILE_TYPE_STYLE style);
	uint32_t compileClassTypeRecursive(const std::string& baseName, llvm::StructType* currentType, uint32_t baseCount);
	void compileClassType(llvm::StructType* T);
	void compileArrayClassType(llvm::StructType* T);
	void compileArrayPointerType();	
	
	/** @} */
	
	/**
	 * \defgroup JSinterop Functions used to support interoperability with JS.
	 * 
	 * These functions are implemented in JSInterop.cpp
	 * @{
	 */
	
	void compileClassesExportedToJs();
	void handleBuiltinNamespace( llvm::ImmutableCallSite callV );

	/** @} */
	
	// Extra stuff not yet categorized
	COMPILE_INSTRUCTION_FEEDBACK handleBuiltinCall(llvm::ImmutableCallSite callV);
	void compileBB(const llvm::BasicBlock& BB, const std::map<const llvm::BasicBlock*, uint32_t>& blocksMap);
	void compileDowncast( llvm::ImmutableCallSite );
	void compileOperand(const llvm::Value* v, POINTER_KIND requestedPointerKind = UNDECIDED);
	void compileOperandImpl(const llvm::Value* v);
	void compilePHIOfBlockFromOtherBlock(const llvm::BasicBlock* to, const llvm::BasicBlock* from);
	void addSelfPointer(const llvm::Value* obj);
	
	friend class DuettoRenderInterface;
	
	static uint32_t getMaskForBitWidth(int width)
	{
		return (1 << width) - 1;
	}
	
public:
	DuettoWriter(llvm::Module& module, 
		     llvm::raw_ostream& stream, 
		     llvm::AliasAnalysis& AA,
		     const std::string& sourceMapName,
		     llvm::raw_ostream* sourceMap,
		     bool prettyOutput) :
		
		module(module),
		targetData( &module ),
		currentFun(nullptr),
		
		types(module),
		inliner(AA), 
		globalDeps(module), 
		namegen( globalDeps, inliner, prettyOutput), 
		analyzer( namegen, globalDeps.classesWithBaseInfo() ), 
		
		sourceMapGenerator(sourceMap,module.getContext()),
		sourceMapName(sourceMapName),
		NewLine(sourceMapGenerator),
		
		stream(stream, prettyOutput)
	{
	}

	void makeJS();
};

}
#endif
