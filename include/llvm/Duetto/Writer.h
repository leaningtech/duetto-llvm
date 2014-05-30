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

llvm::raw_ostream& operator<<(llvm::raw_ostream& s, const NewLineHandler& handler);

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
private:

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

	void compileTypedArrayType(llvm::Type* t);
	
	// COMPILE_ADD_SELF is returned by AllocaInst when a self pointer must be added to the returned value
	// COMPILE_EMPTY is returned if there is no need to add a ;\n to end the line
	enum COMPILE_INSTRUCTION_FEEDBACK { COMPILE_OK = 0, COMPILE_UNSUPPORTED, COMPILE_ADD_SELF, COMPILE_EMPTY };
	COMPILE_INSTRUCTION_FEEDBACK compileTerminatorInstruction(const llvm::TerminatorInst& I);
	COMPILE_INSTRUCTION_FEEDBACK compileTerminatorInstruction(const llvm::TerminatorInst& I,
			const std::map<const llvm::BasicBlock*, uint32_t>& blocksMap);
	bool compileInlineableInstruction(const llvm::Instruction& I);
	void addSelfPointer(const llvm::Value* obj);
	COMPILE_INSTRUCTION_FEEDBACK compileNotInlineableInstruction(const llvm::Instruction& I);
	enum COMPILE_FLAG { NORMAL = 0, DRY_RUN = 1, GEP_DIRECT = 2 };
	llvm::Type* compileRecursiveAccessToGEP(llvm::Type* curType, const llvm::Use* it,
			const llvm::Use* const itE, COMPILE_FLAG flag);
	void compilePredicate(llvm::CmpInst::Predicate p);
	void compileOperandForIntegerPredicate(const llvm::Value* v, llvm::CmpInst::Predicate p);
	void compileEqualPointersComparison(const llvm::Value* lhs, const llvm::Value* rhs, llvm::CmpInst::Predicate p);
	void compileIntegerComparison(const llvm::Value* lhs, const llvm::Value* rhs, llvm::CmpInst::Predicate p);
	void compilePtrToInt(const llvm::Value* v);
	void compileSubtraction(const llvm::Value* lhs, const llvm::Value* rhs);
	enum COMPILE_TYPE_STYLE { LITERAL_OBJ=0, THIS_OBJ };
	void compileType(llvm::Type* t, COMPILE_TYPE_STYLE style);
	void compileTypeImpl(llvm::Type* t, COMPILE_TYPE_STYLE style);
	
	/*
	 * \param v The pointer to dereference, it may be a regular pointer, a complete obj or a complete array
	 * \param offset An offset coming from code, which may be also NULL
	 * \param namedOffset An offset that will be added verbatim to the code
	 */
	void compileDereferencePointer(const llvm::Value* v, const llvm::Value* offset, const char* namedOffset = NULL);
	void compileGEP(const llvm::Value* val, const llvm::Use* it, const llvm::Use* const itE);
	llvm::Type* compileObjectForPointerGEP(const llvm::Value* val, const llvm::Use* it,
			const llvm::Use* const itE, COMPILE_FLAG flag);
	bool compileOffsetForPointerGEP(const llvm::Value* val, const llvm::Use* it, const llvm::Use* const itE,
			llvm::Type* lastType);
	llvm::Type* compileObjectForPointer(const llvm::Value* val, COMPILE_FLAG flag);
	/*
	 * Returns true if anything is printed
	 */
	bool compileOffsetForPointer(const llvm::Value* val, llvm::Type* lastType);
	void compileMove(const llvm::Value* dest, const llvm::Value* src, const llvm::Value* size);
	enum COPY_DIRECTION { FORWARD=0, BACKWARD, RESET };
	void compileMemFunc(const llvm::Value* dest, const llvm::Value* src, const llvm::Value* size,
			COPY_DIRECTION copyDirection);
	void compileCopyRecursive(const std::string& baseName, const llvm::Value* baseDest,
		const llvm::Value* baseSrc, const llvm::Type* currentType, const char* namedOffset);
	void compileResetRecursive(const std::string& baseName, const llvm::Value* baseDest,
		const llvm::Value* resetValue, const llvm::Type* currentType, const char* namedOffset);
	void compileDowncast(const llvm::Value* src, uint32_t baseOffset);
	void compileAllocation(const DynamicAllocInfo & info);
	void compileFree(const llvm::Value* obj);
	void compilePointer(const llvm::Value* v, POINTER_KIND acceptedKind);
	void compileOperandImpl(const llvm::Value* v);
	enum NAME_KIND { LOCAL=0, GLOBAL=1 };
	void compileMethodArgs(const llvm::User::const_op_iterator it, const llvm::User::const_op_iterator itE, const llvm::Function * = nullptr);
	void handleBuiltinNamespace(const char* ident, const llvm::Function* calledFunction,
			llvm::User::const_op_iterator it, llvm::User::const_op_iterator itE);
	COMPILE_INSTRUCTION_FEEDBACK handleBuiltinCall(llvm::ImmutableCallSite callV);
	void compileMethod(const llvm::Function& F);
	void compileGlobal(const llvm::GlobalVariable& G);
	uint32_t compileClassTypeRecursive(const std::string& baseName, llvm::StructType* currentType, uint32_t baseCount);
	void compileClassType(llvm::StructType* T);
	void compileArrayClassType(llvm::StructType* T);
	void compileArrayPointerType();
	void compileCreateClosure();
	void compileHandleVAArg();
	void compileConstantExpr(const llvm::ConstantExpr* ce);
	void compileNullPtrs();
	static uint32_t getMaskForBitWidth(int width);
	void compileSignedInteger(const llvm::Value* v);
	void compileUnsignedInteger(const llvm::Value* v);
	//JS interoperability support
	void compileClassesExportedToJs();
public:
	ostream_proxy stream;
	DuettoWriter(llvm::Module& m, llvm::raw_ostream& s, llvm::AliasAnalysis& AA,
		const std::string& sourceMapName, llvm::raw_ostream* sourceMap, bool prettyOutput):
		module(m),targetData(&m),currentFun(NULL), types(m), inliner(AA), globalDeps(m), namegen( globalDeps, inliner, prettyOutput), analyzer( namegen ), 
		sourceMapGenerator(sourceMap,m.getContext()),sourceMapName(sourceMapName),NewLine(sourceMapGenerator),
		stream(s, prettyOutput)
	{
	}
	void makeJS();
	void compileBB(const llvm::BasicBlock& BB, const std::map<const llvm::BasicBlock*, uint32_t>& blocksMap);
	void compileOperand(const llvm::Value* v, POINTER_KIND requestedPointerKind = UNDECIDED);
	void compileConstant(const llvm::Constant* c);
	void compilePHIOfBlockFromOtherBlock(const llvm::BasicBlock* to, const llvm::BasicBlock* from);
};

}
#endif
