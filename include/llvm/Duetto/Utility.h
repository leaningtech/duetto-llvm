//===-- Duetto/Utility.h - Duetto common routines -------------------------===//
//
//                     Duetto: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2013 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _DUETTO_UTILITY_H
#define _DUETTO_UTILITY_H

#include <set>
#include "llvm/IR/CallSite.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"

namespace duetto
{

bool isNopCast(const llvm::Value* val);
bool isValidVoidPtrSource(const llvm::Value* val, std::set<const llvm::PHINode*>& visitedPhis);

inline bool isValidVoidPtrSource(const llvm::Value* val)
{
	std::set<const llvm::PHINode*> visitedPhis;
	return isValidVoidPtrSource(val, visitedPhis);
}

bool isBitCast(const llvm::Value* v);
bool isGEP(const llvm::Value* v);
uint32_t getIntFromValue(const llvm::Value* v);

// Printable name of the llvm type - useful only for debugging
std::string valueObjectName(const llvm::Value * v);

class TypeSupport
{
public:
	TypeSupport( const llvm::Module & module ) : module(module) {}
	
	static bool isValidTypeCast(const llvm::Value * castOp, llvm::Type * dstPtr);
	
	static bool isClientGlobal(const llvm::GlobalValue & v)
	{
		return v.hasName() && (
			v.getName().startswith("_ZN6client")  ||
			v.getName().startswith("_ZNK6client") );
	}
	
	static bool isClientType(llvm::Type* t)
	{
		return (t->isStructTy() && llvm::cast<llvm::StructType>(t)->hasName() && t->getStructName().startswith("class._ZN6client") );
	}

	static bool isClientArrayType(llvm::Type* t)
	{
		return (t->isStructTy() && llvm::cast<llvm::StructType>(t)->hasName() && t->getStructName().startswith("class._ZN6client5ArrayE") );
	}
	
	static bool isI32Type(llvm::Type* t)
	{
		return t->isIntegerTy() && llvm::cast<llvm::IntegerType>(t)->getBitWidth()==32;
	}

	static bool isTypedArrayType(llvm::Type* t)
	{
		return t->isIntegerTy(8) || t->isIntegerTy(16) || t->isIntegerTy(32) ||
			t->isFloatTy() || t->isDoubleTy();
	}

	static bool isImmutableType(llvm::Type* t)
	{
		if(t->isIntegerTy() || t->isFloatTy() || t->isDoubleTy() || t->isPointerTy())
			return true;
		return false;
	}

	static bool isUnion(llvm::Type* t)
	{
		return (t->isStructTy() && llvm::cast<llvm::StructType>(t)->hasName() &&
			t->getStructName().startswith("union."));

	}

	bool hasBasesInfo(const llvm::StructType* t) const
	{
		return getBasesMetadata(t) != nullptr;
	}

	bool getBasesInfo(llvm::StructType* t, uint32_t& firstBase, uint32_t& baseCount) const;

private:
	const llvm::NamedMDNode* getBasesMetadata(const llvm::StructType * t) const;
	static bool safeCallForNewedMemory(const llvm::CallInst* ci);

	const llvm::Module & module;
};

/*
 * Provide information about a malloc/calloc/etc call
 */
class DynamicAllocInfo
{
public:
	enum AllocType
	{
		not_an_alloc,
		malloc,
		calloc,
		duetto_allocate,
		opnew, // operator new(unsigned int)
		opnew_array // operator new[](unsigned int)
	};
	
	/**
	 * This constructor works with any instruction.
	 * 
	 * If the passed argument is not a call, or is not an alloc,
	 * isValidAlloc will return false. In this case any other
	 * use of this object is not permitted.
	 */
	DynamicAllocInfo(llvm::ImmutableCallSite);
	
	bool isValidAlloc() const { return type != not_an_alloc; }
	
	AllocType getAllocType() const { return type; }
	
	static AllocType getAllocType(llvm::ImmutableCallSite);
	
	/**
	 * Every alloc instruction produces an i8*.
	 * This function tries to understand how the result of an alloc
	 * is used, and deduce the actual used type of the allocation.
	 * 
	 * Will report an llvm error if the use of the result is not consistent
	 */
	llvm::PointerType * getCastedType() const { return castedType; }
	
	/**
	 * This argument will never be null
	 */
	const llvm::Value * getByteSizeArg() const;
	
	/**
	 * This can be null if getAllocType() == calloc
	 */
	const llvm::Value * getNumberOfElementsArg() const;
	
	/**
	 * Check if the size of the allocation is known only at runtime
	 */
	bool sizeIsRuntime() const;
	
	/**
	 * Check if the allocation should use a createArray function
	 */
	bool useCreateArrayFunc() const;
	
	/**
	 * Check if the allocation should use a createTypedArray function
	 */
	bool useCreatePointerArrayFunc() const;
	
	/**
	 * Check if the allocation should use typed arrays
	 */
	bool useTypedArray() const;

private:
	llvm::PointerType * computeCastedType() const;
	
	llvm::ImmutableCallSite call;
	AllocType type;
	llvm::PointerType * castedType;
};
}

#endif //_DUETTO_UTILITY_H

