//===-- PointerAnalyzer.cpp - The Duetto JavaScript generator -------------===//
//
//                     Duetto: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include <iomanip>
#include <sstream>
#include "llvm/Duetto/PointerAnalyzer.h"
#include "llvm/Duetto/Utility.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/GlobalAlias.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/Support/FormattedStream.h"

using namespace llvm;

namespace duetto {

/*
 * The map is used to handle cyclic PHI nodes
 */
POINTER_KIND DuettoPointerAnalyzer::getPointerKind(const Value* v) const
{
	assert(v->getType()->isPointerTy());

	pointer_kind_map_t::iterator iter = pointerKindMap.find(v);

	if (pointerKindMap.end() != iter)
		return iter->second;
	iter = pointerKindMap.insert( std::make_pair(v, UNDECIDED) ).first;
	
#ifdef DUETTO_DEBUG_POINTERS
	debugAllPointersSet.insert(v);
#endif
 
	PointerType * pt = cast<PointerType>(v->getType());
	if(TypeSupport::isClientArrayType(pt->getElementType()))
	{
		//Handle client arrays like COMPLETE_ARRAYs, so the right 0 offset
		//is used when doing GEPs
		return iter->second = COMPLETE_ARRAY;
	}
	if(TypeSupport::isClientType(pt->getElementType()))
	{
		//Pointers to client type are complete objects, and are never expanded to
		//regular ones since an array of client objects does not exists.
		//NOTE: An array of pointer to client objects exists, not an array of objects.
		return iter->second = COMPLETE_OBJECT;
	}
	if(AllocaInst::classof(v) || GlobalVariable::classof(v))
	{
		if(TypeSupport::isImmutableType(pt->getElementType()) &&  needsWrappingArray(v) )
			return iter->second = COMPLETE_ARRAY;
		else
			return iter->second = COMPLETE_OBJECT;
	}
	//Follow bitcasts
	if(isBitCast(v))
	{
		const User* bi = cast<User>(v);
		//Casts from unions return regular pointers
		if(TypeSupport::isUnion(bi->getOperand(0)->getType()->getPointerElementType()))
		{
			//Special case arrays
			if(ArrayType::classof(pt->getElementType()))
				return iter->second = COMPLETE_OBJECT;
			else
				return iter->second = COMPLETE_ARRAY;
		}
		return iter->second = getPointerKind(bi->getOperand(0));
	}
	if(isNopCast(v))
	{
		const User* bi=cast<User>(v);
		return iter->second = getPointerKind(bi->getOperand(0));
	}
	//Follow select
	if(const SelectInst* s=dyn_cast<SelectInst>(v))
	{
		POINTER_KIND k1 = getPointerKind(s->getTrueValue());
		if(k1 == REGULAR)
			return iter->second = REGULAR;
		
		POINTER_KIND k2 = getPointerKind(s->getFalseValue());
		//If the type is different we need to collapse to REGULAR
		if(k1!=k2)
			return iter->second = REGULAR;
		//The type is the same
		return iter->second = k1;
	}
	if (DynamicAllocInfo::getAllocType(v) != DynamicAllocInfo::not_an_alloc )
		return iter->second = COMPLETE_ARRAY;

	if (isa<PHINode>(v) || isa<Argument>(v))
	{
		if (TypeSupport::isImmutableType( pt->getElementType() ) )
		{
			return iter->second = needsWrappingArray(v)? REGULAR : COMPLETE_OBJECT;
		}
		else
		{
			return iter->second = (getPointerUsageFlagsComplete(v) & need_self_flags) ? REGULAR : COMPLETE_OBJECT;
		}
	}
	
	if (const LoadInst * li = dyn_cast<LoadInst>(v) )
	{
		if (const AllocaInst * ai = dyn_cast<AllocaInst>(li->getPointerOperand() ) )
			return iter->second = getPointerKindForAlloca(ai);
	}
	return iter->second = REGULAR;
}

POINTER_KIND DuettoPointerAnalyzer::getPointerKindForAlloca(const AllocaInst * ai) const
{
	if (getPointerKind(ai) != COMPLETE_OBJECT)
		return REGULAR;
	
	Type * pt = ai->getType()->getElementType();
	
	if ( !pt->isPointerTy() )
		return REGULAR;
	
	bool allUsesAreStoresOrLoad = std::all_of( ai->use_begin(), ai->use_end(), [](const Use & u )
	{
		return isa<LoadInst>(u.getUser()) || ( isa<StoreInst>(u.getUser()) && u.getOperandNo() == 0 );
	});
	
	return allUsesAreStoresOrLoad ? COMPLETE_OBJECT : REGULAR;
}

bool DuettoPointerAnalyzer::hasSelfMember(const Value* v) const
{
	assert( TypeSupport::findRealType(v)->isPointerTy() );
	
	PointerType * tp = cast<PointerType>(TypeSupport::findRealType(v) );

	if ( TypeSupport::isImmutableType(tp->getElementType()) || !isa<StructType>(tp->getElementType()) )
		return false;
	
	if ( classesWithBaseInfo.count( cast<StructType>( tp->getElementType() ) ) )
		return false;

	return (getPointerUsageFlagsComplete(v) & need_self_flags);
}

#ifndef NDEBUG
void DuettoPointerAnalyzer::dumpPointer(const Value* v) const
{
	std::ostringstream fmt;
	fmt << std::setw(96) << std::left;

	{
		std::ostringstream tmp;
		tmp << namegen.getName(v).data();
		
		if (const Argument * arg = dyn_cast<Argument>(v))
		{
			tmp << " arg of function: " << arg->getParent()->getName().data();
		}
		else if (const Instruction * inst = dyn_cast<Instruction>(v) )
		{
			tmp << " in function: " << inst->getParent()->getParent()->getName().data();
		}
		
		fmt << tmp.str();
	}
	
	if (v->getType()->isPointerTy())
	{
		fmt << std::setw(18) << std::left;
		switch (getPointerKind(v))
		{
			case COMPLETE_OBJECT: fmt << "COMPLETE_OBJECT"; break;
			case COMPLETE_ARRAY: fmt << "COMPLETE_ARRAY"; break;
			case REGULAR: fmt << "REGULAR"; break;
			default: fmt << "UNDECIDED"; break;
		}
		
		fmt << std::setw(18) << std::left << getPointerUsageFlags(v);
		fmt << std::setw(18) << std::left << getPointerUsageFlagsComplete(v);
		fmt << std::setw(18) << std::left << std::boolalpha << TypeSupport::isImmutableType( v->getType()->getPointerElementType() );
	}
	else
		fmt << "Is not a pointer";

	llvm::errs() << fmt.str() << "\n";
}
#endif

#ifdef DUETTO_DEBUG_POINTERS

void DuettoPointerAnalyzer::dumpAllPointers() const
{
	llvm::errs() << "Dumping all pointers\n";
	
	llvm::errs() << "Name" << std::string(92,' ') << "Kind              UsageFlags        UsageFlagsComplete IsImmutable?\n";
	
	for (auto ptr : debugAllPointersSet)
		dumpPointer(ptr);
}

void DuettoPointerAnalyzer::dumpAllFunctions() const
{
	llvm::errs() << "Dumping functions:\n";

	for (auto f : debugAllFunctionsSet)
	{
		llvm::errs() << f->getName();
		if (canBeCalledIndirectly(f))
		{
			llvm::errs() << " called indirectly";
		}
		llvm::errs() << "\n";
	}
}

#endif //DUETTO_DEBUG_POINTERS

bool DuettoPointerAnalyzer::needsWrappingArray(const Value* v) const
{
	assert( v->getType()->isPointerTy() );
	assert( TypeSupport::isImmutableType(cast<PointerType>( v->getType() )->getElementType() ) );
	
	bool hasAliasedStore = std::any_of( v->use_begin(), v->use_end(), [&]( const Use & u )
	{
		const User * U = u.getUser();
		if (isBitCast(U) || isNopCast(U) || isa<PHINode>(U) || isa<SelectInst>(U) )
		{
			if ( getPointerUsageFlagsComplete(U) & POINTER_NONCONST_DEREF )
				return true;
		}
		else if (isa<CallInst>(U) || isa<InvokeInst>(U) )
		{
			std::set<const Value *> openset;
			if ( usageFlagsForCall(v, ImmutableCallSite(U), openset ) & POINTER_NONCONST_DEREF )
				return true;
		}
		return false;
	});
	
	return hasAliasedStore || (getPointerUsageFlagsComplete(v) & need_wrap_array_flags ) ||
		( (isa<Argument>(v) || isa<PHINode>(v) ) && (getPointerUsageFlagsComplete(v) & POINTER_NONCONST_DEREF) );
}

uint32_t DuettoPointerAnalyzer::getPointerUsageFlagsComplete(const Value * v) const
{
	assert(v->getType()->isPointerTy());

	pointer_usage_map_t::const_iterator iter = pointerCompleteUsageMap.find(v);

	if (pointerCompleteUsageMap.end() == iter)
	{
		std::set<const Value *> openset;
		iter = pointerCompleteUsageMap.insert( std::make_pair(v,dfsPointerUsageFlagsComplete(v, openset) ) ).first;
	}

	return iter->second;
}

uint32_t DuettoPointerAnalyzer::getPointerUsageFlags(const llvm::Value * v) const
{
	assert(v->getType()->isPointerTy());

	pointer_usage_map_t::const_iterator iter = pointerUsageMap.find(v);

	if (iter == pointerUsageMap.end())
	{
		uint32_t ans = 0;
		for (Value::const_use_iterator it = v->use_begin(); it != v->use_end(); ++it)
		{
			const User* U = it->getUser();
			// Check if the pointer "v" is used as "ptr" for a StoreInst. 
			if (const StoreInst * I = dyn_cast<StoreInst>(U) )
			{
				if (I->getPointerOperand() == v)
					ans |= POINTER_NONCONST_DEREF; 
			}
			
			// Check if the pointer "v" is used as lhs or rhs of a comparison operation
			else if (const CmpInst * I = dyn_cast<CmpInst>(U) )
			{
				if (!I->isEquality())
					ans |= POINTER_ORDINABLE;
				else
					ans |= POINTER_EQUALITY_COMPARABLE;
			}
			
			// Check if the pointer is casted to int
			else if (isa<PtrToIntInst>(U) )
			{
				ans |= POINTER_CASTABLE_TO_INT;
			}
			
			// Pointer used as a base to a getElementPtr
			else if (isGEP(U) )
			{
				const User * I = cast<User>(U);
  				const ConstantInt * p = dyn_cast<ConstantInt>(I->getOperand(1));
				if (!p || !p->isZero())
					ans |= POINTER_ARITHMETIC;
			}
			/** TODO deal with all use cases and remove the following 2 blocks **/
			else if (
				isa<PHINode>(U) || 
				isa<SelectInst>(U) ||
				isa<LoadInst>(U) ||
				isa<CallInst>(U) ||
				isa<InvokeInst>(U) ||
				isa<ReturnInst>(U) || 
				isa<GlobalValue>(U) ||
				isa<ConstantArray>(U) ||
				isa<ConstantStruct>(U) ||
				isBitCast(U) ||
				isNopCast(U) )
			{
				continue;
			}
			else
			{
#ifdef DUETTO_DEBUG_POINTERS
				llvm::errs() << "Adding POINTER_UNKNOWN in getPointerUsageFlags due to instruction: " << valueObjectName(U) << "\n";
#endif //DUETTO_DEBUG_POINTERS
				ans |= POINTER_UNKNOWN;
			}
		}
		
		iter = pointerUsageMap.insert( std::make_pair(v, ans) ).first;
	}

	return iter->second;
}

uint32_t DuettoPointerAnalyzer::dfsPointerUsageFlagsComplete(const Value * v, std::set<const Value *> & openset) const
{
	if ( !openset.insert(v).second )
	{
		return 0;
	}

	uint32_t f = getPointerUsageFlags(v);

	for (Value::const_use_iterator it = v->use_begin(); it != v->use_end(); ++it)
	{
		const User * U = it->getUser();
		// Check if "v" is used as a operand in a phi node
		if (isa<PHINode>(U) ||
			isa<SelectInst>(U) ||
			isBitCast(U) ||
			isNopCast(U))
		{
			f |= dfsPointerUsageFlagsComplete(U, openset);
		}
		else if (const CallInst * I = dyn_cast<CallInst>(U))
		{
			// Indirect calls require a finer analysis
			f |= usageFlagsForCall(v,I,openset);
		}
		else if (const InvokeInst * I = dyn_cast<InvokeInst>(U))
		{
			f |= usageFlagsForCall(v,I,openset);
		}
		else if (isa<ReturnInst>(U))
		{
			//TODO deal with me properly
			f |= POINTER_UNKNOWN;
		}
		else if (const StoreInst * I = dyn_cast<StoreInst>(U) )
		{
			if (I->getValueOperand() == v)
			{
				// Tracking the stores is almost impossible
				/** But we can do this - in a conservative way - by checking the type of the pointed object.
				 * This should be implemented in future
				 */
				f |= POINTER_UNKNOWN;
			}
		}
		else if (
			isa<ConstantStruct>(U) ||
			isa<ConstantArray>(U) ||
			isa<GlobalValue>(U) )
		{
			f |= POINTER_UNKNOWN;
		}
		else if ( // Things we know are ok
			isa<CmpInst>(U) ||
			isa<LoadInst>(U) ||
			isa<PtrToIntInst>(U) ||
			isGEP(U) )
		{
			continue;
		}
		else
		{
#ifdef DUETTO_DEBUG_POINTERS
			llvm::errs() << "Adding POINTER_UNKNOWN in dfsPointerUsageFlagsComplete due to instruction: " << valueObjectName(U) << "\n";
#endif //DUETTO_DEBUG_POINTERS

			f |= POINTER_UNKNOWN;
		}
	}
	
	return f;
}

uint32_t DuettoPointerAnalyzer::usageFlagsForCall(const Value * v, ImmutableCallSite I, std::set<const Value *> & openset) const
{
	const Function * f = I.getCalledFunction();

	if ( !f || canBeCalledIndirectly(f) || f->isVarArg())
		return POINTER_UNKNOWN;

	assert( f->arg_size() == I.arg_size() );
	
	uint32_t flags = 0;
	Function::const_arg_iterator iter = f->arg_begin();

	for (unsigned int argNo = 0; iter != f->arg_end(); ++iter, ++argNo)
		if ( I.getArgument(argNo) == v )
			flags |= dfsPointerUsageFlagsComplete( iter, openset );

#ifndef NDEBUG
	bool ok = false;
	for (unsigned int argNo = 0; argNo < I.arg_size(); ++argNo)
		if ( I.getArgument(argNo) == v )
			ok = true;
	assert(  ok || ((llvm::errs() << f->getName()),false) );
#endif
	
	return flags;
}

}
