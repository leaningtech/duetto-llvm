//===-- Pointers.cpp - The Duetto JavaScript C pointers implementation ----===//
//
//                     Duetto: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Duetto/Utils.h"
#include "llvm/Duetto/Writer.h"
#include <llvm/IR/Operator.h>
#include <iomanip>
#include <sstream>

using namespace llvm;
using namespace duetto;

#ifdef DUETTO_DEBUG_POINTERS
static void print_debug_pointer_uknown(const llvm::Value * v, const llvm::User * u,const char * f)
{
	llvm::errs() << "Adding POINTER_UNKNOWN in \"" << f << "\", pointer: " << v->getName() << " being used by: ";

	if (const Instruction * p = dyn_cast<const Instruction>(u) )
		llvm::errs() << " instruction " << p->getOpcodeName() << "\n";
	else if (const Constant * p = dyn_cast<const Constant>(u) )
	{
		llvm::errs() << " constant " << p->getName() << "(";
		
		// Feel free to find a way to avoid this obscenity
		if (isa<const BlockAddress>(p))
			llvm::errs() << "BlockAddress";
		else if (isa<const ConstantAggregateZero>(p))
			llvm::errs() << "ConstantAggregateZero";
		else if (isa<const ConstantArray>(p))
			llvm::errs() << "ConstantArray";
		else if (isa<const ConstantDataSequential>(p))
			llvm::errs() << "ConstantDataSequential";
		else if (const ConstantExpr * pc = dyn_cast<const ConstantExpr>(p))
		{
			llvm::errs() << "ConstantExpr [" << pc->getOpcodeName() <<"]";
		}
		else if (isa<const ConstantFP>(p))
			llvm::errs() << "ConstantFP";
		else if (isa<const ConstantInt>(p))
			llvm::errs() << "ConstantInt";
		else if (isa<const ConstantPointerNull>(p))
			llvm::errs() << "ConstantPointerNull";
		else if (isa<const ConstantStruct>(p))
			llvm::errs() << "ConstantStruct";
		else if (isa<const ConstantVector>(p))
			llvm::errs() << "ConstantVector";
		else if (isa<const GlobalValue>(p))
			llvm::errs() << "GlobalValue";
		else if (isa<const UndefValue>(p))
			llvm::errs() << "UndefValue";
		else
			llvm::errs() << "Unknown";
		llvm::errs() << ")\n";
		
		llvm::errs() << "Object dump: "; p->dump(); llvm::errs() << "\n";
		llvm::errs() << "Of type: "; p->getType()->dump(); llvm::errs() << "\n";
		llvm::errs() << "\n";
	}
	else if (const Operator * p = dyn_cast<const Operator>(u) )
		llvm::errs() << " operator " << p->getName() << "\n";
}

bool DuettoWriter::printPointerInfo(const Value * v)
{
	std::ostringstream fmt;
	fmt << std::setw(96) << std::left;

	if (v->hasName())
		fmt << v->getName().data();
	else
	{
		std::ostringstream tmp;
		tmp << "tmp" << getUniqueIndexForValue(v);
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
		fmt << std::setw(18) << std::left << std::boolalpha << isImmutableType( v->getType()->getPointerElementType() );
	}
	else
		fmt << "Is not a pointer";

	llvm::errs() << fmt.str() << "\n";
	return false;
}

#endif //DUETTO_DEBUG_POINTERS

/*
 * The map is used to handle cyclic PHI nodes
 */
DuettoWriter::POINTER_KIND DuettoWriter::dfsPointerKind(const Value* v, std::map<const PHINode*, POINTER_KIND>& visitedPhis) const
{
#ifdef DUETTO_DEBUG_POINTERS
	debugAllPointersSet.insert(v);
#endif
	assert(v->getType()->isPointerTy());
	PointerType* pt=cast<PointerType>(v->getType());
	if(isClientArrayType(pt->getElementType()))
	{
		//Handle client arrays like COMPLETE_ARRAYs, so the right 0 offset
		//is used when doing GEPs
		return COMPLETE_ARRAY;
	}
	if(isClientType(pt->getElementType()))
	{
		//Pointers to client type are complete objects, and are never expanded to
		//regular ones since an array of client objects does not exists.
		//NOTE: An array of pointer to client objects exists, not an array of objects.
		return COMPLETE_OBJECT;
	}
	if(AllocaInst::classof(v) || GlobalVariable::classof(v))
	{
		if(isImmutableType(pt->getElementType()) && !isNoWrappingArrayOptimizable(v))
			return COMPLETE_ARRAY;
		else
			return COMPLETE_OBJECT;
	}
	//Follow bitcasts
	if(isBitCast(v))
	{
		const User* bi=static_cast<const User*>(v);
		//Casts from unions return regular pointers
		if(isUnion(bi->getOperand(0)->getType()->getPointerElementType()))
		{
			//Special case arrays
			if(ArrayType::classof(pt->getElementType()))
				return COMPLETE_OBJECT;
			else
				return COMPLETE_ARRAY;
		}
		return dfsPointerKind(bi->getOperand(0), visitedPhis);
	}
	if(isNopCast(v))
	{
		const User* bi=static_cast<const User*>(v);
		return dfsPointerKind(bi->getOperand(0), visitedPhis);
	}
	//Follow select
	if(const SelectInst* s=dyn_cast<SelectInst>(v))
	{
		POINTER_KIND k1=dfsPointerKind(s->getTrueValue(), visitedPhis);
		if(k1==REGULAR)
			return REGULAR;
		POINTER_KIND k2=dfsPointerKind(s->getFalseValue(), visitedPhis);
		//If the type is different we need to collapse to REGULAR
		if(k1!=k2)
			return REGULAR;
		//The type is the same
		return k1;
	}
	if(isComingFromAllocation(v))
		return COMPLETE_ARRAY;
	//Follow PHIs
	const PHINode* newPHI=dyn_cast<const PHINode>(v);
	if(newPHI)
	{
		std::map<const PHINode*, POINTER_KIND>::iterator alreadyVisited=visitedPhis.find(newPHI);
		if(alreadyVisited!=visitedPhis.end())
		{
			//Assume true, if needed it will become false later on
			return alreadyVisited->second;
		}
		//Intialize the PHI with undecided
		std::map<const PHINode*, POINTER_KIND>::iterator current=
			visitedPhis.insert(std::make_pair(newPHI, UNDECIDED)).first;
		for(unsigned i=0;i<newPHI->getNumIncomingValues() && current->second!=REGULAR;i++)
		{
			POINTER_KIND k=dfsPointerKind(newPHI->getIncomingValue(i), visitedPhis);
			if(current->second == UNDECIDED)
				current->second = k;
			// COMPLETE_OBJECT can't change to COMPLETE_ARRAY for the "self" optimization
			// so switch directly to REGULAR
			else if (k != current->second)
				current->second = REGULAR;
		}
		return current->second;
	}
	return REGULAR;
}

DuettoWriter::POINTER_KIND DuettoWriter::getPointerKind(const Value* v) const
{
	assert(v->getType()->isPointerTy());

	pointer_kind_map_t::const_iterator iter = pointerKindMap.find(v);

	if (pointerKindMap.end() == iter)
	{
		std::map<const PHINode*, POINTER_KIND> visitedPhis;
		iter = pointerKindMap.insert( std::make_pair(v,dfsPointerKind(v, visitedPhis)) ).first;
	}
	return iter->second;
}

uint32_t DuettoWriter::getPointerUsageFlags(const llvm::Value * v) const
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
			if (const StoreInst * I = dyn_cast<const StoreInst>(U) )
			{
				if (I->getPointerOperand() == v)
					ans |= POINTER_NONCONST_DEREF; 
			}
			
			// Check if the pointer "v" is used as lhs or rhs of a comparison operation
			else if (const CmpInst * I = dyn_cast<const CmpInst>(U) )
			{
				if (!I->isEquality())
					ans |= POINTER_ORDINABLE;
			}
			
			// Check if the pointer is casted to int
			else if (isa<const PtrToIntInst>(U) )
			{
				ans |= POINTER_CASTABLE_TO_INT;
			}
			
			// Pointer used as a base to a getElementPtr
			else if (isGEP(U) )
			{
				const User * I = cast<const User>(U);
  				const ConstantInt * p = dyn_cast<const ConstantInt>(I->getOperand(1));
				if (!p || !p->isZero())
					ans |= POINTER_ARITHMETIC;
			}
			/** TODO deal with all use cases and remove the following 2 blocks **/
			else if (
				isa<const PHINode>(U) || 
				isa<const SelectInst>(U) ||
				isa<const LoadInst>(U) ||
				isa<const CallInst>(U) ||
				isa<const InvokeInst>(U) ||
				isa<const ReturnInst>(U) || 
				isa<const GlobalValue>(U) ||
				isa<const ConstantArray>(U) ||
				isa<const ConstantStruct>(U) ||
				isBitCast(U) ||
				isNopCast(U) )
			{
				continue;
			}
			else
			{
#ifdef DUETTO_DEBUG_POINTERS
				print_debug_pointer_uknown(v,it->getUser(),"getPointerUsageFlags");
#endif //DUETTO_DEBUG_POINTERS
				ans |= POINTER_UNKNOWN;
			}
		}
		
		iter = pointerUsageMap.insert( std::make_pair(v, ans) ).first;
	}

	return iter->second;
}

uint32_t DuettoWriter::dfsPointerUsageFlagsComplete(const Value * v, std::set<const Value *> & openset) const
{
	if ( !openset.insert(v).second )
	{
		return 0;
	}

	uint32_t f = getPointerUsageFlags(v) | (
		(isBitCast(v) || isa<const PHINode>(v) || isa<const SelectInst>(v)) ? POINTER_IS_NOT_UNIQUE_OWNER : 0);

	for (Value::const_use_iterator it = v->use_begin(); it != v->use_end(); ++it)
	{
		const User* U = it->getUser();
		// Check if "v" is used as a operand in a phi node
		if (isa<const PHINode>(U) ||
			isa<const SelectInst>(U) ||
			isBitCast(U) ||
			isNopCast(U))
		{
			f |= dfsPointerUsageFlagsComplete(U, openset) | POINTER_IS_NOT_UNIQUE_OWNER;
		} 
		else if (isa<const CallInst>(U) || isa<const InvokeInst>(U) )
		{
			//TODO deal with me properly
			f |= POINTER_UNKNOWN;
		}
		else if (isa<const ReturnInst>(U))
		{
			//TODO deal with me properly
			f |= POINTER_UNKNOWN;
		}
		else if (const StoreInst * I = dyn_cast<const StoreInst>(U) )
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
			isa<const ConstantStruct>(U) ||
			isa<const ConstantArray>(U) ||
			isa<const GlobalValue>(U) )
		{
			f |= POINTER_UNKNOWN;
		}
		else if ( // Things we know are ok
			isa<const CmpInst>(U) ||
			isa<const LoadInst>(U) ||
			isa<const PtrToIntInst>(U) ||
			isGEP(U) )
		{
			continue;
		}
		else
		{
#ifdef DUETTO_DEBUG_POINTERS
			print_debug_pointer_uknown(v,it->getUser(),"dfsPointerUsageFlagsComplete");
#endif //DUETTO_DEBUG_POINTERS

			//NOTE no need to add POINTER_IS_NOT_UNIQUE_OWNER to PtrToIntInst since IntToPtr are disabled anyway
			f |= POINTER_UNKNOWN;
		}
	}
	return f;
}

uint32_t DuettoWriter::getPointerUsageFlagsComplete(const Value * v) const
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

bool DuettoWriter::isNoSelfPointerOptimizable(const llvm::Value * v) const
{
	assert( v->getType()->isPointerTy() );
	return ! (getPointerUsageFlagsComplete(v) & (POINTER_ARITHMETIC | POINTER_ORDINABLE | POINTER_CASTABLE_TO_INT) );
}

bool DuettoWriter::isNoWrappingArrayOptimizable(const llvm::Value * v) const
{
	assert( v->getType()->isPointerTy() );
	
	return isImmutableType(v->getType()->getPointerElementType()) && // This type of optimization makes sense only for immutable types
		!(getPointerUsageFlagsComplete(v) & (POINTER_ARITHMETIC | POINTER_ORDINABLE | POINTER_CASTABLE_TO_INT | POINTER_IS_NOT_UNIQUE_OWNER) );
}
