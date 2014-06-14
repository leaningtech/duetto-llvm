//===-- PointerAnalyzer.cpp - The Cheerp JavaScript generator -------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include <iomanip>
#include <sstream>
#include "llvm/Cheerp/PointerAnalyzer.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/GlobalAlias.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/Support/FormattedStream.h"

using namespace llvm;

namespace cheerp {

/*
 * The map is used to handle cyclic PHI nodes
 */
POINTER_KIND PointerAnalyzer::getPointerKind(const Value* v) const
{
	assert(v->getType()->isPointerTy());

	pointer_kind_map_t::iterator iter = pointerKindMap.find(v);

	if (pointerKindMap.end() != iter)
		return iter->second;
	iter = pointerKindMap.insert( std::make_pair(v, UNDECIDED) ).first;
	
	PointerType * pt = cast<PointerType>(v->getType());

	if ( pt->isPointerTy() && pt->getPointerElementType()->isFunctionTy() )
		return iter->second = COMPLETE_OBJECT;

	if( const IntrinsicInst * ii = dyn_cast<IntrinsicInst>(v) )
	{
		switch( ii->getIntrinsicID() )
		{
		case Intrinsic::cheerp_create_closure:
			return iter->second = COMPLETE_OBJECT;
		case Intrinsic::cheerp_pointer_base:
		case Intrinsic::cheerp_make_complete_object:
			return iter->second = COMPLETE_OBJECT;
		default:
			break;
		}
	}

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
	if( isa<AllocaInst>(v) || isa<GlobalVariable>(v))
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
	}
	if( isBitCast(v) || isNopCast(v) )
	{
		const User* bi = cast<User>(v);

		assert( bi->getOperand(0)->getType()->isPointerTy() );

		PointerType * toTy = cast<PointerType>(v->getType());
		PointerType * fromTy = cast<PointerType>(bi->getOperand(0)->getType());

		if ( (getStratForType(fromTy) != getStratForType(toTy) ) &&
			! TypeSupport::isClientType( fromTy->getElementType() ) &&
			( getPointerUsageFlagsComplete(bi) & need_self_flags ) )
		{
			return iter->second = REGULAR;
		}
		else
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

	if ( const Argument * arg = dyn_cast<Argument>(v) )
	{
		const Function * F = arg->getParent();
		
		if (! hasNonRegularArgs(F) )
			return iter->second = REGULAR;
	}
	
	if (isa<PHINode>(v) || isa<Argument>(v))
	{
		if (TypeSupport::isImmutableType( pt->getElementType() ) )
		{
			if ( isa<Argument>(v) )
				return iter->second = (needsWrappingArray(v) || isArgumentReadOnly( cast<Argument>(v) ) ) ? REGULAR : COMPLETE_OBJECT;
			else
				return iter->second = REGULAR;
		}
		else
		{
			return iter->second = (getPointerUsageFlagsComplete(v) & need_self_flags) ? REGULAR : COMPLETE_OBJECT;
		}
	}
	return iter->second = REGULAR;
}

POINTER_KIND PointerAnalyzer::getPointerKindForStore(const Value* v) const
{
	if (const PointerType * pt = dyn_cast<PointerType>(v->getType()) )
	{
		if ( const PointerType * pt2 = dyn_cast<PointerType>( pt->getPointerElementType() ) )
		{
			if ( pt2->getPointerElementType()->isFunctionTy() )
				return COMPLETE_OBJECT;
		}
	}
	return REGULAR;
}

POINTER_KIND PointerAnalyzer::getPointerKindForStore(const Constant* v) const
{
	if (const PointerType * pt = dyn_cast<PointerType>(v->getType()) )
	{
		if ( pt->getPointerElementType()->isFunctionTy() )
			return COMPLETE_OBJECT;
	}
	return REGULAR;
}

POINTER_KIND PointerAnalyzer::getPointerKindForArgOperand(User::const_op_iterator it,
							  Function::const_arg_iterator arg) const
{
	ImmutableCallSite callV ( it->getUser() );
	assert( callV );

	const Function * F = callV.getCalledFunction();

	// Direct call, just forward to getPointerKind for the argument
	if ( F && arg != F->arg_end() )
	{
		assert( arg->getArgNo() == callV.getArgumentNo(it) );

		switch( F->getIntrinsicID() )
		{
		case Intrinsic::cheerp_create_closure:
			return arg->getArgNo() == 0 ?
				COMPLETE_OBJECT : REGULAR;
		case Intrinsic::cheerp_make_complete_object:
			return COMPLETE_OBJECT;
		default:
			return getPointerKind(arg);
		}
	}
	Type * tp = (*it)->getType();

	return tp->getPointerElementType()->isFunctionTy() ?
		COMPLETE_OBJECT :
		REGULAR;
}

bool PointerAnalyzer::hasSelfMember(const Value* v) const
{
	assert( v->getType()->isPointerTy() );
	
	PointerType * tp = cast<PointerType>( v->getType() );

	if ( TypeSupport::isImmutableType(tp->getElementType()) )
		return false;

	if ( isa<StructType>( tp->getElementType() ) && types.hasBasesInfo( tp->getElementType() ) )
		return false;

	return (getPointerUsageFlagsComplete(v) & need_self_flags);
}

#ifndef NDEBUG

void PointerAnalyzer::dumpPointer(const Value* v, bool dumpOwnerFunc) const
{
	llvm::formatted_raw_ostream fmt( llvm::errs() );

	fmt.changeColor( llvm::raw_ostream::RED, false, false );
	v->printAsOperand( fmt );
	fmt.resetColor();
	
	if (dumpOwnerFunc)
	{
		if ( const Instruction * I = dyn_cast<Instruction>(v) )
			fmt << " in function: " << I->getParent()->getParent()->getName();
		else if ( const Argument * A = dyn_cast<Argument>(v) )
			fmt << " arg of function: " << A->getParent()->getName();
	}

	if (v->getType()->isPointerTy())
	{
		fmt.PadToColumn(92);
		switch (getPointerKind(v))
		{
			case COMPLETE_OBJECT: fmt << "COMPLETE_OBJECT"; break;
			case COMPLETE_ARRAY: fmt << "COMPLETE_ARRAY"; break;
			case REGULAR: fmt << "REGULAR"; break;
			default: fmt << "UNDECIDED"; break;
		}
		
		fmt.PadToColumn(112); fmt << "0x"; fmt.write_hex(getPointerUsageFlags(v));
		fmt.PadToColumn(132); fmt << "0x"; fmt.write_hex(getPointerUsageFlagsComplete(v));
		fmt.PadToColumn(152) << (TypeSupport::isImmutableType( v->getType()->getPointerElementType() ) ? "true" : "false" );
	}
	else
		fmt << " is not a pointer";
	fmt << '\n';
}

#endif //NDEBUG

bool PointerAnalyzer::needsWrappingArray(const Value* v) const
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
			if ( usageFlagsForCall(u, ImmutableCallSite(U), openset ) & POINTER_NONCONST_DEREF )
				return true;
		}
		return false;
	});
	
	return hasAliasedStore || (getPointerUsageFlagsComplete(v) & need_wrap_array_flags ) ||
		( (isa<Argument>(v) || isa<PHINode>(v) ) && (getPointerUsageFlagsComplete(v) & POINTER_NONCONST_DEREF) );
}

bool PointerAnalyzer::isArgumentReadOnly(const Argument* v) const
{
	const Function * F = v->getParent();
	if ( F->hasAddressTaken() ) return false;

	AliasAnalysis::Location L(v);

	for ( const Use & u : F->uses() )
	{
		ImmutableCallSite callV (u.getUser() );
		
		if ( ! (callV.isCall() || callV.isInvoke() ) )
			continue;

		assert( callV.getCalledFunction() == F );
		
		if ( AA.getModRefInfo(callV, L) & AliasAnalysis::Mod )
			return false;
	}
	return true;
}

uint32_t PointerAnalyzer::getPointerUsageFlagsComplete(const Value * v) const
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

uint32_t PointerAnalyzer::getPointerUsageFlags(const llvm::Value * v) const
{
	assert(v->getType()->isPointerTy());
	
	// HACK
	// Temporary workaround to force all the PHIs of immutable types to be REGULAR.
	// this will be removed when we will get rid of PHIs entirely
	if ( isa< PHINode > (v) && TypeSupport::isImmutableType( v->getType()->getPointerElementType()) )
		return POINTER_UNKNOWN;

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
#ifndef NDEBUG
				llvm::errs() << "Adding POINTER_UNKNOWN in getPointerUsageFlags due to instruction: " << valueObjectName(U) << "\n";
#endif
				ans |= POINTER_UNKNOWN;
			}
		}
		
		iter = pointerUsageMap.insert( std::make_pair(v, ans) ).first;
	}

	return iter->second;
}

uint32_t PointerAnalyzer::dfsPointerUsageFlagsComplete(const Value * v, std::set<const Value *> & openset) const
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
		else if ( ImmutableCallSite cs = U )
		{
			f |= usageFlagsForCall(*it,cs,openset);
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
#ifndef NDEBUG
			llvm::errs() << "Adding POINTER_UNKNOWN in dfsPointerUsageFlagsComplete due to instruction: " << valueObjectName(U) << "\n";
#endif

			f |= POINTER_UNKNOWN;
		}
	}
	
	return f;
}

uint32_t PointerAnalyzer::usageFlagsForCall(const llvm::Use & u, ImmutableCallSite I, std::set<const Value *> & openset) const
{
	if (I.isCallee(&u) )
		return 0;

	assert( std::find( I.arg_begin(), I.arg_end(), u) != I.arg_end() );

	const Function * F = I.getCalledFunction();

	if ( !hasNonRegularArgs(F) )
		return POINTER_UNKNOWN;

	unsigned argNo = I.getArgumentNo(&u);

	if ( argNo < F->arg_size() )
	{
		// standard argument
		Function::const_arg_iterator iter = F->arg_begin();
		std::advance(iter, argNo);
		return dfsPointerUsageFlagsComplete(iter, openset);
	}
	else
	{
		assert( F->isVarArg() );
		return POINTER_UNKNOWN;
	}
}

#ifndef NDEBUG

void dumpAllPointers(const Function & F, const PointerAnalyzer & analyzer)
{
	llvm::errs() << "Function: " << F.getName();
	if ( F.hasAddressTaken() )
		llvm::errs() << " (with address taken)";
	llvm::errs() << "\n";
	
	for ( const Argument & arg : F.getArgumentList() )
		analyzer.dumpPointer(&arg, false);

	for ( const BasicBlock & BB : F )
	{
		for ( const Instruction & I : BB )
		{
			if ( I.getType()->isPointerTy() )
				analyzer.dumpPointer(&I, false);
		}
	}
	llvm::errs() << "\n";
}

void writePointerDumpHeader()
{
	llvm::formatted_raw_ostream fmt( llvm::errs() );
	fmt.PadToColumn(0) << "Name";
	fmt.PadToColumn(92) << "Kind";
	fmt.PadToColumn(112) << "UsageFlags";
	fmt.PadToColumn(132) << "UsageFlagsComplete";
	fmt.PadToColumn(152) << "IsImmutable";
	fmt << '\n';
}

#endif //NDEBUG

}
