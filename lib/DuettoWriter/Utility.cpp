//===-- Utility.cpp - The Duetto JavaScript generator --------------------===//
//
//                     Duetto: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include <sstream>
#include "llvm/Duetto/Utility.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalAlias.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Operator.h"
#include "llvm/Support/FormattedStream.h"

using namespace llvm;

namespace duetto {

bool isNopCast(const Value* val)
{
	const CallInst * newCall = dyn_cast<const CallInst>(val);
	if(newCall && newCall->getCalledFunction())
	{
		unsigned int id = newCall->getCalledFunction()->getIntrinsicID();
		
		if ( Intrinsic::duetto_upcast_collapsed == id ||
			Intrinsic::duetto_cast_user == id )
			return true;
		
		if ( Intrinsic::duetto_downcast == id )
		{
			Type* t = newCall->getArgOperand(0)->getType()->getPointerElementType();

			if ( TypeSupport::isClientType(t) ||
				getIntFromValue( newCall->getArgOperand(1) ) == 0 )
				return true;
		}
		
	}
	return false;
}

bool isValidVoidPtrSource(const Value* val, std::set<const PHINode*>& visitedPhis)
{
	if (DynamicAllocInfo::getAllocType(val) != DynamicAllocInfo::not_an_alloc )
		return true;
	const PHINode* newPHI=dyn_cast<const PHINode>(val);
	if(newPHI)
	{
		if(visitedPhis.count(newPHI))
		{
			//Assume true, if needed it will become false later on
			return true;
		}
		visitedPhis.insert(newPHI);
		for(unsigned i=0;i<newPHI->getNumIncomingValues();i++)
		{
			if(!isValidVoidPtrSource(newPHI->getIncomingValue(i),visitedPhis))
			{
				visitedPhis.erase(newPHI);
				return false;
			}
		}
		visitedPhis.erase(newPHI);
		return true;
	}
	return false;
}

bool isBitCast(const Value* v)
{
	const User* b=static_cast<const User*>(v);
	if(isa<BitCastInst>(v))
	{
		bool validCast = TypeSupport::isValidTypeCast(b->getOperand(0), v->getType());
// 		if(!validCast)
// 		{
// 			llvm::errs() << "Error while handling cast " << *v << "\n";
// 			llvm::report_fatal_error("Unsupported code found, please report a bug", false);
// 			return false;
// 		}
		return true;
	}
	const ConstantExpr* ce=dyn_cast<const ConstantExpr>(v);
	if(ce && ce->getOpcode()==Instruction::BitCast)
	{
		bool validCast = TypeSupport::isValidTypeCast(b->getOperand(0), v->getType());
// 		if(!validCast)
// 		{
// 			llvm::errs() << "Error while handling cast " << *v << "\n";
// 			llvm::report_fatal_error("Unsupported code found, please report a bug", false);
// 			return false;
// 		}
		return true;
	}
	return false;
}

bool isGEP(const Value* v)
{
	if(GetElementPtrInst::classof(v))
		return true;
	const ConstantExpr* ce=dyn_cast<const ConstantExpr>(v);
	if(ce && ce->getOpcode()==Instruction::GetElementPtr)
		return true;
	return false;
}


uint32_t getIntFromValue(const Value* v)
{
	if(!ConstantInt::classof(v))
	{
		llvm::errs() << "Expected constant int found " << *v << "\n";
		llvm::report_fatal_error("Unsupported code found, please report a bug", false);
		return 0;
	}

	const ConstantInt* i=cast<const ConstantInt>(v);
	return i->getZExtValue();
}

std::string valueObjectName(const Value* v)
{
	std::ostringstream os;
	if (const Instruction * p = dyn_cast<const Instruction>(v) )
		os << " instruction " << p->getOpcodeName() << "\n";
	else if (const Constant * p = dyn_cast<const Constant>(v) )
	{
		os << " constant " << p->getName().str() << "(";
		
		// Feel free to find a way to avoid this obscenity
		if (isa<const BlockAddress>(p))
			os << "BlockAddress";
		else if (isa<const ConstantAggregateZero>(p))
			os << "ConstantAggregateZero";
		else if (isa<const ConstantArray>(p))
			os << "ConstantArray";
		else if (isa<const ConstantDataSequential>(p))
			os << "ConstantDataSequential";
		else if (const ConstantExpr * pc = dyn_cast<const ConstantExpr>(p))
		{
			os << "ConstantExpr [" << pc->getOpcodeName() <<"]";
		}
		else if (isa<const ConstantFP>(p))
			os << "ConstantFP";
		else if (isa<const ConstantInt>(p))
			os << "ConstantInt";
		else if (isa<const ConstantPointerNull>(p))
			os << "ConstantPointerNull";
		else if (isa<const ConstantStruct>(p))
			os << "ConstantStruct";
		else if (isa<const ConstantVector>(p))
			os << "ConstantVector";
		else if (isa<const GlobalAlias>(p))
			os << "GlobalAlias";
		else if (isa<const GlobalValue>(p))
			os << "GlobalValue";
		else if (isa<const UndefValue>(p))
			os << "UndefValue";
		else
			os << "Unknown";
		os << ")\n";
	}
	else if ( isa<const Operator>(p) )
		os << " operator " << p->getName().str() << "\n";
	return os.str();
}

bool TypeSupport::isValidTypeCast(const Value * castOp, Type * dstPtr)
{
	Type * srcPtr = castOp->getType();

	//Only pointer casts are possible anyway
	assert(srcPtr->isPointerTy() && dstPtr->isPointerTy());
	Type * src = cast<PointerType>(srcPtr)->getElementType();
	Type * dst = cast<PointerType>(dstPtr)->getElementType();

	//Conversion between client objects is free
	if(isClientType(src) && isClientType(dst))
		return true;

	//Conversion between any function pointer is ok
	if(src->isFunctionTy() && dst->isFunctionTy())
		return true;

	//Allow conversions between equivalent struct types
	if(src->isStructTy() && dst->isStructTy())
	{
		StructType* srcSt = cast<StructType>(src);
		StructType* dstSt = cast<StructType>(dst);
		if(srcSt->isLayoutIdentical(dstSt))
			return true;
	}
	if(dst->isIntegerTy(8))
		return true;

	//Support getting functions back from the Vtable
	if(src->isPointerTy() && dst->isPointerTy())
	{
		Type* innerSrc=cast<PointerType>(src)->getElementType();
		Type* innerDst=cast<PointerType>(dst)->getElementType();
		if(innerSrc->isIntegerTy(8) || innerDst->isFunctionTy())
		{
			const ConstantExpr* constGep=dyn_cast<const ConstantExpr>(castOp);
			if(constGep && constGep->getOpcode()==Instruction::GetElementPtr)
			{
				const Value* sourceVal = constGep->getOperand(0);
				if(sourceVal->hasName() &&
					strncmp(sourceVal->getName().data(),"_ZTV",4)==0)
				{
					//This casts ultimately comes from a VTable, it's ok
					return true;
				}
			}
		}
		if(innerSrc->isFunctionTy() && innerDst->isFunctionTy())
			return true;
	}
	//Also allow the unsafe cast from i8* in a few selected cases
	if(src->isIntegerTy(8))
	{
		if ( isValidVoidPtrSource(castOp) )
		{
			bool allowedRawUsages = std::all_of(
				castOp->user_begin(),
				castOp->user_end(),
				[](const User * U) -> bool
				{
					//Check that the other use is a memset or an icmp
					if ( const ConstantExpr * ce = dyn_cast<ConstantExpr>(U) )
						return ce->getOpcode() == Instruction::BitCast;
					return isa<ICmpInst>(U) || isa<CastInst>(U) || safeCallForNewedMemory(dyn_cast<CallInst>(U));
				});

			if(allowedRawUsages)
				return true;
		}
	}
	if(isUnion(src) && (ArrayType::classof(dst) || isTypedArrayType(dst)))
		return true;

	//Allow changing the size of an array
	if (ArrayType::classof(src) && ArrayType::classof(dst) &&
		src->getSequentialElementType() == dst->getSequentialElementType())
	{
		return true;
	}
	return false;
}


bool TypeSupport::isI32Type(Type* t)
{
	return t->isIntegerTy() && static_cast<const IntegerType*>(t)->getBitWidth()==32;
}

bool TypeSupport::isTypedArrayType(Type* t)
{
	return t->isIntegerTy(8) || t->isIntegerTy(16) || t->isIntegerTy(32) ||
		t->isFloatTy() || t->isDoubleTy();
}

bool TypeSupport::isImmutableType(Type* t)
{
	if(t->isIntegerTy() || t->isFloatTy() || t->isDoubleTy() || t->isPointerTy())
		return true;
	return false;
}

bool TypeSupport::isUnion(Type* t)
{
	return (t->isStructTy() && cast<StructType>(t)->hasName() &&
		t->getStructName().startswith("union."));
}

bool TypeSupport::getBasesInfo(StructType* t, uint32_t& firstBase, uint32_t& baseCount) const
{
	const NamedMDNode* basesNamedMeta = getBasesMetadata(t);
	if(!basesNamedMeta)
		return false;

	MDNode* basesMeta=basesNamedMeta->getOperand(0);
	assert(basesMeta->getNumOperands()==2);
	firstBase=getIntFromValue(basesMeta->getOperand(0));
	int32_t baseMax=getIntFromValue(basesMeta->getOperand(1))-1;
	baseCount=0;

	StructType::element_iterator E=t->element_begin()+firstBase;
	StructType::element_iterator EE=t->element_end();
	for(;E!=EE;++E)
	{
		baseCount++;
		StructType* baseT=cast<StructType>(*E);
		NamedMDNode* baseNamedMeta=module.getNamedMetadata(Twine(baseT->getName(),"_bases"));
		if(baseNamedMeta)
			baseMax-=getIntFromValue(baseNamedMeta->getOperand(0)->getOperand(1));
		else
			baseMax--;
		assert(baseMax>=0);
		if(baseMax==0)
			break;
	}
	return true;
}

Type* TypeSupport::dfsFindRealType(const Value* v, std::set<const PHINode*>& visitedPhis)
{
	if(isBitCast(v))
		return static_cast<const User*>(v)->getOperand(0)->getType();
	else if(const IntrinsicInst* ci = dyn_cast<IntrinsicInst>(v))
	{
		//Support duetto.cast.user
		if(ci->getIntrinsicID() == Intrinsic::duetto_cast_user)
			return ci->getArgOperand(0)->getType();
	}

	const PHINode* newPHI=dyn_cast<const PHINode>(v);
	if(newPHI)
 	{
		if(!visitedPhis.insert(newPHI).second)
		{
			//Assume true, if needed it will become false later on
			return nullptr;
		}
		
		assert(newPHI->getNumIncomingValues()>=1);

		Type* ret = dfsFindRealType(newPHI->getIncomingValue(0),visitedPhis);

		for(unsigned i=1;i<newPHI->getNumIncomingValues();i++)
		{
			Type* t=dfsFindRealType(newPHI->getIncomingValue(i),visitedPhis);
			if(t==NULL)
				continue;
			else if(ret==NULL)
				ret=t;
			else if(ret!=t)
			{
				llvm::errs() << "Unconsistent real types for phi " << *v << "\n";
				llvm::report_fatal_error("Unsupported code found, please report a bug", false);
				return ret;
			}
		}
		visitedPhis.erase(newPHI);
		return ret;
 	}
	return v->getType();
}

const llvm::NamedMDNode* TypeSupport::getBasesMetadata(const llvm::StructType * t) const
{
	if(!t->hasName())
		return nullptr;

	return module.getNamedMetadata(Twine(t->getName(),"_bases"));
}

bool TypeSupport::safeCallForNewedMemory(const CallInst* ci)
{
	//We allow the unsafe cast to i8* only
	//if the usage is free or delete
	//or one of the lifetime/invariant intrinsics
	return (ci && ci->getCalledFunction() &&
		(ci->getCalledFunction()->getName()=="free" ||
		ci->getCalledFunction()->getName()=="_ZdaPv" ||
		ci->getCalledFunction()->getName()=="_ZdlPv" ||
		ci->getCalledFunction()->getIntrinsicID()==Intrinsic::lifetime_start ||
		ci->getCalledFunction()->getIntrinsicID()==Intrinsic::lifetime_end ||
		ci->getCalledFunction()->getIntrinsicID()==Intrinsic::invariant_start ||
		ci->getCalledFunction()->getIntrinsicID()==Intrinsic::invariant_end ||
		//Allow unsafe casts for a limited number of functions that accepts callback args
		//TODO: find a nicer approach for this
		ci->getCalledFunction()->getName()=="__cxa_atexit"));
}

DynamicAllocInfo::DynamicAllocInfo( ImmutableCallSite callV ) : call(callV), type( getAllocType(callV) ), castedType(nullptr)
{
	if ( isValidAlloc() )
		castedType = computeCastedType();
}

DynamicAllocInfo::AllocType DynamicAllocInfo::getAllocType( ImmutableCallSite callV )
{
	if (callV.isCall() || callV.isInvoke() )
	{
		if (const Function * f = callV.getCalledFunction() )
		{
			if (f->getName() == "malloc")
				return malloc;
			else if (f->getName() == "calloc")
				return calloc;
			else if (f->getIntrinsicID() == Intrinsic::duetto_allocate)
				return duetto_allocate;
			else if (f->getName() == "_Znwj")
				return opnew;
			else if (f->getName() == "_Znaj")
				return opnew_array;
		}
	}
	return not_an_alloc;
}

PointerType * DynamicAllocInfo::computeCastedType() const 
{
	assert(isValidAlloc() );
	
	if ( type == duetto_allocate )
	{
		assert( call.getType()->isPointerTy() );
		return cast<PointerType>(call.getType());
	}
	
	auto getTypeForUse = [](const User * U) -> Type *
	{
		if ( isa<BitCastInst>(U) )
			return U->getType();
		else if ( const IntrinsicInst * ci = dyn_cast<IntrinsicInst>(U) )
			if ( ci->getIntrinsicID() == Intrinsic::duetto_cast_user )
				return U->getType();
		return nullptr;
	};
	
	auto firstNonNull = std::find_if(
		call->user_begin(),
		call->user_end(),
		getTypeForUse);
	
	// If there are no casts, use i8* from the call itself
	if ( call->user_end() == firstNonNull )
	{
		assert( call->getType()->isPointerTy() );
		return cast<PointerType>(call->getType());
	}
	
	assert( getTypeForUse(*firstNonNull)->isPointerTy() );
	
	PointerType * pt = cast<PointerType>( getTypeForUse(*firstNonNull) );
	
	// Check that all uses are the same
	if (! std::all_of( 
		std::next(firstNonNull),
		call->user_end(),
		[&]( const User * U ) { return getTypeForUse(U) == pt; }) )
	{
		llvm::errs() << "Can not deduce valid type for allocation instruction: " << call->getName() << '\n';
		llvm::report_fatal_error("Unsupported code found, please report a bug", false);
	}
	
	return pt;
}

const Value * DynamicAllocInfo::getByteSizeArg() const
{
	assert( isValidAlloc() );

	if ( calloc == type )
	{
		assert( call.arg_size() == 2 );
		return call.getArgument(1);
	}

	assert( call.arg_size() == 1 );
	return call.getArgument(0);
}

const Value * DynamicAllocInfo::getNumberOfElementsArg() const
{
	assert( isValidAlloc() );
	
	if ( type == calloc )
	{
		assert( call.arg_size() == 2 );
		return call.getArgument(0);
	}
	return nullptr;
}

bool DynamicAllocInfo::sizeIsRuntime() const
{
	assert( isValidAlloc() );
	if ( getAllocType() == calloc && isa<ConstantInt> (getNumberOfElementsArg() ) )
	{
		return false;
	}
	if ( isa<ConstantInt>(getByteSizeArg()) )
		return false;
	return true;
}

bool DynamicAllocInfo::useCreateArrayFunc() const
{
	if (getCastedType()->getElementType()->isStructTy() )
	{
		assert( !TypeSupport::isTypedArrayType( getCastedType()->getElementType() ) );
		return sizeIsRuntime();
	}
	return false;
}

bool DynamicAllocInfo::useCreatePointerArrayFunc() const
{
	if (getCastedType()->getElementType()->isPointerTy() )
	{
		assert( !TypeSupport::isTypedArrayType( getCastedType()->getElementType() ) );
		return sizeIsRuntime();
	}
	return false;
}

bool DynamicAllocInfo::useTypedArray() const
{
	return TypeSupport::isTypedArrayType( getCastedType()->getElementType() );
}

}
