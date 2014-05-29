//===-- Inliner.cpp - The Duetto JavaScript generator --------------------===//
//
//                     Duetto: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Duetto/Inliner.h"
#include "llvm/Duetto/Utility.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Intrinsics.h"

using namespace llvm;

namespace duetto {

bool Inliner::decideIfInline(const llvm::Instruction * I) const
{
	if (isInlineable(I))
	{
		// Always inline bitcasts and geps
		if (I->getOpcode() == Instruction::GetElementPtr ||
			I->getOpcode() == Instruction::BitCast)
			return true;
		
		// Stub
		return I->hasOneUse();
	}
	else
		return false;
}

bool Inliner::isInlineable(const Instruction * I) const
{
	/** Always false if there is a phi user **/
	if ( std::any_of(I->user_begin(), I->user_end(), 
			[](const User * U)
			{
				return isa<PHINode>(U);
			}) )
		return false;
	
		
	if (const LoadInst * LI = dyn_cast<LoadInst>(I) )
		return isLoadInlineable(LI);

	if (const BitCastInst * BI = dyn_cast<BitCastInst>(I) )
		return isBitCastInlineable(BI);
	
	/**
	 * List of instruction which can be inlined in any case
	 */
	switch (I->getOpcode())
	{
		case Instruction::GetElementPtr:
		case Instruction::Add:
		case Instruction::Sub:
		case Instruction::Mul:
		case Instruction::And:
		case Instruction::Or:
		case Instruction::Xor:
		case Instruction::Trunc:
		case Instruction::FPToSI:
		case Instruction::SIToFP:
		case Instruction::SDiv:
		case Instruction::SRem:
		case Instruction::Shl:
		case Instruction::AShr:
		case Instruction::LShr:
		case Instruction::FAdd:
		case Instruction::FDiv:
		case Instruction::FSub:
		case Instruction::FPTrunc:
		case Instruction::FPExt:
		case Instruction::FMul:
		case Instruction::FCmp:
		case Instruction::ICmp:
		case Instruction::ZExt:
		case Instruction::SExt:
		case Instruction::Select:
		case Instruction::ExtractValue:
		case Instruction::URem:
		case Instruction::UDiv:
		case Instruction::UIToFP:
		case Instruction::FPToUI:
		case Instruction::PtrToInt:
			return true;
		case Instruction::Call:
		case Instruction::Invoke:
		case Instruction::Ret:
		case Instruction::LandingPad:
		case Instruction::PHI:
		case Instruction::Load:
		case Instruction::Store:
		case Instruction::InsertValue:
		case Instruction::Resume:
		case Instruction::Br:
		case Instruction::Alloca:
		case Instruction::Switch:
		case Instruction::Unreachable:
		case Instruction::VAArg:
			return false;
		default:
			llvm::report_fatal_error(Twine("Unsupported opcode: ",StringRef(I->getOpcodeName())), false);
			return false;
	}
}

bool Inliner::isLoadInlineable(const LoadInst * I) const
{
	if (I->isUsedOutsideOfBlock( I->getParent() ) )
		return false;

	const llvm::BasicBlock * bb = I->getParent();

	const Value * val = I->getPointerOperand();
	
	// Find last user
	BasicBlock::const_iterator first = std::next(BasicBlock::const_iterator(I) );

	const User * last = bb->end();
	for ( auto it = first; it != bb->end(); ++it )
		if ( std::find( I->user_begin(), I->user_end(), it ) != I->user_end() )
			last = std::next(it);
	
	// Check all the stores between first and last
	for (auto it = first; it != last; ++it )
	{
		ImmutableCallSite callV(it);
		const Value * p = nullptr;
		if ( const StoreInst * st = dyn_cast<StoreInst>(it) )
			p = st->getPointerOperand();
		else if ( callV.isCall() || callV.isInvoke() )
		{
			const Function * f = callV.getCalledFunction();
			
			if (f && 
				(f->getIntrinsicID() == Intrinsic::memcpy ||
				f->getIntrinsicID() == Intrinsic::memset) )
			{
				p = callV.getArgument(0);
			}
			else 
				return false;
		}
		
		if (p && (AA.alias( val, p ) != AliasAnalysis::NoAlias ) )
			return false;
	}
	return true;
}

bool Inliner::isBitCastInlineable(const BitCastInst * BI) const
{
        //Inline casts which are not unions
        llvm::Type* src = BI->getOperand(0)->getType();
        
        if(!src->isPointerTy() || !TypeSupport::isUnion(src->getPointerElementType()))
                return true;

        Type* pointedType = src->getPointerElementType();
        
        //Do not inline union casts to array
        if(ArrayType::classof(pointedType))
                return false;

        //Inline if the only uses are load and stores
        return std::all_of(
                BI->user_begin(),
                BI->user_end(),
                [](const User * U)
                {
                        return isa<LoadInst>(U) || isa<StoreInst>(U);
                });;
}

}
