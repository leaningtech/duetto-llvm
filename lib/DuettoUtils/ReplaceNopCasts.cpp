//===-- ReplaceNopCasts.cpp - The Duetto JavaScript generator ---------------===//
//
//                     Duetto: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include <algorithm>
#include "llvm/Duetto/ReplaceNopCasts.h"
#include "llvm/Duetto/Utility.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Support/FormattedStream.h"

namespace llvm {

using namespace duetto;

bool ReplaceNopCasts::runOnModule(Module & M)
{
	constantExprDone.clear();

	for ( const GlobalVariable & GV : M.globals() )
	{
		if ( GV.hasInitializer() )
		{
			if ( const ConstantExpr * expr = dyn_cast<ConstantExpr>(GV.getInitializer()) )
				processConstexpr(expr);
		}
	}

	bool Changed = false;

	for ( Function & F : M )
	{
		if ( F.empty() ) continue;

		for ( BasicBlock & BB : F )
			Changed |= processBasicBlock(BB);
		
		assert( ! verifyFunction(F, &llvm::errs()) );
	}

	return Changed;
}

bool ReplaceNopCasts::processBasicBlock(BasicBlock& BB)
{
	bool Changed = false;
	
	/**
	 * First pass: replace nopCasts with bitcasts and report warning for invalid type casts
	 */
	for ( BasicBlock::iterator it = BB.begin(); it != BB.end(); )
	{
		Instruction * Inst = it++;
		
		if (isNopCast(Inst) )
		{
			assert( isa<CallInst>(Inst) );
			
			CallInst * call = cast<CallInst>(Inst);
			
			if ( TypeSupport::isClientType( call->getType()) )
			{
				llvm::errs() << "Cast of client type: " << *call << "\n";
				continue;
			}
			if ( TypeSupport::isClientType( call->getArgOperand(0)->getType()) )
			{
				llvm::errs() << "Cast of client type: " << *call->getArgOperand(0) << "\n";
				continue;
			}
			
			ReplaceInstWithInst( call,  BitCastInst::Create( Instruction::CastOps::BitCast, call->getArgOperand(0), call->getType() ) );

			Changed = true;
		}
		else if ( isa<BitCastInst>(Inst) )
		{
			if ( ! TypeSupport::isValidTypeCast(Inst->getOperand(0), Inst->getType()) )
				reportUnsafeCast( Inst );
		}
		else
		{
			for ( Use & u : Inst->operands() )
			{
				if ( const ConstantExpr * expr = dyn_cast<ConstantExpr>(u.get()) )
					processConstexpr( expr );
			}
		}
	}
	
	/**
	 * Second pass: collapse bitcasts of bitcasts.
	 * 
	 * Note: this might leave some dead instruction around, but we don't care since bitcasts are inlined anyway
	 */
	for ( BasicBlock::iterator it = BB.begin(); it != BB.end(); ++it )
	{
		if ( isa<BitCastInst>(it) ) 
		{
			while ( BitCastInst * src = dyn_cast<BitCastInst>(it->getOperand(0) ) )
			{
				it->setOperand(0, src->getOperand(0) );
				Changed = true;
			}
		}
	}

	return Changed;
}

void ReplaceNopCasts::processConstexpr(const ConstantExpr * expr)
{
	if ( !constantExprDone.insert( expr ).second ) 
		return;

	if ( expr->getOpcode() == Instruction::BitCast )
	{
		assert ( expr->getNumOperands() > 0 );
		if ( ! TypeSupport::isValidTypeCast( expr->getOperand(0), expr->getType() ) )
			reportUnsafeCast( expr );
	}
	
	for ( const Use & u : expr->operands() )
	{
		if ( const ConstantExpr * next = dyn_cast<ConstantExpr>(u.get()) )
			processConstexpr(next);
	}
}

void ReplaceNopCasts::reportUnsafeCast(const User* u) const
{
	llvm::errs() << "warning in instruction: " << *u << "\n";
	llvm::errs() << "\t Type conversion between: \t" << *u->getOperand(0)->getType() << " and " << *u->getType() << "\n\tis not safe. ";
	llvm::errs() << "Expect issues. And report a bug.\n\n";
}

const char *ReplaceNopCasts::getPassName() const {
	return "ReplaceNopCasts";
}

char ReplaceNopCasts::ID = 0;

ModulePass *createReplaceNopCastsPass() { return new ReplaceNopCasts(); }

}
