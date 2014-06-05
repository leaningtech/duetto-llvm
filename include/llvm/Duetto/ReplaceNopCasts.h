//===-- Duetto/ReplaceNopCasts.h - replace nop casts with bitcasts ---------------===//
//
//                     Duetto: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _DUETTO_REPLACE_NOP_CASTS_H
#define _DUETTO_REPLACE_NOP_CASTS_H

#include <unordered_set>
#include "llvm/IR/Constants.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"

namespace llvm {

// Replace all NopCasts with BitCast, and report warnings for existing unsafe bitcasts
class ReplaceNopCasts: public ModulePass
{
public:
	static char ID;

	explicit ReplaceNopCasts() : ModulePass(ID) { }

	virtual bool runOnModule(Module &M) override;
	
	virtual const char *getPassName() const override;
	
private:
	bool processBasicBlock(BasicBlock & BB);
	void processConstexpr( const ConstantExpr * expr );
	
	void reportUnsafeCast( const User * u ) const;
	
	std::unordered_set< const ConstantExpr * > constantExprDone;
};

//===----------------------------------------------------------------------===//
//
// ReplaceNopCasts
//
ModulePass *createReplaceNopCastsPass();
}

#endif //_DUETTO_REPLACE_NOP_CASTS_H

