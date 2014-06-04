//===-- Duetto/RemovePhi.h - Duetto phi removal code ---------------===//
//
//                     Duetto: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _DUETTO_REMOVE_PHI_H
#define _DUETTO_REMOVE_PHI_H

#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Pass.h"

namespace llvm {

/**
 * This pass replace any phi with an alloca/store + loads
 */
class DuettoPHIRemoval : public FunctionPass
{
public:
	static char ID;
	explicit DuettoPHIRemoval() : FunctionPass(ID) {}

	bool runOnFunction(Function &F) override;

	const char *getPassName() const;
};

/**
 * Look for pairs of Alloca/Stores which can be compiled with a single instruction in JS.
 * Move the relative store right next to the sibling alloca.
 */
class DuettoAllocaStorePair : public FunctionPass
{
public:
	static char ID;
	explicit DuettoAllocaStorePair() : FunctionPass(ID) {}
	
	bool runOnFunction(Function &F) override;
	
	const char *getPassName() const;
};

//===----------------------------------------------------------------------===//
//
// DuettoPHIRemoval - This pass removes all the phis from a function
//
FunctionPass *createDuettoPHIRemovalPass();

//===----------------------------------------------------------------------===//
//
// DuettoAllocaStorePair - This pass couples sibling allocas and stores
//
FunctionPass *createDuettoAllocaStorePairPass();
}

#endif //_DUETTO_REMOVE_PHI_H

