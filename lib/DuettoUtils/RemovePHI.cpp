//===-- RemovePhi.cpp - The Duetto JavaScript generator ---------------===//
//
//                     Duetto: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "duettoRemovePhi"
#include "llvm/ADT/Statistic.h"
#include "llvm/Duetto/RemovePHI.h"
#include "llvm/Transforms/Utils/Local.h"
#include <algorithm>

STATISTIC(NumAllocaStorePairs, "Number of alloca and stores paired");

using namespace llvm;

namespace llvm {

bool DuettoPHIRemoval::runOnFunction(Function& F)
{
	if (F.empty()) return false;
	
	// Find the first non alloca inst in the first BB
	BasicBlock::iterator InsertionPoint = F.begin()->begin();
	while (InsertionPoint != F.begin()->end() && isa<AllocaInst>(InsertionPoint)) 
		++InsertionPoint;
	
	bool Changed = false;
	
	for (BasicBlock & bb : F)
		for (BasicBlock::iterator it = bb.begin(); it != bb.end(); )
		{
			if (PHINode * phi = dyn_cast<PHINode>(it++) )
			{
				DemotePHIToStack(phi, InsertionPoint);
				Changed = true;
			}
		}
	
	return Changed;
}

const char *DuettoPHIRemoval::getPassName() const {
	return "DuettoPHIRemoval";
}

char DuettoPHIRemoval::ID = 0;

FunctionPass *createDuettoPHIRemovalPass() { return new DuettoPHIRemoval(); }

bool DuettoAllocaStorePair::runOnFunction(Function & F)
{
	bool Changed = false;

	for (BasicBlock & bb : F)
	{
		for ( BasicBlock::iterator it = bb.begin(); it != bb.end(); ++it )
		{
			if ( isa<AllocaInst>(it) )
			{
				BasicBlock::iterator firstUser = std::find_if( 
					std::next(it), 
					bb.end(),
					[&]( const Instruction & I )
					{
						return std::find( it->user_begin(), it->user_end(), &I ) != it->user_end();
					});
				
				if ( (firstUser != bb.end() ) && isa<StoreInst>(firstUser) )
				{
					StoreInst * st = cast<StoreInst>(firstUser);
					if ( st->getPointerOperand() == it )
					{
						Value * v = st->getValueOperand();
						
						bool isValueAvailable = true;

						if ( Instruction * vi = dyn_cast<Instruction>(v) )
						{
							if  ( vi->getParent() != &bb )
								isValueAvailable = false;
							else
								isValueAvailable = std::find_if( BasicBlock::iterator(vi), bb.end(),[&] (const Instruction & i ) { return &i == it; }) != bb.end();
						}
						
						if ( isValueAvailable )
						{
							// Pair
							firstUser->moveBefore( std::next(it) );
						
							NumAllocaStorePairs++;
							Changed = true;
						}
					}
				}
			}
		}
	}
	
	return Changed;
}


const char* DuettoAllocaStorePair::getPassName() const
{
    return "DuettoAllocaStorePair";
}

char DuettoAllocaStorePair::ID = 0;

FunctionPass *createDuettoAllocaStorePairPass() { return new DuettoAllocaStorePair(); }


}
