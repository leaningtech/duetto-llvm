//===-- Duetto/Inliner.h - Duetto instruction inlining analyzer -------------===//
//
//                     Duetto: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _DUETTO_INLINER_H
#define _DUETTO_INLINER_H

#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/IR/Instruction.h"
#include <unordered_map>

namespace duetto {

/**
 * Detect if an instruction can be inlined
 */
class Inliner
{
public:
	explicit Inliner(llvm::AliasAnalysis & AA) : AA(AA) {}
	
	/**
	 * Implement an heuristic to decide whether an instruction should actually be inlined
	 */
	bool isInlined(const llvm::Instruction * I) const
	{
		auto iter = cache.find(I);
		
		if ( cache.end() == iter )
		{
			iter = cache.emplace(I, decideIfInline(I) ).first;
		}

		return iter->second;
	}

private:
	
	/**
	 * Decide whether actually inline an instruction
	 */
	bool decideIfInline(const llvm::Instruction * I) const;
	
	/**
	 * Regardless of the actual heuristic used,
	 * checks if an instruction can be inlined
	 */
	bool isInlineable(const llvm::Instruction * I) const;
	
	/**
	 * Check if a LoadInst can be inlined
	 */
	bool isLoadInlineable(const llvm::LoadInst * I) const;
	
	/**
	 * Check if BitCasts can be inlined
	 */
	bool isBitCastInlineable(const llvm::BitCastInst * I) const;
	
	mutable std::unordered_map< const llvm::Instruction *, bool > cache;
	llvm::AliasAnalysis & AA;
};

}

#endif //_DUETTO_INLINER_H

