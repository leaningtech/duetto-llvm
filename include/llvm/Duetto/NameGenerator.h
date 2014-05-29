//===-- Duetto/NameGenerator.h - Duetto name generator code ----------------===//
//
//                     Duetto: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2013 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _DUETTO_NAME_GENERATOR_H
#define _DUETTO_NAME_GENERATOR_H

#include "llvm/ADT/SmallString.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include <unordered_map>
#include <unordered_set>

namespace duetto {

/**
 * Iterator over all the worlds composed by a given set of symbols.
 * 
 * SymbolTraits description:
 * 
 *  struct SymbolTraits {
 *       static constexpr char first_symbol = ... ; // The first symbol in the symbol order (i.e. 'a' for alphabet, etc.)
 *       static char next( char c ); // The symbol after c, or first symbol if c is the last symbol
 *       static bool is_valid( StringRef ); // Determine if the passed string is valid.
 */
template<class SymbolTraits, unsigned StringSize = 4>
class name_iterator : 
	public std::iterator< std::input_iterator_tag, llvm::SmallString<StringSize> >,
	SymbolTraits // Empty base opt
{
public:
	name_iterator(SymbolTraits st = SymbolTraits () ) : 
		SymbolTraits( std::move(st) )
	{
		value_.assign(1, SymbolTraits::first_symbol );
	}
	
	explicit name_iterator(llvm::StringRef s, SymbolTraits st = SymbolTraits () ) : 
		SymbolTraits( std::move(st) ),
		value_(s) 
	{}
	
	const llvm::SmallString<4>& operator*() const { return value_; }
	const llvm::SmallString<4>* operator->() const { return &value_; }
	
	name_iterator& operator++() { advance(); return *this; }
	name_iterator operator++(int) { name_iterator cpy(*this); advance(); return cpy; }
	
	bool operator==(const name_iterator & other) const { return value_ == other.value_; }
	bool operator!=(const name_iterator & other) const { return ! operator==(other); }
	
private:
	void advance()
	{
		do
		{
			for ( int i = value_.size() - 1; i >= 0; --i )
			{
				value_[i] = SymbolTraits::next(value_[i]);
				
				if ( i == 0 )
				{
					if ( value_[0] == SymbolTraits::first_symbol )
						value_.insert( value_.begin(), SymbolTraits::first_symbol );
				}
				else if ( value_[i] != SymbolTraits::first_symbol  )
					break;
			}
		}
		while( !SymbolTraits::is_valid( value_ ) );
	}
	
	llvm::SmallString<4> value_;
};

class GlobalDepsAnalyzer;
class Inliner;

// This class is responsible for generate unique indices for a llvm::Value
/**
 * At the moment the implementation is very simple, it is supposed to become
 * much more smarter (i.e. recicle names depending on the scope) in the future.
 */
class NameGenerator
{
public:
	/**
	 * This initialize the namegenerator by collecting
	 * all the global variable names
	 */
	explicit NameGenerator( const GlobalDepsAnalyzer &, const Inliner &, bool makeReadableNames = false );
	
	llvm::StringRef getName(const llvm::Value* ) const;
	llvm::StringRef getName(const llvm::StructType* ) const;
	
	// This will be removed when we will entirely get rid of PHIs.
	uint32_t getUniqueIndexForPHI(const llvm::Function * f);

private:
	void generateCompressedNames( const GlobalDepsAnalyzer &, const Inliner & );
	void generateReadableNames( const GlobalDepsAnalyzer &, const Inliner & );
	
	std::unordered_map<const llvm::Value*, llvm::SmallString<4> > namemap;
	std::unordered_map<const llvm::Function *, uint32_t> currentUniqueIndexForPHI;
};

}

#endif //_DUETTO_NAME_GENERATOR_H
