//===-- Opcodes.cpp - The Duetto JavaScript generator ---------------------===//
//
//                     Duetto: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Duetto/NameGenerator.h"
#include "llvm/Duetto/GlobalDepsAnalyzer.h"
#include "llvm/Duetto/Inliner.h"
#include "llvm/Duetto/Utility.h"
#include <sstream>

using namespace llvm;

namespace duetto {

/**
 * Note: this describes the valid *C++* identifiers.
 * 
 * Iterate over all the valid JS identifier is much more complicated , because JS uses unicode.
 * Reference for valid JS identifiers:
*  http://stackoverflow.com/questions/1661197/valid-characters-for-javascript-variable-names
*/
struct JSSymbols
{
	enum : char {first_symbol = 'a' };
	static char next( char c )
	{
		return  c == '_' ? 'a' :
			c == 'z' ? 'A' :
			c == 'Z' ? '0' :
			c == '9' ? '_' :
			++c;
	}
	
	static bool is_valid( StringRef s )
	{
		return  !s.empty() && // Can not be empty
			! std::isdigit(s.front() ) && // Can not start with a digit
			! std::all_of( s.begin(), s.end(), [](char c) { return c == '_'; }) && // Can not be all '_'
			! reserved_names.count(s) && // Can not be a reserved keyword
			s != "nullObj" &&
			s != "null" &&
			s != "nullArray" &&
			! s.startswith("_t"); //_t reserved for argument kind conversion
			
			// Technically we should add also the duetto-generated functions, like createArray, etc
			// but we do not expect to ever have a program with ~52^10 names.
	}
	
	static const std::set< StringRef > reserved_names;
};

/**
 * From https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Reserved_Words
 */
const std::set< StringRef > JSSymbols::reserved_names = {
	"abstract",
	"boolean",
	"break",
	"byte",
	"case",
	"catch",
	"char",
	"class",
	"const",
	"continue",
	"debugger",
	"default",
	"delete",
	"do",
	"double",
	"else",
	"enum",
	"export",
	"extends",
	"final",
	"finally",
	"float",
	"for",
	"function",
	"goto",
	"if",
	"implements",
	"import",
	"in",
	"instanceof",
	"int",
	"interface",
	"let",
	"long",
	"native",
	"new",
	"package",
	"private",
	"protected",
	"public",
	"return",
	"short",
	"static",
	"super",
	"switch",
	"synchronized",
	"this",
	"throw",
	"throws",
	"transient",
	"try",
	"typeof",
	"var",
	"void",
	"volatile",
	"while",
	"with",
	"yield"
};

static std::string filter_llvm_name(StringRef s, bool isGlobal)
{
	std::ostringstream stream;
	//Add an '_' or 'L' to skip reserved names
	stream.write( isGlobal ? "_" : "L", 1);
	
	for ( char c : s )
	{
		//We need to escape invalid chars
		switch(c)
		{
			case '.':
				stream.write("_p",2);
				break;
			case '-':
				stream.write("_m",2);
				break;
			case ':':
				stream.write("_c",2);
				break;
			case '<':
				stream.write("_l",2);
				break;
			case '>':
				stream.write("_r",2);
				break;
			case ' ':
				stream.write("_s",2);
				break;
			case '_':
				//NOTE: This may cause collisions
				stream.write("_",1);
				break;
			default:
				stream << c;
		}
	}
	
	return stream.str();
}


NameGenerator::NameGenerator(const GlobalDepsAnalyzer & globals, const Inliner & inliner, bool makeReadableNames)
{
	if ( makeReadableNames )
		generateReadableNames( globals, inliner );
	else
		generateCompressedNames( globals, inliner );
}

void NameGenerator::generateCompressedNames( const GlobalDepsAnalyzer & globals, const Inliner & inliner)
{
	auto need_name = [&inliner](const Instruction & I) -> bool
	{
		return !inliner.isInlined(&I) && I.getType()->getTypeID() != Type::VoidTyID;
	};

	typedef std::pair<int, const Value *> useValuePair;
	typedef std::pair<int, std::vector<const Value *> > useValuesPair;
	typedef std::vector<useValuesPair> useValuesVec;
	
	/**
	 * Collect the local values.
	 * 
	 * We sort them by uses, then store together those in the same position.
	 * i.e. allLocalValues[0].second will contain all the most used local values
	 * for each function, and allLocalValues[0].first will be the sum of the uses
	 * of all those local values.
	 */
	
	useValuesVec allLocalValues;
	
	for (const Function * f : globals.functionOrderedList() )
	{
		if ( f->empty() ) 
			continue;
		
		std::set< useValuePair > thisFunctionLocals;

		for (const BasicBlock & bb : *f)
			for (const Instruction & I : bb)
			{
				if ( need_name(I) )
				{
					thisFunctionLocals.emplace( I.getNumUses(), &I );
				}
			}
			
		for ( auto arg_it = f->arg_begin(); arg_it != f->arg_end(); ++arg_it )
			thisFunctionLocals.emplace( arg_it->getNumUses(), arg_it );
		
		if ( thisFunctionLocals.size() > allLocalValues.size() )
			allLocalValues.resize( thisFunctionLocals.size() );
		
		auto dst_it = allLocalValues.begin();
		
		for (auto src_it = thisFunctionLocals.rbegin(); src_it != thisFunctionLocals.rend(); ++src_it, ++dst_it )
		{
			dst_it->first += src_it->first;
			dst_it->second.push_back( src_it->second );
		}
	}
	
	assert( std::is_sorted( 
		allLocalValues.rbegin(), 
		allLocalValues.rend(),
		[] (const useValuesPair & lhs, const useValuesPair & rhs) { return lhs.first < rhs.first; } 
		) );

	/**
	 * Sort the global values by uses
	 */
	
	std::set< useValuePair, std::greater< useValuePair > > allGlobalValues;
	
	for ( const GlobalValue * GV : globals.globalOrderedList() )
		allGlobalValues.emplace( GV->getNumUses(), GV );
	
	name_iterator<JSSymbols> name_it;
	std::set< useValuePair >::const_iterator global_it = allGlobalValues.begin();
	useValuesVec::const_iterator local_it = allLocalValues.begin();
	
	for ( ; global_it != allGlobalValues.end() && local_it != allLocalValues.end(); ++name_it )
	{
		if ( global_it->first >= local_it->first )
		{
			// Assign this name to a global value
			namemap.emplace( global_it->second, *name_it );
			
			++global_it;
		}
		else
		{
			// Assign this name to all the local values
			for ( const Value * v : local_it->second )
				namemap.emplace( v, *name_it );
			
			++local_it;
		}
	}
	
	// Assign remaining vars	
	for ( ; global_it != allGlobalValues.end(); ++global_it )
		namemap.emplace( global_it->second, *name_it );
	
	for ( ; local_it != allLocalValues.end(); ++local_it )
		for ( const Value * v : local_it->second )
			namemap.emplace( v, *name_it );
}

void NameGenerator::generateReadableNames(const GlobalDepsAnalyzer & globals, const Inliner & inliner)
{
	auto need_name = [&inliner](const Instruction & I) -> bool
	{
		return !inliner.isInlined(&I) && I.getType()->getTypeID() != Type::VoidTyID;
	};
	
	for (const Function * f : globals.functionOrderedList() )
	{
		int tmpCounter = 0;

		for (const BasicBlock & bb : *f)
			for (const Instruction & I : bb)
			{
				if ( need_name(I) )
				{
					if (I.hasName() )
						namemap.emplace( &I, StringRef(filter_llvm_name(I.getName(), false) ) );
					else
						namemap.emplace( &I, StringRef( "tmp" + std::to_string(tmpCounter++) ) );
				}
			}
		
		int argCounter = 0;
		for ( auto arg_it = f->arg_begin(); arg_it != f->arg_end(); ++arg_it )
			if ( arg_it->hasName() )
				namemap.emplace( arg_it, StringRef(filter_llvm_name(arg_it->getName(), false) ) );
			else
				namemap.emplace( arg_it, StringRef( "arg" + std::to_string(argCounter++) ) );
			
		namemap.emplace( f, StringRef(filter_llvm_name( f->getName(), true ) ) );
	}
	
	for (const GlobalVariable * GV : globals.varsOrderedList() )
		namemap.emplace( GV, StringRef(filter_llvm_name( GV->getName(), true ) ) );
}

StringRef NameGenerator::getName(const llvm::Value* v) const
{
	assert (namemap.count(v) || ( v->dump(), false ) );
	return namemap.at(v);
}

StringRef NameGenerator::getName(const StructType* st) const
{
	// Stub impl
	static std::string filtered;

	filtered = filter_llvm_name(st->getName(), true);
	
	return filtered;
}

uint32_t NameGenerator::getUniqueIndexForPHI(const llvm::Function * f)
{
	return currentUniqueIndexForPHI[f]++;
}

}
