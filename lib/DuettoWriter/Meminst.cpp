//===-- Meminst.cpp - The Duetto JavaScript generator ---------------------===//
//
//                     Duetto: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Duetto/Writer.h"
#include "llvm/Duetto/Utility.h"

using namespace duetto;
using namespace llvm;


void DuettoWriter::compileAllocation(const DynamicAllocInfo & info)
{
	assert (info.isValidAlloc());

	Type * t = info.getCastedType()->getElementType();

	uint32_t typeSize = targetData.getTypeAllocSize(t);
	
	if (info.useTypedArray())
	{
		stream << "new ";
		compileTypedArrayType(t);
		stream << '(';
		
		if(info.getNumberOfElementsArg())
			compileOperand(info.getNumberOfElementsArg());
		else if( !info.sizeIsRuntime() )
		{
			uint32_t allocatedSize = getIntFromValue( info.getByteSizeArg() );
			uint32_t numElem = (allocatedSize+typeSize-1)/typeSize;
			stream << numElem;
		}
		else
		{
			compileOperand( info.getByteSizeArg() );
			stream << '/' << typeSize;
		}
		stream << ')';
	}
	else if (info.useCreateArrayFunc() )
	{
		assert( t->isStructTy() );
		StructType* st = cast<StructType>(t);
		
		assert( globalDeps.dynAllocArrays().count(st) );
		
		stream << "createArray" << namegen.getName(st) << '(';
		if( info.getNumberOfElementsArg() )
			compileOperand( info.getNumberOfElementsArg() );
		else
		{
			compileOperand( info.getByteSizeArg() );
			stream << '/' << typeSize;
		}
		stream << ')';
	}
	else if (info.useCreatePointerArrayFunc() )
	{
		stream << "createPointerArray(";
		if( info.getNumberOfElementsArg() )
			compileOperand( info.getNumberOfElementsArg() );
		else
		{
			compileOperand( info.getByteSizeArg() );
			stream << '/' << typeSize;
		}
		stream << ')';
	
		assert( globalDeps.needCreatePointerArray() );
	}
	else if (!info.sizeIsRuntime() )
	{
		// Create a plain array
		const Value * numberOfElems = info.getNumberOfElementsArg();
		
		//NOTE should we use uint32_t here? Probably not, but need to fix getIntFromValue too!
		uint32_t numElem;
		
		if (numberOfElems)
			numElem = getIntFromValue( numberOfElems );
		else
		{
			assert( isa<ConstantInt>( info.getByteSizeArg() ) );
			uint32_t allocatedSize = getIntFromValue( info.getByteSizeArg() );

			numElem = (allocatedSize+typeSize-1)/typeSize;
		}
		
		stream << '[';
		for(uint32_t i = 0; i < numElem;i++)
		{
			compileType(t, LITERAL_OBJ);
			if((i+1) < numElem)
				stream << ",";
		}
		stream << ']';
	}
	else
	{
		llvm::errs() << "Allocating type " << *t << "\n";
		llvm::report_fatal_error("Unsupported type in allocation", false);
	}
}

void DuettoWriter::compileMove(const Value* dest, const Value* src, const Value* size)
{
	//TODO: Optimize the checks if possible
	//Check if they are inside the same memory island
	stream << "if(";
	Type* lastTypeDest=compileObjectForPointer(dest, NORMAL);
	stream << "===";
	Type* lastTypeSrc=compileObjectForPointer(src, NORMAL);
	//If so they may overlap, check and use reverse copy if needed
	stream << "&&";
	bool notFirst=compileOffsetForPointer(dest,lastTypeDest);
	if(!notFirst)
		stream << '0';
	stream << ">";
	notFirst=compileOffsetForPointer(src,lastTypeSrc);
	if(!notFirst)
		stream << '0';
	stream << "){" << NewLine;
	//Destination is after source, copy backward
	compileMemFunc(dest, src, size, BACKWARD);
	stream << "}else{";
	//Destination is before source, copy forward
	compileMemFunc(dest, src, size, FORWARD);
	stream << "}" << NewLine;
}

/* Method that handles memcpy, memset and memmove.
 * If src is not NULL present a copy operation is done using the supplied direction.
 * memset is handled by passing a NULL src and setting resetValue as needed. direction should be FORWARD */
void DuettoWriter::compileMemFunc(const Value* dest, const Value* src, const Value* size,
		COPY_DIRECTION copyDirection)
{
	// HACK screw you, bitcast!
// 	if ( isBitCast(dest) )
// 	{
// 		assert( isa<User>(dest) );
// 		const User * u = cast<User>(dest);
// 		dest = u->getOperand(0);
// 	}
// 	if ( isBitCast(src) )
// 	{
// 		assert( isa<User>(src) );
// 		const User * u = cast<User>(src);
// 		src = u->getOperand(0);
// 	}
	
	Type* destType=dest->getType();

	Type* pointedType = cast<PointerType>(destType)->getElementType();
	if(TypeSupport::isUnion(pointedType))
	{
		//We can use the natural i8*, since the union will have already an allocated
		//typed array when it has been casted to i8*
		pointedType = destType->getPointerElementType();
	}
	uint32_t typeSize = targetData.getTypeAllocSize(pointedType);

	//Check that the number of element is not zero
	if(isa<ConstantInt>(size))
	{
		uint32_t allocatedSize = getIntFromValue(size);
		uint32_t numElem = (allocatedSize+typeSize-1)/typeSize;
		if(numElem==0)
			return;
	}
	else
	{
		//Compute number of elements at runtime
		stream << "var __numElem__=";
		compileOperand(size);
		stream << '/' << typeSize;
		//Make sure to close this if below
		stream << ';' << NewLine << "if(__numElem__!=0)" << NewLine << '{';
	}

	//The first element is copied directly, to support complete objects
	//In the BACKWARD case we need to copy the first as the last element
        //and we do this below
	if(copyDirection==RESET)
		compileResetRecursive("", dest, src, pointedType, NULL);
	else if(copyDirection==FORWARD)
		compileCopyRecursive("", dest, src, pointedType, NULL);

	//The rest is compiled using a for loop, or native TypedArray set operator

	//NOTE: For constant values we can stop code generation here
	//For the dynamic case we still need to close the if below
	if(isa<ConstantInt>(size))
	{
		uint32_t allocatedSize = getIntFromValue(size);
		uint32_t numElem = (allocatedSize+typeSize-1)/typeSize;

		if(numElem==1)
			return;
	}

	Type* lastTypeSrc = NULL;
	Type* lastTypeDest = NULL;
	//Prologue: Construct the first part, up to using the size
	if(copyDirection!=RESET && types.isTypedArrayType(pointedType))
	{
		// The semantics of set is memmove like, no need to care about direction
		lastTypeDest=compileObjectForPointer(dest, NORMAL);
		stream << ".set(";
		lastTypeSrc=compileObjectForPointer(src, NORMAL);
		//We need to get a subview of the source
		stream << ".subarray(";
		bool notFirst=compileOffsetForPointer(src,lastTypeSrc);
		if(!notFirst)
			stream << '0';
		stream << ',';
		notFirst=compileOffsetForPointer(src,lastTypeSrc);
		if(notFirst)
			stream << '+';
	}
	else
	{
		//memset is always handled using the for loop
		if(copyDirection == FORWARD || copyDirection == RESET)
			stream << "for(var __i__=1;__i__<";
		else
			stream << "for(var __i__=";
	}

	// Use the size
	if(isa<ConstantInt>(size))
	{
		uint32_t allocatedSize = getIntFromValue(size);
		uint32_t numElem = (allocatedSize+typeSize-1)/typeSize;

		stream << numElem;
	}
	else
	{
		stream << "__numElem__";
	}

	//Epilogue: Write the code after the size
	if(copyDirection!=RESET && types.isTypedArrayType(pointedType))
	{
		stream << "),";
		bool notFirst=compileOffsetForPointer(dest,lastTypeDest);
		if(!notFirst)
			stream << '0';
		stream << ");" << NewLine;
	}
	else
	{
		if(copyDirection == FORWARD || copyDirection == RESET)
			stream	<< ";__i__++){" << NewLine;
		else
			stream << "-1;__i__>0;__i__--){" << NewLine;

		if(copyDirection==RESET)
			compileResetRecursive("", dest, src, pointedType,"__i__");
		else
			compileCopyRecursive("", dest, src, pointedType,"__i__");
		stream << NewLine << '}';
	}

	//The first element must be copied last in the backward case
	if(copyDirection==BACKWARD)
		compileCopyRecursive("", dest, src, pointedType, NULL);

	if(!isa<ConstantInt>(size))
	{
		//Close the if for the '0' case
		stream << NewLine << '}';
	}
}

void DuettoWriter::compileCopyRecursive(const std::string& baseName, const Value* baseDest,
		const Value* baseSrc, Type* currentType, const char* namedOffset)
{
	switch(currentType->getTypeID())
	{
		case Type::IntegerTyID:
		case Type::FloatTyID:
		case Type::DoubleTyID:
		case Type::PointerTyID:
		{
			compileDereferencePointer(baseDest, NULL, namedOffset);
			stream << baseName << " = ";
			compileDereferencePointer(baseSrc, NULL, namedOffset);
			stream << baseName << ';' << NewLine;
			break;
		}
		case Type::StructTyID:
		{
			if(TypeSupport::isUnion(currentType))
			{
				stream << "var __tmp__=new Int8Array(";
				compileDereferencePointer(baseDest, NULL, namedOffset);
				stream << baseName << ");" << NewLine;
				stream << "__tmp__.set(";
				stream << "new Int8Array(";
				compileDereferencePointer(baseSrc, NULL, namedOffset);
				stream << baseName << "));" << NewLine;
				break;
			}
			const StructType* st=cast<StructType>(currentType);
			StructType::element_iterator E=st->element_begin();
			StructType::element_iterator EE=st->element_end();
			uint32_t offset=0;
			for(;E!=EE;++E)
			{
				char buf[16];
				snprintf(buf,16,".a%u",offset);
				compileCopyRecursive(baseName+buf, baseDest, baseSrc, *E, namedOffset);
				offset++;
			}
			break;
		}
		case Type::ArrayTyID:
		{
			const ArrayType* at=cast<ArrayType>(currentType);
			char buf[16];
			for(uint64_t i=0;i<at->getNumElements();i++)
			{
				snprintf(buf,16,"[%lu]",i);
				compileCopyRecursive(baseName+buf, baseDest, baseSrc, at->getElementType(), namedOffset);
			}
			break;
		}
		default:
			llvm::errs() << "Support type in copy ";
			currentType->dump();
			llvm::errs() << '\n';
	}
}

void DuettoWriter::compileResetRecursive(const std::string& baseName, const Value* baseDest,
		const Value* resetValue, Type* currentType, const char* namedOffset)
{
	switch(currentType->getTypeID())
	{
		case Type::IntegerTyID:
		{
			compileDereferencePointer(baseDest, NULL, namedOffset);
			stream << baseName << " = ";
			if( isa<Constant>(resetValue))
			{
				uint8_t constResetValue = getIntFromValue(resetValue);
				char buf[11];
				buf[10]=0;
				if(currentType->getIntegerBitWidth()==8)
					snprintf(buf,10,"0x%x",constResetValue);
				else if(currentType->getIntegerBitWidth()==16)
					snprintf(buf,10,"0x%x%x",constResetValue,constResetValue);
				else if(currentType->getIntegerBitWidth()==32)
				{
					snprintf(buf,10,"0x%x%x%x%x",
						constResetValue,constResetValue,constResetValue,constResetValue);
				}
				else
					llvm::report_fatal_error("Unsupported values for memset", false);
				stream << buf;
			}
			else
			{
				if(currentType->getIntegerBitWidth()!=8)
					llvm::report_fatal_error("Unsupported values for memset", false);
				compileOperand(resetValue);
			}
			stream << ';' << NewLine;
			break;
		}
		case Type::FloatTyID:
		case Type::DoubleTyID:
		{
			compileDereferencePointer(baseDest, NULL, namedOffset);
			if(!isa<Constant>(resetValue) || getIntFromValue(resetValue) != 0)
				llvm::report_fatal_error("Unsupported values for memset", false);
			stream << baseName << " = 0;" << NewLine;
			break;
		}
		case Type::PointerTyID:
		{
			compileDereferencePointer(baseDest, NULL, namedOffset);
			if(!isa<Constant>(resetValue) || getIntFromValue(resetValue) != 0)
				llvm::report_fatal_error("Unsupported values for memset", false);
			//Pointers to client objects must use a normal null
			Type* pointedType = currentType->getPointerElementType();
			stream << baseName << " = ";
			if(types.isClientType(pointedType))
				stream << "null";
			else
				stream << "nullObj";
			stream << ';' << NewLine;
			break;
		}
		case Type::StructTyID:
		{
			if(TypeSupport::isUnion(currentType))
			{
				stream << "var __tmp__=new Int8Array(";
				compileDereferencePointer(baseDest, NULL, namedOffset);
				stream << baseName << ");" << NewLine;
				stream << "for(var __i__=0;__i__<__tmp__.length;__i__++) __tmp__[__i__]=0;" << NewLine;
				break;
			}
			const StructType* st= cast<StructType>(currentType);
			StructType::element_iterator E=st->element_begin();
			StructType::element_iterator EE=st->element_end();
			uint32_t offset=0;
			for(;E!=EE;++E)
			{
				char buf[16];
				snprintf(buf,16,".a%u",offset);
				compileResetRecursive(baseName+buf, baseDest, resetValue, *E, namedOffset);
				offset++;
			}
			break;
		}
		case Type::ArrayTyID:
		{
			const ArrayType* at=cast<ArrayType>(currentType);
			char buf[16];
			for(uint64_t i=0;i<at->getNumElements();i++)
			{
				snprintf(buf,16,"[%lu]",i);
				compileResetRecursive(baseName+buf, baseDest, resetValue, at->getElementType(), namedOffset);
			}
			break;
		}
		default:
			llvm::errs() << "Support type in reset ";
			currentType->dump();
			llvm::errs() << '\n';
	}
}

void DuettoWriter::compileFree(const Value* obj)
{
	//TODO: Clean up class related data structures
}
