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

#include "llvm/Duetto/Utility.h"
#include "llvm/Duetto/Writer.h"

using namespace llvm;
using namespace duetto;

void DuettoWriter::compileSignedInteger(const llvm::Value* v)
{
	//We anyway have to use 32 bits for sign extension to work
	uint32_t shiftAmount = 32-v->getType()->getIntegerBitWidth();
	if(shiftAmount==0)
	{
		//Use simpler code
		stream << '(';
		compileOperand(v);
		stream << ">> 0)";
	}
	else
	{
		stream << "((";
		compileOperand(v);
		stream << "<<" << shiftAmount << ")>>" << shiftAmount << ')';
	}
}

void DuettoWriter::compileUnsignedInteger(const llvm::Value* v)
{
	//We anyway have to use 32 bits for sign extension to work
	uint32_t initialSize = v->getType()->getIntegerBitWidth();
	stream << '(';
	if(initialSize == 32)
	{
		//Use simpler code
		compileOperand(v);
		stream << ">>> 0)";
	}
	else
	{
		compileOperand(v);
		stream << " & " << getMaskForBitWidth(initialSize) << ')';
	}
}

void DuettoWriter::compilePredicate(CmpInst::Predicate p)
{
	switch(p)
	{
		case CmpInst::FCMP_UEQ: //TODO: fix this, if an operand is NaN LLVM expects false,
		case CmpInst::FCMP_OEQ:
		case CmpInst::ICMP_EQ:
			stream << " === ";
			break;
		case CmpInst::FCMP_UNE: //The undordered case correspond to the usual JS operator
					//See ECMA-262, Section 11.9.6
		case CmpInst::ICMP_NE:
			stream << " !== ";
			break;
		case CmpInst::FCMP_OGT: //TODO: fix this, if an operand is NaN LLVM expects false,
		case CmpInst::FCMP_UGT:	//but JS returns undefined. Adding ==true after the whole expression
					//should work
		case CmpInst::ICMP_SGT:
		case CmpInst::ICMP_UGT: //TODO: To support unsigned we need to add casts around the ops
			stream << " > ";
			break;
		case CmpInst::FCMP_UGE:
		case CmpInst::FCMP_OGE:
		case CmpInst::ICMP_SGE:
		case CmpInst::ICMP_UGE:
			stream << " >= ";
			break;
		case CmpInst::FCMP_OLT: //TODO: fix this, if an operand is NaN LLVM expects false,
		case CmpInst::FCMP_ULT:	//but JS returns undefined. Adding ==true after the whole expression
					//should work
		case CmpInst::ICMP_SLT:
		case CmpInst::ICMP_ULT: //TODO: To support unsigned we need to add casts around the ops
			stream << " < ";
			break;
		case CmpInst::FCMP_ULE:
		case CmpInst::FCMP_OLE:
		case CmpInst::ICMP_SLE:
		case CmpInst::ICMP_ULE:
			stream << " <= ";
			break;
		default:
			llvm::errs() << "Support predicate " << p << '\n';
	}
}

void DuettoWriter::compileOperandForIntegerPredicate(const Value* v, CmpInst::Predicate p)
{
	if(CmpInst::isUnsigned(p))
		compileUnsignedInteger(v);
	else
		compileSignedInteger(v);
}

void DuettoWriter::compileIntegerComparison(const llvm::Value* lhs, const llvm::Value* rhs, CmpInst::Predicate p)
{
	if(lhs->getType()->isPointerTy())
	{
		if(p==CmpInst::ICMP_EQ || p==CmpInst::ICMP_NE)
			compileEqualPointersComparison(lhs, rhs, p);
		else
		{
			//Comparison on different bases is anyway undefined, so ignore them
			Type* lastType1=compileObjectForPointer(lhs, DRY_RUN);
			Type* lastType2=compileObjectForPointer(rhs, DRY_RUN);
			bool notFirst=compileOffsetForPointer(lhs,lastType1);
			if(!notFirst)
				stream << '0';
			compilePredicate(p);
			notFirst=compileOffsetForPointer(rhs,lastType2);
			if(!notFirst)
				stream << '0';
		}
	}
	else
	{
		compileOperandForIntegerPredicate(lhs,p);
		compilePredicate(p);
		compileOperandForIntegerPredicate(rhs,p);
	}
}

void DuettoWriter::compilePtrToInt(const llvm::Value* v)
{
	Type* lastType = compileObjectForPointer(v, DRY_RUN);
	stream << '(';
	bool ret=compileOffsetForPointer(v, lastType);
	if(!ret)
		stream << '0';
	stream << ')';
}

void DuettoWriter::compileSubtraction(const llvm::Value* lhs, const llvm::Value* rhs)
{
	//Integer subtraction
	//TODO: optimize negation
	stream << "((";
	compileOperand(lhs);
	stream << " - ";
	compileOperand(rhs);
	stream << ')';
	if(types.isI32Type(lhs->getType()))
		stream << ">> 0";
	else
		stream << "& " << getMaskForBitWidth(lhs->getType()->getIntegerBitWidth());
	stream << ')';
}
