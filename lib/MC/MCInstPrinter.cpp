//===- MCInstPrinter.cpp - Convert an MCInst to target assembly syntax ----===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/MC/MCInstPrinter.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/raw_ostream.h"
#include <cinttypes>
#include <cstdint>

using namespace llvm;

void llvm::dumpBytes(ArrayRef<uint8_t> bytes, raw_ostream &OS) {
  static const char hex_rep[] = "0123456789abcdef";
  bool First = true;
  for (char i: bytes) {
    if (First)
      First = false;
    else
      OS << ' ';
    OS << hex_rep[(i & 0xF0) >> 4];
    OS << hex_rep[i & 0xF];
  }
}

MCInstPrinter::~MCInstPrinter() = default;

/// getOpcodeName - Return the name of the specified opcode enum (e.g.
/// "MOV32ri") or empty if we can't resolve it.
StringRef MCInstPrinter::getOpcodeName(unsigned Opcode) const {
  return MII.getName(Opcode);
}

void MCInstPrinter::printRegName(raw_ostream &OS, unsigned RegNo) const {
  llvm_unreachable("Target should implement this");
}

void MCInstPrinter::printAnnotation(raw_ostream &OS, StringRef Annot) {
  if (!Annot.empty()) {
    if (CommentStream) {
      (*CommentStream) << Annot;
      // By definition (see MCInstPrinter.h), CommentStream must end with
      // a newline after each comment.
      if (Annot.back() != '\n')
        (*CommentStream) << '\n';
    } else
      OS << " " << MAI.getCommentString() << " " << Annot;
  }
}

/// Utility functions to make adding mark ups simpler.
StringRef MCInstPrinter::markup(StringRef s) const {
  if (getUseMarkup())
    return s;
  else
    return "";
}

// For asm-style hex (e.g. 0ffh) the first digit always has to be a number.
static bool needsLeadingZero(uint64_t Value)
{
  while (Value)
  {
    uint64_t digit = (Value >> 60) & 0xf;
    if (digit != 0)
      return (digit >= 0xa);
    Value <<= 4;
  }
  return false;
}

format_object<int64_t> MCInstPrinter::formatDec(int64_t Value) const {
  return format("%" PRId64, Value);
}

format_object<int64_t> MCInstPrinter::formatHex(int64_t Value) const {
  switch(PrintHexStyle) {
  case HexStyle::C:
    if (Value < 0)
      return format("-0x%" PRIx64, -Value);
    else
      return format("0x%" PRIx64, Value);
  case HexStyle::Asm:
    if (Value < 0) {
      if (needsLeadingZero((uint64_t)(-Value)))
        return format("-0%" PRIx64 "h", -Value);
      else
        return format("-%" PRIx64 "h", -Value);
    } else {
      if (needsLeadingZero((uint64_t)(Value)))
        return format("0%" PRIx64 "h", Value);
      else
        return format("%" PRIx64 "h", Value);
    }
  }
  llvm_unreachable("unsupported print style");
}

format_object<uint64_t> MCInstPrinter::formatHex(uint64_t Value) const {
  switch(PrintHexStyle) {
  case HexStyle::C:
     return format("0x%" PRIx64, Value);
  case HexStyle::Asm:
    if (needsLeadingZero(Value))
      return format("0%" PRIx64 "h", Value);
    else
      return format("%" PRIx64 "h", Value);
  }
  llvm_unreachable("unsupported print style");
}
