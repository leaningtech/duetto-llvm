; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc < %s -mtriple=i686-- -O3 | FileCheck %s

; Tests for using callbr as an asm-goto wrapper

; Test 1 - fallthrough label gets removed, but the fallthrough code that is
; unreachable due to asm ending on a jmp is still left in.
define i32 @test1(i32 %a) {
; CHECK-LABEL: test1:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    movl {{[0-9]+}}(%esp), %eax
; CHECK-NEXT:    addl $4, %eax
; CHECK-NEXT:    #APP
; CHECK-NEXT:    xorl %eax, %eax
; CHECK-NEXT:    jmp .LBB0_2
; CHECK-NEXT:    #NO_APP
; CHECK-NEXT:  .LBB0_1: # %normal
; CHECK-NEXT:    xorl %eax, %eax
; CHECK-NEXT:    retl
; CHECK-NEXT:  .Ltmp0: # Block address taken
; CHECK-NEXT:  .LBB0_2: # %fail
; CHECK-NEXT:    movl $1, %eax
; CHECK-NEXT:    retl
entry:
  %0 = add i32 %a, 4
  callbr void asm "xorl $0, $0; jmp ${1:l}", "r,X,~{dirflag},~{fpsr},~{flags}"(i32 %0, i8* blockaddress(@test1, %fail)) to label %normal [label %fail]

normal:
  ret i32 0

fail:
  ret i32 1
}

; Test 2 - callbr terminates an unreachable block, function gets simplified
; to a trivial zero return.
define i32 @test2(i32 %a) {
; CHECK-LABEL: test2:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    xorl %eax, %eax
; CHECK-NEXT:    retl
entry:
  br label %normal

unreachableasm:
  %0 = add i32 %a, 4
  callbr void asm sideeffect "xorl $0, $0; jmp ${1:l}", "r,X,~{dirflag},~{fpsr},~{flags}"(i32 %0, i8* blockaddress(@test2, %fail)) to label %normal [label %fail]

normal:
  ret i32 0

fail:
  ret i32 1
}


; Test 3 - asm-goto implements a loop. The loop gets recognized, but many loop
; transforms fail due to canonicalization having callbr exceptions. Trivial
; blocks at labels 1 and 3 also don't get simplified due to callbr.
define dso_local i32 @test3(i32 %a) {
; CHECK-LABEL: test3:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:  .Ltmp1: # Block address taken
; CHECK-NEXT:  .LBB2_1: # %label01
; CHECK-NEXT:    # =>This Loop Header: Depth=1
; CHECK-NEXT:    # Child Loop BB2_2 Depth 2
; CHECK-NEXT:    # Child Loop BB2_3 Depth 3
; CHECK-NEXT:    # Child Loop BB2_4 Depth 4
; CHECK-NEXT:  .Ltmp2: # Block address taken
; CHECK-NEXT:  .LBB2_2: # %label02
; CHECK-NEXT:    # Parent Loop BB2_1 Depth=1
; CHECK-NEXT:    # => This Loop Header: Depth=2
; CHECK-NEXT:    # Child Loop BB2_3 Depth 3
; CHECK-NEXT:    # Child Loop BB2_4 Depth 4
; CHECK-NEXT:    addl $4, {{[0-9]+}}(%esp)
; CHECK-NEXT:  .Ltmp3: # Block address taken
; CHECK-NEXT:  .LBB2_3: # %label03
; CHECK-NEXT:    # Parent Loop BB2_1 Depth=1
; CHECK-NEXT:    # Parent Loop BB2_2 Depth=2
; CHECK-NEXT:    # => This Loop Header: Depth=3
; CHECK-NEXT:    # Child Loop BB2_4 Depth 4
; CHECK-NEXT:    .p2align 4, 0x90
; CHECK-NEXT:  .Ltmp4: # Block address taken
; CHECK-NEXT:  .LBB2_4: # %label04
; CHECK-NEXT:    # Parent Loop BB2_1 Depth=1
; CHECK-NEXT:    # Parent Loop BB2_2 Depth=2
; CHECK-NEXT:    # Parent Loop BB2_3 Depth=3
; CHECK-NEXT:    # => This Inner Loop Header: Depth=4
; CHECK-NEXT:    #APP
; CHECK-NEXT:    jmp .LBB2_1
; CHECK-NEXT:    jmp .LBB2_2
; CHECK-NEXT:    jmp .LBB2_3
; CHECK-NEXT:    #NO_APP
; CHECK-NEXT:  .LBB2_5: # %normal0
; CHECK-NEXT:    # in Loop: Header=BB2_4 Depth=4
; CHECK-NEXT:    #APP
; CHECK-NEXT:    jmp .LBB2_1
; CHECK-NEXT:    jmp .LBB2_2
; CHECK-NEXT:    jmp .LBB2_3
; CHECK-NEXT:    jmp .LBB2_4
; CHECK-NEXT:    #NO_APP
; CHECK-NEXT:  .LBB2_6: # %normal1
; CHECK-NEXT:    movl {{[0-9]+}}(%esp), %eax
; CHECK-NEXT:    retl
entry:
  %a.addr = alloca i32, align 4
  store i32 %a, i32* %a.addr, align 4
  br label %label01

label01:                                          ; preds = %normal0, %label04, %entry
  br label %label02

label02:                                          ; preds = %normal0, %label04, %label01
  %0 = load i32, i32* %a.addr, align 4
  %add = add nsw i32 %0, 4
  store i32 %add, i32* %a.addr, align 4
  br label %label03

label03:                                          ; preds = %normal0, %label04, %label02
  br label %label04

label04:                                          ; preds = %normal0, %label03
  callbr void asm sideeffect "jmp ${0:l}; jmp ${1:l}; jmp ${2:l}", "X,X,X,~{dirflag},~{fpsr},~{flags}"(i8* blockaddress(@test3, %label01), i8* blockaddress(@test3, %label02), i8* blockaddress(@test3, %label03))
          to label %normal0 [label %label01, label %label02, label %label03]

normal0:                                          ; preds = %label04
  callbr void asm sideeffect "jmp ${0:l}; jmp ${1:l}; jmp ${2:l}; jmp ${3:l}", "X,X,X,X,~{dirflag},~{fpsr},~{flags}"(i8* blockaddress(@test3, %label01), i8* blockaddress(@test3, %label02), i8* blockaddress(@test3, %label03), i8* blockaddress(@test3, %label04))
          to label %normal1 [label %label01, label %label02, label %label03, label %label04]

normal1:                                          ; preds = %normal0
  %1 = load i32, i32* %a.addr, align 4
  ret i32 %1
}
