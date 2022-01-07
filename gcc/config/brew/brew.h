/* Target Definitions for brew.
   Copyright (C) 2008-2021 Free Software Foundation, Inc.
   Contributed by Andras Tantos.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_BREW_H
#define GCC_BREW_H

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "%{!mno-crt0:crt0%O%s} crti.o%s crtbegin.o%s"

/* Provide an ENDFILE_SPEC appropriate for svr4.  Here we tack on our own
   magical crtend.o file (see crtstuff.c) which provides part of the
   support for getting C++ file-scope static object constructed before
   entering `main', followed by the normal svr3/svr4 "finalizer" file,
   which is either `gcrtn.o' or `crtn.o'.  */

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s crtn.o%s"

/* Provide a LIB_SPEC appropriate for svr4.  Here we tack on the default
   standard C library (unless we are building a shared library) and
   the simulator BSP code.  */

#undef LIB_SPEC
#define LIB_SPEC "%{!shared:%{!symbolic:-lc}}"

#undef  LINK_SPEC
#define LINK_SPEC "%{h*} %{v:-V}\
                   %{static:-Bstatic} %{shared:-shared} %{symbolic:-Bsymbolic}"

#ifndef MULTILIB_DEFAULTS
#define MULTILIB_DEFAULTS { "meb" }
#endif

/* Layout of Source Language Data Types */

#define INT_TYPE_SIZE 32
#define SHORT_TYPE_SIZE 16
#define LONG_TYPE_SIZE 32
#define LONG_LONG_TYPE_SIZE 64

#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 64
#define LONG_DOUBLE_TYPE_SIZE 64

#define DEFAULT_SIGNED_CHAR 0

#undef  SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef  PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef  WCHAR_TYPE
#define WCHAR_TYPE "unsigned int"

#undef  WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD

/* Registers...

   $pc  - a.k.a. $r0 - program counter.
   $sp  - a.k.a. $r1 - stack pointer.
   $fp  - a.k.a. $r2 - frame pointer.
   $r3  - general purpose 32-bit register.
   $r4  - general purpose 32-bit register.
   $r5  - general purpose 32-bit register.
   $r6  - general purpose 32-bit register.
   $r7  - general purpose 32-bit register.
   $r8  - general purpose 32-bit register.
   $r9  - general purpose 32-bit register.
   $r10 - general purpose 32-bit register.
   $r11 - general purpose 32-bit register.
   $r12 - general purpose 32-bit register.
   $r13 - general purpose 32-bit register.
   $r14 - general purpose 32-bit register.

*/

#define REGISTER_NAMES {            \
  "$pc",  "$sp",  "$fp",  "$r3",    \
  "$r4",  "$r5",  "$r6",  "$r7",    \
  "$r8",  "$r9",  "$r10", "$r11",   \
  "$r12", "$r13", "$r14",           \
  "?fp",  "?ap" }

#define BREW_PC     0
#define BREW_SP     1
#define BREW_FP     2
#define BREW_R3     3
#define BREW_R4     4
#define BREW_R5     5
#define BREW_R6     6
#define BREW_R7     7
#define BREW_R8     8
#define BREW_R9     9
#define BREW_R10    10
#define BREW_R11    11
#define BREW_R12    12
#define BREW_R13    13
#define BREW_R14    14
/* TODO: what are these ? */
#define BREW_QFP    15
#define BREW_QAP    16

#define FIRST_PSEUDO_REGISTER 17
#define LAST_PHYSICAL_REG BREW_R14

enum reg_class
{
  NO_REGS,
  GENERAL_REGS,
  SPECIAL_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};


#define REG_CLASS_CONTENTS \
{ { 0x00000000 }, /* Empty */                                  \
  { 0x0001FFFE }, /* $sp, $fp, $r0 to $r14; $?fp, $?ap */      \
  { 0x00000001 }, /* $pc */                                    \
  { 0x0001FFFF }  /* All registers */                          \
}

#define N_REG_CLASSES LIM_REG_CLASSES

#define REG_CLASS_NAMES {\
    "NO_REGS", \
    "GENERAL_REGS", \
    "SPECIAL_REGS", \
    "ALL_REGS" }


/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.  */
#define FIXED_REGISTERS     { 1, 1, 1, 0, /* $pc,  $sp,  $fp,  $r3  */ \
                              0, 0, 0, 0, /* $r4,  $r5,  $r6,  $r7  */ \
                              0, 0, 0, 0, /* $r8,  $r9,  $r10, $r11 */ \
                              0, 0, 0,    /* $r12, $r13, $r14       */ \
                              1, 1, }     /* $?fp, $?ap             */

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */
#define CALL_USED_REGISTERS { 1, 1, 1, 1, /* $pc,  $sp,  $fp,  $r3  */ \
                              1, 1, 1, 1, /* $r4,  $r5,  $r6,  $r7  */ \
                              0, 0, 0, 0, /* $r8,  $r9,  $r10, $r11 */ \
                              0, 0, 0,    /* $r12, $r13, $r14       */ \
                              1, 1, }     /* $?fp, $?ap             */


/* We can't copy to or from our CC register. */
#define AVOID_CCMODE_COPIES 1

/* A C expression whose value is a register class containing hard
   register REGNO.  */
#define REGNO_REG_CLASS(R) ((R == BREW_PC) ? SPECIAL_REGS : GENERAL_REGS)

/* The Overall Framework of an Assembler File */

#undef  ASM_SPEC
#define ASM_SPEC "%{!mfloat:-F} %{mno-float:-NF}"
#define ASM_COMMENT_START "#"
#define ASM_APP_ON ""
#define ASM_APP_OFF ""

#define FILE_ASM_OP     "\t.file\n"

/* Switch to the text or data segment.  */
#define TEXT_SECTION_ASM_OP  "\t.text"
#define DATA_SECTION_ASM_OP  "\t.data"

/* Assembler Commands for Alignment */

#define ASM_OUTPUT_ALIGN(STREAM,POWER) \
        fprintf (STREAM, "\t.p2align\t%d\n", POWER);

/* Output and Generation of Labels */

#define GLOBAL_ASM_OP "\t.global\t"

/* Passing Arguments in Registers */

/* A C type for declaring a variable that is used as the first
   argument of `FUNCTION_ARG' and other related values.  */
#define CUMULATIVE_ARGS unsigned int

/* Define this if the maximum size of all the outgoing args is to be
   accumulated and pushed during the prologue.  The amount can be
   found in the variable crtl->outgoing_args_size. This is later
   used in brew_compute_frame */ 
#define ACCUMULATE_OUTGOING_ARGS 1

/* A C statement (sans semicolon) for initializing the variable CUM
   for the state at the beginning of the argument list.  
   We simply count the number of registers to be passed in, so we
   initialize the count to 0 */
#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,FNDECL,N_NAMED_ARGS) \
  (CUM = 0)

/* How Scalar Function Values Are Returned */

/* STACK AND CALLING */

/* Define this macro if pushing a word onto the stack moves the stack
   pointer to a smaller address.  */
#define STACK_GROWS_DOWNWARD 1

/* Define this if the above stack space is to be considered part of the
   space allocated by the caller.  */
#define OUTGOING_REG_PARM_STACK_SPACE(FNTYPE) 1
#define STACK_PARMS_IN_REG_PARM_AREA

/* Define this if functions should assume that stack space has been
   allocated for arguments even when their values are passed in
   registers.

   The value of this macro is the size, in bytes, of the area reserved for
   arguments passed in registers.

   This space can either be allocated by the caller or be a part of the
   machine-dependent stack frame: `OUTGOING_REG_PARM_STACK_SPACE'
   says which.  */
#define FIXED_STACK_AREA 16
#define REG_PARM_STACK_SPACE(FNDECL) FIXED_STACK_AREA

/* Offset from the argument pointer ($sp for us) register to the
   first argument's address. Since the caller saves $pc, this is
   4 bytes for us. */
#define FIRST_PARM_OFFSET(F) 4

/* Define this macro to nonzero value if the addresses of local variable slots
   are at negative offsets from the frame pointer.  */
#define FRAME_GROWS_DOWNWARD 1

/* Define this macro as a C expression that is nonzero for registers that are
   used by the epilogue or the return pattern.  The stack and frame
   pointer registers are already assumed to be used as needed.  */
#define EPILOGUE_USES(R) (false)

/* A C expression whose value is RTL representing the location of the incoming
   return address at the beginning of any function, before the prologue.  This
   RTL is either a `REG', indicating that the return value is saved in `REG',
   or a `MEM' representing a location in the stack.  This enables DWARF2
   unwind info for C++ EH.
   For us, before the prologue, RA is at -4($sp).  */
#define INCOMING_RETURN_ADDR_RTX                                        \
  gen_frame_mem (Pmode, plus_constant (Pmode, stack_pointer_rtx, UNITS_PER_WORD))

/* After the prologue, RA is at -4($fp) in the current frame.  */
// TODO: this was not defined for Moxie for some reason, but was for other targets, such as i386...
/*
#define RETURN_ADDR_RTX(COUNT, FRAME)                                            \
  ((COUNT) == 0                                                                  \
   ? gen_rtx_MEM (Pmode, plus_constant (Pmode, arg_pointer_rtx, UNITS_PER_WORD)) \
   : gen_rtx_MEM (Pmode, plus_constant (Pmode, (FRAME), UNITS_PER_WORD)))
*/

/* Describe how we implement __builtin_eh_return.  */
#define EH_RETURN_DATA_REGNO(N)        ((N) < 4 ? (N+4) : INVALID_REGNUM)

/* Store the return handler into the call frame.  */
#define EH_RETURN_HANDLER_RTX                                                \
  gen_frame_mem (Pmode, plus_constant (Pmode, frame_pointer_rtx, UNITS_PER_WORD))

/* Storage Layout */
#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN ( ! TARGET_LITTLE_ENDIAN )
#define WORDS_BIG_ENDIAN ( ! TARGET_LITTLE_ENDIAN )

/* Alignment required for a function entry point, in bits.  */
#define FUNCTION_BOUNDARY 16

/* Define this macro as a C expression which is nonzero if accessing
   less than a word of memory (i.e. a `char' or a `short') is no
   faster than accessing a word of memory.  */
#define SLOW_BYTE_ACCESS 1

/* Number of storage units in a word; normally the size of a
   general-purpose register, a power of two from 1 or 8.  */
#define UNITS_PER_WORD 4

/* Define this macro to the minimum alignment enforced by hardware
   for the stack pointer on this machine.  The definition is a C
   expression for the desired alignment (measured in bits).
   
   NOTE: Since we don't have a HW stack, really this could be anything,
   but since we don't support unaligned accesses, it's safest and
   easiest to keep the stack 32-bit aligned */
#define STACK_BOUNDARY 32

/* Normal alignment required for function parameters on the stack, in
   bits.  All stack parameters receive at least this much alignment
   regardless of data type.  */
#define PARM_BOUNDARY 32

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY  32

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 32

/* The best alignment to use in cases where we have a choice.  */
#define FASTEST_ALIGNMENT 32

/* Every structures size must be a multiple of 8 bits.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* Look at the fundamental type that is used for a bit-field and use 
   that to impose alignment on the enclosing structure.
   struct s {int a:8}; should have same alignment as "int", not "char".  */
#define        PCC_BITFIELD_TYPE_MATTERS        1

/* Largest integer machine mode for structures.  If undefined, the default
   is GET_MODE_SIZE(DImode).  */
#define MAX_FIXED_MODE_SIZE 32

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)                 \
  (TREE_CODE (TYPE) == ARRAY_TYPE                   \
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode        \
   && (ALIGN) < FASTEST_ALIGNMENT ? FASTEST_ALIGNMENT : (ALIGN))
     
/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* Generating Code for Profiling */
#define FUNCTION_PROFILER(FILE,LABELNO) (abort (), 0)

/* Trampolines for Nested Functions.  */
#define TRAMPOLINE_SIZE (2 + 6 + 4 + 2 + 6)

/* Alignment required for trampolines, in bits.  */
#define TRAMPOLINE_ALIGNMENT 32

/* An alias for the machine mode for pointers.  */
#define Pmode         SImode

/* An alias for the machine mode used for memory references to
   functions being called, in `call' RTL expressions.  */
#define FUNCTION_MODE QImode

/* The two 'hard' registers for accessing the stack and the 
   frame within a function. These must be marked as 'fixed'
   in `FIXED_REGISTERS'.
*/
/* Register to use for pushing sub-function arguments. */
#define STACK_POINTER_REGNUM BREW_SP
/* Register to use for accessing function arguments. */
#define HARD_FRAME_POINTER_REGNUM BREW_FP

/* Virtual base register for access to local variables of the function.
   This will eventually be resolved to fp or sp. */
#define FRAME_POINTER_REGNUM BREW_QFP

/* Virtual base register to access function arguments.
   This will eventually be resolved to fp or sp. */
#define ARG_POINTER_REGNUM BREW_QAP


/* Definitions for register eliminations.

   We have two registers that can be eliminated on the brew.  First, the
   frame pointer register can often be eliminated in favor of the stack
   pointer register.  Secondly, the argument pointer register can always be
   eliminated; it is replaced with either the stack or frame pointer.  */

/* This is an array of structures.  Each structure initializes one pair
   of eliminable registers.  The "from" register number is given first,
   followed by "to".  Eliminations of the same "from" register are listed
   in order of preference.  */
/* FIXME: Moxie only defined these to be eliminated towards $fp, which would
   probably mean that $fp is never eliminated. That, I think is a bit too
   pessimistic, and the Alpha version (replicated here) is a more optimal
   decision, however for that to work, brew_initial_elimination_offset
   would need to be updated too. */
/*
#define ELIMINABLE_REGS                              \
{{ ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},        \
 { ARG_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM},   \
 { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},      \
 { FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM}}
*/

#define ELIMINABLE_REGS                              \
{{ ARG_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM},   \
 { FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM}}

/* This macro returns the initial difference between the specified pair
   of registers.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)                          \
  do {                                                                        \
    (OFFSET) = brew_initial_elimination_offset ((FROM), (TO));                \
  } while (0)

/* A C expression that is nonzero if REGNO is the number of a hard
   register in which function arguments are sometimes passed.  */
#define FUNCTION_ARG_REGNO_P(r) (r >= BREW_R4 && r <= BREW_R7)

/* A macro whose definition is the name of the class to which a valid
   base register must belong.  A base register is one used in an
   address which is the register value plus a displacement.  */
#define BASE_REG_CLASS GENERAL_REGS

#define INDEX_REG_CLASS NO_REGS

#define HARD_REGNO_OK_FOR_BASE_P(NUM) \
  ((unsigned) (NUM) < FIRST_PSEUDO_REGISTER \
   && (REGNO_REG_CLASS(NUM) == GENERAL_REGS \
       || (NUM) == HARD_FRAME_POINTER_REGNUM))

/* A C expression which is nonzero if register number NUM is suitable
   for use as a base register in operand addresses.  */
#ifdef REG_OK_STRICT
#define REGNO_OK_FOR_BASE_P(NUM)                 \
  (HARD_REGNO_OK_FOR_BASE_P(NUM)                  \
   || HARD_REGNO_OK_FOR_BASE_P(reg_renumber[(NUM)]))
#else
#define REGNO_OK_FOR_BASE_P(NUM)                 \
  ((NUM) >= FIRST_PSEUDO_REGISTER || HARD_REGNO_OK_FOR_BASE_P(NUM))
#endif

/* A C expression which is nonzero if register number NUM is suitable
   for use as an index register in operand addresses.  */
#define REGNO_OK_FOR_INDEX_P(NUM) BREW_FP

/* The maximum number of bytes that a single instruction can move
   quickly between memory and registers or between two memory
   locations.  */
#define MOVE_MAX 4

/* Move insns will sign extend SImode and CCmode
   moves.  All other references are zero extended.  */
/* NOTE: Moxie originally only had ZERO_EXTEND modes */
#define LOAD_EXTEND_OP(MODE) \
  (((MODE) == SImode || (MODE) == CCmode) \
   ? SIGN_EXTEND : ZERO_EXTEND)

/* A number, the maximum number of registers that can appear in a
   valid memory address.  */
#define MAX_REGS_PER_ADDRESS 1

/* An alias for a machine mode name.  This is the machine mode that
   elements of a jump-table should have.  */
#define CASE_VECTOR_MODE SImode

/* Run-time Target Specification */

#define TARGET_CPU_CPP_BUILTINS() \
  { \
    builtin_define_std ("brew");                        \
    builtin_define_std ("BREW");                        \
    builtin_define ("__BREW_LITTLE_ENDIAN__");          \
  }

#define HAS_LONG_UNCOND_BRANCH true

#endif /* GCC_BREW_H */
