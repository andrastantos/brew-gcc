/* Target Code for brew
   Copyright (C) 2008-2021 Free Software Foundation, Inc.
   Contributed by Andras Tantos <andras@tantosonline.com>.

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

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "stringpool.h"
#include "attribs.h"
#include "df.h"
#include "regs.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "diagnostic-core.h"
#include "output.h"
#include "stor-layout.h"
#include "varasm.h"
#include "calls.h"
#include "expr.h"
#include "optabs.h"
#include "expmed.h"
#include "builtins.h"
#include "explow.h"
#include "tm_p.h"
#include "insn-config.h"
#include "recog.h"
#include "reload.h"
#include "insn-attr.h"
#include "sched-int.h"

/* This file should be included last.  */
#include "target-def.h"


#define DEBUG_MARKER do { emit_insn(gen_nop()); emit_insn(gen_nop()); emit_insn(gen_nop()); emit_insn(gen_nop()); emit_insn(gen_nop()); } while(false)
//#define DEBUG_MARKER


// Per-function machine data.
struct GTY(()) machine_function
 {
   // Number of bytes saved on the stack for callee saved registers.
   int callee_saved_reg_size;
   // Number of bytes saved on the stack for local variables.
   int local_vars_size;
   // Total SP adjustment needed. Includes padding.
   int size_for_adjusting_sp;
   // A bit-mask: 1 for each reg that was saved, 0 for ones that didn't.
   // bit0 is R0, bit 14 is R14
   uint32_t reg_save_mask;
 };

static struct machine_function *
brew_init_machine_status (void)
{
  // hopefully this zero-initializes the struct
  return ggc_cleared_alloc<machine_function>();
}


///////////////////////////////////////////////////////////////////////////
//
//
// RTL EXPANSION CODE FOR define_expand PATTERNS
//
//
///////////////////////////////////////////////////////////////////////////

/* Returns true if floating point emulation is enabled */
/* This is controlled by the -msoft-float/-mno-soft-float command-line option. */
/* Default is HW floating point (no-soft-float) */
bool brew_soft_float()
{
  return TARGET_SOFT_FLOAT != 0;
}

/* Returns the condition code for the inverted comparison (so for example LT for GT) */
/* NOTE: this is not a negated code! */
static enum rtx_code
invert_code(enum rtx_code code)
{
  switch (code) {
    case NE: return NE;
    case EQ: return EQ;
    case GE: return LE;
    case GT: return LT;
    case LE: return GE;
    case LT: return GT;
    case GEU: return LEU;
    case GTU: return LTU;
    case LEU: return GEU;
    case LTU: return GTU;
    default:
      gcc_assert(false);
  }
}

/* Emit the assembly for a conditional branch (cbranchsi4) */
const char *
brew_emit_cbranch(machine_mode mode, rtx *operands, int insn_len)
{
  /*
    operands[0]: condition
    operands[1]: first thing to compare
    operands[2]: second thing to compare
    operands[3]: branch target
  */
  enum rtx_code code = GET_CODE (operands[0]);

  if (operands[1] == CONST0_RTX(mode))
    {
      code = invert_code(code);
      rtx tmp = operands[2];
      operands[2] = operands[1];
      operands[1] = tmp;
    }
  bool compare_to_zero = operands[2] == CONST0_RTX(mode);
  /* There are certain comparisons that don't make sense
     such as an unsigned integer being less then 0.
     These are replaced by their equivalent unconditional
     branches or nops as the case may be.  The compare-with-zero
     branches (if $rA > 0, if $rA <= 0) are signed, so an unsigned
     comparison with zero becomes != 0 or == 0.  */
  if (insn_len == 4)
    {
      switch (code)
        {
        case NE: return "if %1 != %2 $pc <- %l3";
        case EQ: return "if %1 == %2 $pc <- %l3";
        case GE: return "if signed %1 >= %2 $pc <- %l3";
        case GT: return "if signed %1 > %2 $pc <- %l3";
        case LE: return "if signed %1 <= %2 $pc <- %l3";
        case LT: return "if signed %1 < %2 $pc <- %l3";
        case GEU: if (compare_to_zero) return "$pc <- %l3"; else return "if %1 >= %2 $pc <- %l3";
        case GTU: if (compare_to_zero) return "if %1 != 0 $pc <- %l3"; else return "if %1 > %2 $pc <- %l3";
        case LEU: if (compare_to_zero) return "if %1 == 0 $pc <- %l3"; else return "if %1 <= %2 $pc <- %l3";
        case LTU: if (compare_to_zero) return ""; else return "if %1 < %2 $pc <- %l3";
        default:
          gcc_unreachable ();
        }
    }
  else if (insn_len == 10)
    {
      switch (code)
        {
        case NE: return "if %1 == %2 $pc <- 1f\n\t$pc <- %l3\n1:";
        case EQ: return "if %1 != %2 $pc <- 1f\n\t$pc <- %l3\n1:";
        case GE: return "if signed %1 < %2 $pc <- 1f\n\t$pc <- %l3\n1:";
        case GT: return "if signed %1 <= %2 $pc <- 1f\n\t$pc <- %l3\n1:";
        case LE: return "if signed %1 > %2 $pc <- 1f\n\t$pc <- %l3\n1:";
        case LT: return "if signed %1 >= %2 $pc <- 1f\n\t$pc <- %l3\n1:";
        case GEU: if (compare_to_zero) return "$pc <- 1f\n\t$pc <- %l3\n1:"; else return "if %1 < %2 $pc <- 1f\n\t$pc <- %l3\n1:";
        case GTU: if (compare_to_zero) return "if %1 == 0 $pc <- 1f\n\t$pc <- %l3\n1:"; else return "if %1 <= %2 $pc <- 1f\n\t$pc <- %l3\n1:";
        case LEU: if (compare_to_zero) return "if %1 != 0 $pc <- 1f\n\t$pc <- %l3\n1:"; else return "if %1 > %2 $pc <- 1f\n\t$pc <- %l3\n1:";
        case LTU: if (compare_to_zero) return ""; else return "if %1 >= %2 $pc <- 1f\n\t$pc <- %l3\n1:";
        default:
          gcc_unreachable ();
        }
    }
  else
    {
      gcc_assert(false);
    }
}

/* Emit the assembly for a conditional branch (b<cond> patterns) */
const char *
brew_emit_bcond(machine_mode mode, int condition, bool reverse, rtx *operands)
{
  /*
  condition: a string, such as 'le', 'ne' etc. signifying the comparison to be performed
  reverse: true, if reverse comparison is to be performed (that is branch on false)
  operands[0]: condition register (always compared to 0)
  operands[1]: branch target
  */

  if (!REG_P(operands[0]))
    operands[0] = force_reg(mode, operands[0]);

  switch (condition)
    {
    case NE:  return reverse ? brew_emit_bcond(mode, EQ,  false, operands) : "if %1 != %2 $pc <- %l3";
    case EQ:  return reverse ? brew_emit_bcond(mode, NE,  false, operands) : "if %1 == %2 $pc <- %l3";
    case GE:  return reverse ? brew_emit_bcond(mode, LT,  false, operands) : "if signed %1 >= %2 $pc <- %l3";
    case GT:  return reverse ? brew_emit_bcond(mode, LE,  false, operands) : "if signed %1 > %2 $pc <- %l3";
    case LE:  return reverse ? brew_emit_bcond(mode, GT,  false, operands) : "if signed %1 <= %2 $pc <- %l3";
    case LT:  return reverse ? brew_emit_bcond(mode, GE,  false, operands) : "if signed %1 < %2 $pc <- %l3";
    case GEU: return reverse ? brew_emit_bcond(mode, LTU, false, operands) : "$pc <- %l3";
    // Unsigned: the compare-with-zero branches are signed.
    case GTU: return reverse ? brew_emit_bcond(mode, LEU, false, operands) : "if %1 != %2 $pc <- %l3";
    case LEU: return reverse ? brew_emit_bcond(mode, GTU, false, operands) : "if %1 == %2 $pc <- %l3";
    case LTU: return reverse ? brew_emit_bcond(mode, GEU, false, operands) : "";
    default:
      gcc_unreachable ();
    }
}

void brew_expand_call(machine_mode /*mode*/, rtx *operands)
{
  gcc_assert(MEM_P(operands[0]));
}

// Determines if we wanted to save/restore the specified register
// in function prologue/epilog
static bool
reg_needs_save_restore(int regno)
{
  // We need to save/restore a register if...

  // We clobber the register that should be saved across calls
  if (df_regs_ever_live_p(regno) && (!call_used_or_fixed_reg_p(regno) || regno == BREW_REG_LINK))
    return true;
  // The link register for non-leaf nodes in the call-graph
  if (regno == BREW_REG_LINK && (!crtl->is_leaf || crtl->calls_eh_return || crtl->accesses_prior_frames))
    return true;
  // The EH data registers in functions that use __builtin_eh_return: the
  // unwinder installs the handler's values by overwriting their save slots.
  if (crtl->calls_eh_return)
    for (unsigned int i = 0; EH_RETURN_DATA_REGNO (i) != INVALID_REGNUM; i++)
      if ((unsigned int) regno == EH_RETURN_DATA_REGNO (i))
        return true;
  // The frame pointer, in functions that set one up
  if (regno == BREW_REG_FP)
    return frame_pointer_needed;
  return false;
}

// Order of registers saved on the stack. Largely arbitrary, but the first two
// entries are important in functions with a frame pointer:
//     brew_dynamic_chain_address assumes $fp is in the 1st save slot.
//     brew_return_addr_rtx and brew_eh_return_handler_rtx assume $lr is in
//     the 2nd save slot.
// (Without a frame pointer $lr takes the 1st slot; nothing then looks for it
// there but the unwinders, which follow the CFI.)
static const size_t reg_frame_order[] = {
  BREW_REG_FP,
  BREW_REG_LINK,
  BREW_REG_R0,
  BREW_REG_R1,
  BREW_REG_R2,
  BREW_REG_R3,
  BREW_REG_R4,
  BREW_REG_R5,
  BREW_REG_R6,
  BREW_REG_R7,
  BREW_REG_R8,
  BREW_REG_R9,
  BREW_REG_R10,
  BREW_REG_R11,
  BREW_REG_R13
};

// Compute the size of the local area and the size to be adjusted by the
// prologue and epilogue.
static void
brew_compute_frame(void)
{
  int stack_alignment = STACK_BOUNDARY / BITS_PER_UNIT;

  // Let's start with the real frame size, and adjust with any alignment, if needed
  cfun->machine->local_vars_size = get_frame_size();
  int left_over = cfun->machine->local_vars_size % stack_alignment;
  if (left_over > 0)
    cfun->machine->local_vars_size += stack_alignment - left_over;

  cfun->machine->callee_saved_reg_size = 0;
  cfun->machine->reg_save_mask = 0;
  // Save callee-saved registers
  for (size_t r = 0; r < ARRAY_SIZE(reg_frame_order); ++r)
    {
      size_t regno = reg_frame_order[r];
      if (reg_needs_save_restore(regno))
        {
          cfun->machine->callee_saved_reg_size += 4;
          cfun->machine->reg_save_mask |= 1 << regno;
        }
    }

  cfun->machine->size_for_adjusting_sp =
    crtl->args.pretend_args_size +
    cfun->machine->local_vars_size +
    (ACCUMULATE_OUTGOING_ARGS ? (HOST_WIDE_INT) crtl->outgoing_args_size : 0);
}

// Stack is pre-decrement for push, and post-increment for pop.
// This means that $sp points to the last valid value.
// (except when in-between updates)

// This function expands the instruction sequence for a function prologue.
void
brew_expand_prologue (void)
{
  rtx insn;

  brew_compute_frame();

  if (flag_stack_usage_info)
    current_function_static_stack_size = cfun->machine->size_for_adjusting_sp;

  int save_cnt = 0;
  // Save callee-saved registers.
  // For each register we save, we'll have to decrement SP by 4 and of course
  // make sure that further references are offsetted by that much.
  for (size_t r = 0; r < ARRAY_SIZE(reg_frame_order); ++r)
    {
      size_t regno = reg_frame_order[r];
      if ((cfun->machine->reg_save_mask & (1 << regno)) != 0)
        {
          insn = emit_insn(gen_movsi(
            gen_rtx_MEM(Pmode,
              plus_constant(Pmode, stack_pointer_rtx, -4*save_cnt-4, false) // Not an in-place addition
            ),
            gen_rtx_REG(Pmode, regno)
          ));
          RTX_FRAME_RELATED_P (insn) = 1;
          ++save_cnt;
        }
    }
  gcc_assert(save_cnt*4 == cfun->machine->callee_saved_reg_size);
  // set up FP
  if (frame_pointer_needed)
    {
      insn = emit_insn(gen_movsi(
        hard_frame_pointer_rtx,
        stack_pointer_rtx
      ));
      RTX_FRAME_RELATED_P (insn) = 1;
    }
  // adjust SP
  int sp_adjust = cfun->machine->size_for_adjusting_sp + cfun->machine->callee_saved_reg_size;
  if (sp_adjust > 0)
    {
      insn = emit_insn(
        gen_subsi3(
          stack_pointer_rtx,
          stack_pointer_rtx,
          GEN_INT(sp_adjust)
        )
      );
      RTX_FRAME_RELATED_P (insn) = 1;
    }
}

// Whether a call to DECL (NULL for an indirect call) in EXP can be a sibcall.
// The generic code already rejects callees that need more stack argument
// space than the caller got.
static bool
brew_function_ok_for_sibcall(tree /*decl*/, tree /*exp*/)
{
  // __builtin_eh_return adjusts $sp in the epilogue.
  if (crtl->calls_eh_return)
    return false;
  return true;
}

void
brew_expand_epilogue(bool is_sibcall)
{
  int save_cnt = cfun->machine->callee_saved_reg_size / 4;
  int sp_adjust = cfun->machine->size_for_adjusting_sp + save_cnt * 4;
  if (sp_adjust > 0)
    {
      // Restore SP. Any alloca space is already released (the stack-restore
      // code put $sp back), so this makes $sp the CFA: describe the CFA
      // through $sp from here on, since $fp is about to be reloaded.
      rtx_insn *insn = emit_insn(
        gen_addsi3(
          stack_pointer_rtx,
          stack_pointer_rtx,
          GEN_INT(sp_adjust)
        )
      );
      RTX_FRAME_RELATED_P (insn) = 1;
      add_reg_note (insn, REG_CFA_DEF_CFA, stack_pointer_rtx);
      // Restore registers
      for (int r = ARRAY_SIZE(reg_frame_order)-1; r >= 0; --r )
        {
          size_t regno = reg_frame_order[r];
          if ((cfun->machine->reg_save_mask & (1 << regno)) != 0)
            {
              --save_cnt;
              rtx reg = gen_rtx_REG(Pmode, regno);
              insn = emit_insn(gen_movsi(
                reg,
                gen_rtx_MEM(Pmode,
                  plus_constant(Pmode, stack_pointer_rtx, -4*save_cnt-4, false) // Not an in-place addition
                )
              ));
              RTX_FRAME_RELATED_P (insn) = 1;
              add_reg_note (insn, REG_CFA_RESTORE, reg);
            }
        }
    }
  // For frames that use __builtin_eh_return, the register defined by
  // EH_RETURN_STACKADJ_RTX is set to 0 for all standard return paths.
  // On eh_return paths however, the register is set to the value that
  // should be added to the stack pointer in order to restore the
  // correct stack pointer for the exception handling frame.
  //
  // For BREW we are going to use BREW_REG_STRUCT_VAL_ADDR for
  // EH_RETURN_STACKADJ_RTX, add this onto the stack for eh_return frames.
  #ifdef EH_RETURN_STACKADJ_RTX
  if (crtl->calls_eh_return)
    emit_insn(gen_addsi3(stack_pointer_rtx, stack_pointer_rtx, EH_RETURN_STACKADJ_RTX));
  #endif

  // FIXME: do we need this forced use here???
  emit_use(gen_rtx_REG(Pmode, BREW_REG_LINK));
  // Return: we already adjusted SP, so all we have to do is to get PC from MEM[SP]
  if (!is_sibcall)
    {
      emit_jump_insn (gen_returner ());
    }
  else
    {
      //DEBUG_MARKER;
    }
}

///////////////////////////////////////////////////////////////////////////
//
//
// PREDICATE HELPERS
//
//
///////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////
//
//
// CONSTRAINT HELPERS
//
//
///////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////
//
//
// Helpers for macros in brew.h
//
//
///////////////////////////////////////////////////////////////////////////

// Implements the macro INITIAL_ELIMINATION_OFFSET, return the OFFSET.
// That is: used to figure out the offset between $?fp, $?ap and $fp or $sp.
// $fp (when there is one) and $?ap are the incoming $sp; the registers are
// saved below it, $?fp is right below them, and the prologue moves $sp down
// by the saved registers plus size_for_adjusting_sp.
int
brew_initial_elimination_offset (int from, int to)
{
  // Compute this since we need to use cfun->machine->local_vars_size.
  brew_compute_frame();
  int ret;
  if (from == FRAME_POINTER_REGNUM)
    ret = -cfun->machine->callee_saved_reg_size;
  else if (from == ARG_POINTER_REGNUM)
    ret = 0;
  else
    gcc_unreachable ();

  if (to == STACK_POINTER_REGNUM)
    ret += cfun->machine->callee_saved_reg_size + cfun->machine->size_for_adjusting_sp;
  else
    gcc_assert (to == HARD_FRAME_POINTER_REGNUM);
  return ret;
}

// Implements TARGET_FRAME_POINTER_REQUIRED.  Beyond what GCC checks itself
// (-fno-omit-frame-pointer, frame walking):
//   alloca moves $sp, and with EXIT_IGNORE_STACK 0 GCC doesn't ask for a
//     frame pointer on its own (as ft32);
//   __builtin_eh_return stores the handler into the $lr save slot at $fp-8
//     (brew_eh_return_handler_rtx);
//   accesses_prior_frames is checked by IRA but not by every later pass
//     (as or1k).
static bool
brew_frame_pointer_required (void)
{
  return cfun->calls_alloca || crtl->calls_eh_return || crtl->accesses_prior_frames;
}

/* DYNAMIC_CHAIN_ADDRESS
   A C expression whose value is RTL representing the address in a stack frame
   where the pointer to the caller's frame is stored.  Assume that FRAMEADDR is
   an RTL expression for the address of the stack frame itself.

   If you don't define this macro, the default is to return the value of
   FRAMEADDR--that is, the stack frame address is also the address of the stack
   word that points to the previous frame.  */

/* We store the old $fp at $fp-4, but we also need to record the fact that we *do*
   need to store $fp there. That is, when we get smart enough to elide it whenever
   possible.  */
rtx
brew_dynamic_chain_address(rtx frameaddr)
{
  // NOTE: crtl->accesses_prior_frames will be set if __builtin_return_address is called.
  //       That we will check in epilog/prolog generation to ensure that we actually spill
  //       the link register into this frame location.
  return plus_constant(Pmode, frameaddr, -UNITS_PER_WORD);
}


/* Worker for PROFILE_HOOK: emit _mcount (frompc, selfpc), where frompc
   is the incoming $lr (the caller's return address) and selfpc the
   address of the current function.  */
void
brew_profile_hook()
{
  rtx frompc = get_hard_reg_initial_val (Pmode, BREW_REG_LINK);
  rtx selfpc = XEXP (DECL_RTL (current_function_decl), 0);
  rtx fun = gen_rtx_SYMBOL_REF (Pmode, MCOUNT_NAME);
  emit_library_call (fun, LCT_NORMAL, VOIDmode,
		     frompc, Pmode, selfpc, Pmode);
}

/* RETURN_ADDR_RTX should return the return address for a frame pointer
   in FRAME. COUNT tells how many frames we've stepped back on the
   call-chain */
/* I'm not sure why COUNT would be relevant here.
   After the prologue, RA is at $fp-8 in the current frame.  */
/* This function of course relies on us saving the link register ($lr)
   in every frame, independent of whether it gets clobbered or not.
   Right now that's the case (I think), but we might want to re-visit
   that in the future. In that case, we'll have to modify this function
   to:
     if (count != 0)
       return NULL_RTX;
   On top of that, we would probably have to set cfun->machine-><<something>>
   to ensure that we *do* indeed save $lr in it's accessed this way.
*/
rtx
brew_return_addr_rtx(unsigned int count, rtx frame)
{
  // Our own return address is the incoming $lr; this needs no frame.
  if (count == 0)
    return get_hard_reg_initial_val (Pmode, BREW_REG_LINK);
  // For a non-zero count the caller (expand_builtin_return_addr) walked the
  // frame chain and gives us the n-th frame's $fp.
  return gen_frame_mem(Pmode, plus_constant (Pmode, frame, -UNITS_PER_WORD*2));
}

/* Handler for EH_RETURN_HANDLER_RTX */
/*
  Others, define things this way:
    #define EH_RETURN_HANDLER_RTX   RETURN_ADDR_RTX (0, frame_pointer_rtx)

    #define EH_RETURN_HANDLER_RTX                                                \
      gen_frame_mem (Pmode,                                                        \
        plus_constant (Pmode, frame_pointer_rtx, UNITS_PER_WORD))

  In general, it seems to be popular to store the return address in whatever the incoming return address (INCOMING_RETURN_ADDR_RTX) is.
  This makes sense: on the exception path, we don't need that anymore...

  Some set it to volatile:

    // Implement EH_RETURN_HANDLER_RTX.  The MEM needs to be volatile to prevent it from being deleted.
    rtx
    tilegx_eh_return_handler_rtx (void)
    {
      rtx tmp = gen_frame_mem (Pmode, hard_frame_pointer_rtx);
      MEM_VOLATILE_P (tmp) = true;
      return tmp;
    }
*/
rtx
brew_eh_return_handler_rtx()
{
  // Let's use the return address on the stack: its save slot at $fp-8
  // (brew_frame_pointer_required keeps the frame pointer, and
  // reg_needs_save_restore saves $lr, in functions calling
  // __builtin_eh_return).
  rtx ret_val = gen_frame_mem(Pmode, plus_constant (Pmode, hard_frame_pointer_rtx, -UNITS_PER_WORD*2));
  MEM_VOLATILE_P(ret_val) = true;
  return ret_val;
}

/* Handler for EH_RETURN_STACKADJ_RTX */
/* This is a register that is used to communicate to the epilog
   of functions that might have exception handlers in them that
   they need to adjust $sp by more than the usual amount.
   It is 0-ed out for normal control-flow, and filled in with a
   potentially non-0 value for the exception path.
*/
rtx
brew_eh_return_stackadj_rtx()
{
  return gen_rtx_REG(Pmode, BREW_REG_STRUCT_VAL_ADDR);
}

///////////////////////////////////////////////////////////////////////////
//
//
// Target hook implementations
//
//
///////////////////////////////////////////////////////////////////////////


// for TARGET_RETURN_IN_MEMORY
// Returns true if return value is to be in memory, false for registers
static bool
brew_return_in_memory (const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
  // We allow returns of up to 4 words in registers.
  const HOST_WIDE_INT size = int_size_in_bytes (type);
  return (size == -1 || size > 4 * UNITS_PER_WORD);
}

static const HOST_WIDE_INT max_regs_for_args = 4;
static const int ret_value_reg = BREW_REG_ARG0;
static const int first_arg_value_reg = BREW_REG_ARG0;

// The following set functions work in tandem:
//   brew_function_arg decides in what form the argument should be passed.
//     It returns either a REG rtx specifying the register to be used or
//     NULL_RTX if the arg to be passed on the stack.
//     In helping with deciding *which* register to chose, an argument
//     cum_v.p (retrieved using get_cumulative_args) is used. This is
//     defined as a simply 'unsigned int' to count the number of registers
//     that have already been used up for arguments.
//   brew_function_arg_advance is used to update cum_v.p after the
//     decision in the first function is made. It must therefore come
//     to the same conclusion in terms of using a register or stack
//     and increment cum_v.p as needed.
//   brew_pass_by_reference decides if an argument should be pass
//     by reference (even if the signature asks for by-value).
//     It is called by GCC but also by arg_size_in_register.
//     This later call is used to determine if we want to pass
//     pass the pointer to the reference in a register or not.
//   brew_arg_partial_bytes should return the number of bytes
//     that did get passed in registers for a given argument.
//     It could return 0 in case the argument is fully in registers,
//     but it *has* to return 0 in case it's fully on the stack.
//
// The operation of these functions must also match what
// __builtin_apply assumes, which appears to be that the first
// N bytes of the arguments gets put in registers with argument
// boundaries matching up with register boundaries.

// Returns the size of the argument, in number of registers used,
// if it is to be passed in registres. Returns 0, if arg should be
// passed on the stack. It's possible that an argument only partially
// fits in registers, in which case the function returns the number
// of registers that should be used for the part that does fit.

static bool
brew_pass_by_reference (cumulative_args_t, const function_arg_info &arg);

static int
arg_size_in_register(cumulative_args_t cum_v, const function_arg_info &arg)
{
  // NOTE: we have to make sure that once we start putting arguments on the stack,
  //       all subsequent arguments are put on the stack. Otherwise __builtin_apply
  //       will break, because it makes that assumption. That is to say, that
  //       once this function returns 0, it should always return 0.
  //       We achieve that by updating 'cum' in case we return 0, but
  //       that needs to happen on the caller side.
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  // Otherwise allow arg to be in registers, if we have enough left.
  HOST_WIDE_INT arg_size = arg.promoted_size_in_bytes();
  if (arg_size <= 0)
    return 0;
  HOST_WIDE_INT regs_needed = (arg_size + UNITS_PER_WORD-1) / UNITS_PER_WORD; // Round up arg-size to the next register size

  // If we pass something by reference, only account for the pointer to it.
  if (brew_pass_by_reference(cum_v, arg))
    regs_needed = 1;

  if (regs_needed > max_regs_for_args - *cum)
    return max_regs_for_args - *cum;
  return regs_needed;
}

// for TARGET_FUNCTION_ARG
// Return the next register to be used to hold a function argument or
// NULL_RTX if there's no more space.
static rtx
brew_function_arg (cumulative_args_t cum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  if (arg_size_in_register(cum_v, arg) > 0)
    return gen_rtx_REG(arg.mode, first_arg_value_reg + *cum);
  else
    return NULL_RTX;
}

// for TARGET_FUNCTION_ARG_ADVANCE
static void
brew_function_arg_advance (cumulative_args_t cum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int regs_needed = arg_size_in_register(cum_v, arg);
  *cum += regs_needed;
  gcc_assert(*cum <= max_regs_for_args);
}


// for TARGET_PASS_BY_REFERENCE
// Return non-zero if the function argument described by ARG is to be passed by reference.
static bool
brew_pass_by_reference (cumulative_args_t, const function_arg_info &arg)
{
  return arg.type && TREE_CODE (TYPE_SIZE (arg.type)) != INTEGER_CST;

  //if (arg.aggregate_type_p())
  //  return true;
  //HOST_WIDE_INT size = arg.type_size_in_bytes();
  //return size > max_regs_for_args * UNITS_PER_WORD || size <= 0;
}

// for TARGET_ARG_PARTIAL_BYTES
// Some function arguments will only partially fit in the registers
// that hold arguments. Given a new arg, return the number of bytes
// that fit in argument passing registers.
static int
brew_arg_partial_bytes (cumulative_args_t cum_v, const function_arg_info &arg)
{
  int bytes = arg.promoted_size_in_bytes ();
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int bytes_left = (max_regs_for_args - *cum) * UNITS_PER_WORD;

  if (bytes == -1)
    return 0;

  if (bytes_left == 0)
    return 0;
  if (bytes > bytes_left)
    return bytes_left;
  return 0;
}

// for TARGET_SETUP_INCOMING_VARARGS
// We should do two things here:
//   - push all register-passed *not* named arguments onto the stack
//   - set *pretend_size to the number of bytes put on the stack
// NOTE: this is controlled by arg_size_in_register, so these
// two functions must be kept in sync.
static void
brew_setup_incoming_varargs(
  cumulative_args_t cum_v,
  const function_arg_info &,
  int *pretend_size,
  int no_rtl
) {
  //*pretend_size = 0;
  //return;

  CUMULATIVE_ARGS *cum = get_cumulative_args(cum_v);

  *pretend_size = *cum * UNITS_PER_WORD;

  if (no_rtl)
    return;

  for(unsigned int arg_idx = *cum; arg_idx < max_regs_for_args; arg_idx++)
    {
      unsigned int regno = arg_idx + first_arg_value_reg;
      rtx reg = gen_rtx_REG(SImode, regno);
      rtx slot = arg_idx == 0 ? gen_rtx_REG(Pmode, ARG_POINTER_REGNUM) : gen_rtx_PLUS(
        Pmode,
        gen_rtx_REG(SImode, ARG_POINTER_REGNUM),
        GEN_INT(UNITS_PER_WORD * arg_idx)
      );
      emit_move_insn(gen_rtx_MEM(SImode, slot), reg);
    }
}










// Helper function for `brew_legitimate_address_p'
static bool
brew_reg_ok_for_base_p(const_rtx reg, bool strict_p)
{
  int regno = REGNO(reg);

  if (strict_p)
    {
      // A pseudo is only OK if it was allocated to a suitable hard register.
      // reg_renumber only exists while register allocation results are
      // available, so don't assume it does.
      if (!HARD_REGISTER_NUM_P(regno))
        {
          if (!reg_renumber)
            return false;
          regno = reg_renumber[regno];
        }
      return HARD_REGNO_OK_FOR_BASE_P(regno);
    }
  else
    return
      !HARD_REGISTER_NUM_P(regno) ||
      HARD_REGNO_OK_FOR_BASE_P(regno);
}

// for TARGET_LEGITIMATE_ADDRESS_P
static bool
brew_legitimate_address_p(
  machine_mode mode ATTRIBUTE_UNUSED,
  rtx x,
  bool strict_p,
  addr_space_t as,
  code_helper = ERROR_MARK
) {
  gcc_assert(ADDR_SPACE_GENERIC_P(as));

  // Accept <reg>+<offset> pattern
  if (brew_legitimate_offset_address_p(x, strict_p))
    return true;
  // Accept a <reg> pattern
  if (REG_P(x) && brew_reg_ok_for_base_p(x, strict_p))
    return true;
  // Accept an <offset> pattern
  if (
    GET_CODE(x) == SYMBOL_REF ||
    GET_CODE(x) == LABEL_REF ||
    GET_CODE(x) == CONST
  )
    return true;
  return false;
}

// Helper function for `brew_legitimate_tiny_address_p'
static bool
brew_reg_ok_for_tiny_base_p(const_rtx reg)
{
  int regno = REGNO(reg);

  return
    regno == BREW_REG_FP ||
    regno == BREW_REG_SP ||
    regno == BREW_QFP ||
    regno == BREW_QAP;
}


// A variant of the above that accepts expressions for tiny addressing modes
// This is used in predicates.md

#define DB(x) { printf("brew_legitimate_tiny_address_p %s = %s\n", #x, (x)?"true":"false"); if (!(x)) return false; }
bool
brew_legitimate_tiny_address_p(rtx x)
{
  //printf("brew_legitimate_tiny_address_p --------------------\n");
  //DB(GET_CODE(x) == PLUS);
  //DB(REG_P(XEXP(x, 0)));
  //DB(brew_reg_ok_for_tiny_base_p(XEXP(x, 0)));
  //DB(CONST_INT_P(XEXP(x, 1)));
  //DB(IN_RANGE(INTVAL(XEXP(x, 1)) / 4, -64, 63));
  //printf("                        INTVAL(XEXP(x,1)): %ld\n",INTVAL(XEXP(x,1)));
  //DB((INTVAL(XEXP(x,1)) & 3) == 0);
  //printf("brew_legitimate_tiny_address_p !!! ALL OK !!!\n");
  // Accept <reg>+<offset> pattern if base is $fp or $sp and the offset is in the right range
  if (
    GET_CODE(x) == PLUS &&
    REG_P(XEXP(x, 0)) &&
    brew_reg_ok_for_tiny_base_p(XEXP(x, 0)) &&
    CONST_INT_P(XEXP(x, 1)) &&
    IN_RANGE(INTVAL(XEXP(x, 1)), -256, 252) &&
    (INTVAL(XEXP(x,1)) & 3) == 0
  )
    return true;
  return false;
}

bool
brew_legitimate_offset_address_p(rtx x, bool strict_p)
{
  //printf("brew_legitimate_offset_address_p --------------------\n");
  //DB(GET_CODE(x) == PLUS);
  //DB(REG_P(XEXP(x, 0)));
  //DB(brew_reg_ok_for_base_p(XEXP(x, 0), strict_p));
  //DB(CONST_INT_P(XEXP(x, 1)));
  //DB(IN_RANGE(INTVAL(XEXP(x, 1)), -32768, 32767));
  //printf("brew_legitimate_offset_address_p !!! ALL OK !!!\n");
  // Accept <reg>+<offset> pattern if the offset is in the right range
  if (
    GET_CODE(x) == PLUS &&
    REG_P(XEXP(x, 0)) &&
    brew_reg_ok_for_base_p(XEXP(x, 0), strict_p) &&
    CONST_INT_P(XEXP(x, 1)) &&
    IN_RANGE(INTVAL(XEXP(x, 1)), -32768, 32767)
  )
    return true;
  return false;
}



// for TARGET_FUNCTION_VALUE
// Define how to find the value returned by a function.
static rtx
brew_function_value(
  const_tree valtype,
  const_tree fntype_or_decl ATTRIBUTE_UNUSED,
  bool outgoing ATTRIBUTE_UNUSED
) {
  // We always return values in register $r4 for brew
  return gen_rtx_REG(TYPE_MODE(valtype), ret_value_reg);
}

// for TARGET_LIBCALL_VALUE
// Define how to find the value returned by a library function.
static rtx
brew_libcall_value(
  machine_mode mode,
  const_rtx fun ATTRIBUTE_UNUSED
) {
  // We always return values in register $r4 for brew.
  return gen_rtx_REG(mode, ret_value_reg);
}

// for TARGET_FUNCTION_VALUE_REGNO_P
// Return true if regno is the number of a hard register in which the
// values of called function may come back.
static bool
brew_function_value_regno_p(const unsigned int regno)
{
  // We always return values in register $r4 for brew.
  return (regno == ret_value_reg);
}

// For TARGET_STRUCT_VALUE_RTX.
// This target hook should return the location of the structure value address (normally
// a mem or reg), or 0 if the address is passed as an “invisible” first argument. Note that
// fndecl may be NULL, for libcalls. You do not need to define this target hook if the
// address is always passed as an “invisible” first argument.
static rtx
brew_struct_value_rtx (tree fntype ATTRIBUTE_UNUSED, int incoming ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (Pmode, BREW_REG_STRUCT_VAL_ADDR);
}

// Tuning (-mtune=).
//
// Costs are in the units of COSTS_N_INSNS.  The rtx costs are those of
// the operation itself: rtx_cost adds the costs of the operands.
struct brew_cost_table
{
  int mult;       // MULT
  int div;        // DIV, UDIV, MOD, UMOD (library calls)
  int mem;        // MEM, without its address
  int imm_tiny;   // constant in a tiny field: -7..7 in a move, add or subtract
  int imm_short;  // constant in a 16-bit field
  int imm_long;   // any other constant, a symbol or a label (32 bits)
};

struct brew_tune_info
{
  const struct brew_cost_table *speed_costs; // rtx costs when optimizing for speed
  int load_cost;   // TARGET_MEMORY_MOVE_COST of a load
  int store_cost;  // TARGET_MEMORY_MOVE_COST of a store
  int reg_move;    // TARGET_REGISTER_MOVE_COST
  int branch_cost; // BRANCH_COST when optimizing for speed
  int addr_halfword; // TARGET_ADDRESS_COST of each halfword an address adds
                     // to a load or store, when optimizing for speed
};

// GCC's own defaults (rtx_cost without a target hook,
// default_memory_move_cost, default_register_move_cost, BRANCH_COST 1):
// the costs Brew had before -mtune existed.
static const struct brew_cost_table brew_default_costs =
{
  COSTS_N_INSNS (5), // mult
  COSTS_N_INSNS (7), // div
  COSTS_N_INSNS (1), // mem
  COSTS_N_INSNS (1), // imm_tiny
  COSTS_N_INSNS (1), // imm_short
  COSTS_N_INSNS (1), // imm_long
};

// Espresso, calibrated with the simulator's cycle model of its RTL
// (--timing=espresso) on the code-generation tests and dhrystone
// (brew/benchmarks), both compilers: the rtx costs hardly matter on a core
// that spends half its cycles refilling after taken branches and an
// eighth waiting for fetch, and the first estimates from the measured
// timing (immediates by encoded length, a 4-instruction memory access,
// load move cost 10, BRANCH_COST 3) made -O2 0.5-0.65% slower than the
// generic costs.  Only a cheaper multiply pays: a multiply is one
// instruction (it blocks the next one for a cycle), and pricing it so
// keeps GCC from expanding multiplications by constants into longer
// shift-and-add sequences (-0.2% cycles, -0.8% to -0.95% code at -O2).
static const struct brew_cost_table brew_espresso_costs =
{
  COSTS_N_INSNS (1), // mult
  COSTS_N_INSNS (7), // div
  COSTS_N_INSNS (1), // mem
  COSTS_N_INSNS (1), // imm_tiny
  COSTS_N_INSNS (1), // imm_short
  COSTS_N_INSNS (1), // imm_long
};

// Indexed by enum brew_processor.
static const struct brew_tune_info brew_tune_table[PROCESSOR_max] =
{
  // generic: a load or store is one instruction whatever its address, but
  // a longer one takes more fetching, so each halfword costs a little; with
  // 0, ivopts' ties could keep a copy of an IV live across calls (qsort)
  { &brew_default_costs, 4, 4, 2, 1, 1 },
  // espresso: the generic move and branch costs (a higher load cost or
  // BRANCH_COST measured slower); an address halfword costs an instruction
  // (any non-zero cost avoids the qsort spill; 0 is 2.5% slower there)
  { &brew_espresso_costs, 4, 4, 2, 1, COSTS_N_INSNS (1) },
};

static const struct brew_tune_info *brew_tune = &brew_tune_table[PROCESSOR_GENERIC];

// The size costs don't depend on -mtune.
static const struct brew_cost_table *brew_size_costs = &brew_default_costs;

// for BRANCH_COST
int
brew_branch_cost (bool speed_p, bool predictable_p ATTRIBUTE_UNUSED)
{
  return speed_p ? brew_tune->branch_cost : 1;
}

// for TARGET_RTX_COSTS
//
// Sets the cost of the operation itself and returns false, so that
// rtx_cost adds the costs of the operands, the way it does without a
// hook; with brew_default_costs the result is exactly GCC's default.
static bool
brew_rtx_costs (rtx x, machine_mode mode, int outer_code,
                int opno ATTRIBUTE_UNUSED, int *total, bool speed)
{
  const struct brew_cost_table *costs = speed ? brew_tune->speed_costs : brew_size_costs;
  // As in rtx_cost: an operation N words wide costs N times as much,
  // a multiplication or division N*N times.
  unsigned mode_size = estimated_poly_value (GET_MODE_SIZE (mode));
  int factor = mode_size > UNITS_PER_WORD ? mode_size / UNITS_PER_WORD : 1;

  switch (GET_CODE (x))
    {
    case CONST_INT:
      {
        HOST_WIDE_INT ival = INTVAL (x);
        int cost;
        if (IN_RANGE (ival, -7, 7)
            && (outer_code == SET || outer_code == PLUS || outer_code == MINUS))
          cost = costs->imm_tiny;
        else if (IN_RANGE (ival, -32768, 32767))
          cost = costs->imm_short;
        else
          cost = costs->imm_long;
        *total = factor * cost;
        return true;
      }
    case SYMBOL_REF:
    case LABEL_REF:
      *total = factor * costs->imm_long;
      return true;
    case MULT:
      *total = factor * factor * costs->mult;
      return false;
    case DIV:
    case UDIV:
    case MOD:
    case UMOD:
      *total = factor * factor * costs->div;
      return false;
    case MEM:
      *total = factor * costs->mem;
      return false;
    default:
      return false;
    }
}

// for TARGET_MEMORY_MOVE_COST
static int
brew_memory_move_cost (machine_mode mode, reg_class_t rclass, bool in)
{
  return (in ? brew_tune->load_cost : brew_tune->store_cost)
         + memory_move_secondary_cost (mode, rclass, in);
}

// for TARGET_REGISTER_MOVE_COST
static int
brew_register_move_cost (machine_mode mode ATTRIBUTE_UNUSED,
                         reg_class_t from ATTRIBUTE_UNUSED,
                         reg_class_t to ATTRIBUTE_UNUSED)
{
  return brew_tune->reg_move;
}

// for TARGET_ADDRESS_COST
//
// Every addressing mode is part of a single load or store; they differ
// only in length: $rA and the tiny $sp/$fp forms fit a 16-bit
// instruction, $rA + a 16-bit offset needs 32 bits and an absolute
// address 48.  GCC's default prices $rA + N as an extra addition, which
// made ivopts prefer a second induction variable to an offset address.
static int
brew_address_cost (rtx x, machine_mode mode ATTRIBUTE_UNUSED,
                   addr_space_t as ATTRIBUTE_UNUSED, bool speed)
{
  int halfwords;
  if (REG_P (x) || brew_legitimate_tiny_address_p (x))
    halfwords = 0;
  else if (GET_CODE (x) == PLUS && REG_P (XEXP (x, 0))
           && CONST_INT_P (XEXP (x, 1))
           && IN_RANGE (INTVAL (XEXP (x, 1)), -32768, 32767))
    halfwords = 1;
  else
    halfwords = 2;
  return halfwords * (speed ? brew_tune->addr_halfword : COSTS_N_INSNS (1));
}

// for TARGET_OPTION_OVERRIDE
static void
brew_option_override (void)
{
  // Set the per-function-data initializer.
  init_machine_status = brew_init_machine_status;

  brew_tune = &brew_tune_table[brew_tune_option];
}

// If we can't create asm, we need to abort.
// NOTE: the whole of GCC needs to deal with inline ASM as well,
// where it's not an internal compiler error when we encounter
// invalid asm. This function however is never called under those
// circumstances.
static void
brew_operand_lossage(const char *msgid, rtx op)
{
  debug_rtx(op);
  output_operand_lossage("%s", msgid);
}

// The assembly name of a hard register.  $r12 is the frame pointer only in
// functions that set one up; where the register allocator uses it as an
// ordinary call-saved register, print its generic name (gas accepts both;
// objdump always shows $fp, as it can't tell the two uses apart).
static const char *
brew_reg_name (unsigned int regno)
{
  if (regno == BREW_REG_FP && !frame_pointer_needed)
    return "$r12";
  return reg_names[regno];
}

// for TARGET_PRINT_OPERAND
static void
brew_print_operand (FILE *file, rtx x, int code)
{
  rtx operand = x;

  // New code entries should just be added to the switch below.  If
  // handling is finished, just return.  If handling was just a
  // modification of the operand, the modified operand should be put in
  // "operand", and then do a break to let default handling
  // (zero-modifier) output the operand.

  switch (GET_CODE (operand))
    {
    case REG:
      {
        int regno = REGNO(operand);
        if (regno > LAST_PHYSICAL_REG)
          internal_error ("internal error: bad register: %d", regno);
        switch (code)
          {
          case 0:
            fprintf (file, "%s", brew_reg_name (regno));
            break;
          default:
            brew_operand_lossage("invalid operand modifier letter", x);
            break;
          }
      }
      return;

    case MEM:
      switch (code)
        {
        case 0:
          output_address(GET_MODE (XEXP (operand, 0)), XEXP (operand, 0));
          break;
        default:
          brew_operand_lossage("invalid operand modifier letter", x);
          break;
        }
      return;

    default:
      switch (code)
        {
        case 0:
          // No need to handle all strange variants
          // let output_addr_const do it for us.
          if (CONSTANT_P (operand))
            {
              output_addr_const (file, operand);
              return;
            }
          brew_operand_lossage("unexpected operand", x);
          break;
        default:
          brew_operand_lossage("invalid operand modifier letter", x);
          break;
        }
    }
}

// for PRINT_OPERAND_ADDRESS
// FIXME: I don't think this is always correct. We should run some tests to see...
static void
brew_print_operand_address(FILE *file, machine_mode, rtx x)
{
  switch (GET_CODE(x))
    {
    case REG:
      fprintf(file, "%s", brew_reg_name (REGNO (x)));
      break;

    case PLUS:
      switch (GET_CODE(XEXP(x, 1)))
        {
        case CONST_INT:
          fprintf(file, "%s + %ld", brew_reg_name (REGNO(XEXP(x, 0))), INTVAL(XEXP(x, 1)));
          break;
        case SYMBOL_REF:
          output_addr_const(file, XEXP(x, 1));
          fprintf(file, "%s", brew_reg_name (REGNO(XEXP (x, 0))));
          break;
        case CONST:
          {
            rtx plus = XEXP(XEXP(x, 1), 0);
            if (
              GET_CODE(XEXP(plus, 0)) == SYMBOL_REF &&
              CONST_INT_P(XEXP(plus, 1))
            )
              {
                output_addr_const(file, XEXP (plus, 0));
                fprintf(file,"%s + %ld", brew_reg_name (REGNO(XEXP(x, 0))), INTVAL(XEXP(plus, 1)));
              }
            else
              abort();
          }
          break;
        default:
          abort();
        }
      break;

    default:
      output_addr_const (file, x);
      break;
    }
}






// pass in everything smaller then an int as int for function calls.
#undef  TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES                hook_bool_const_tree_true
#undef  TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY                  brew_return_in_memory
#undef  TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK                must_pass_in_stack_var_size
#undef  TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE                 brew_pass_by_reference
#undef  TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES                 brew_arg_partial_bytes
#undef  TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG                      brew_function_arg
#undef  TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE              brew_function_arg_advance
#undef  TARGET_STRICT_ARGUMENT_NAMING
#define TARGET_STRICT_ARGUMENT_NAMING            hook_bool_CUMULATIVE_ARGS_true
// We should use the LRA register allocator, even if Moxie used the old one.
// FIXME: we should work with both!, set this back to true!!!
#undef  TARGET_LRA_P
#define TARGET_LRA_P                             hook_bool_void_true
#undef  TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P
#define TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P   brew_legitimate_address_p
#undef  TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS            brew_setup_incoming_varargs
#undef  TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE                    brew_function_value
#undef  TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE                     brew_libcall_value
#undef  TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P            brew_function_value_regno_p
#undef  TARGET_STRUCT_VALUE_RTX
#define TARGET_STRUCT_VALUE_RTX                  brew_struct_value_rtx

#undef  TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED            brew_frame_pointer_required
#undef  TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE                   brew_option_override

#undef  TARGET_RTX_COSTS
#define TARGET_RTX_COSTS                         brew_rtx_costs
#undef  TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST                  brew_memory_move_cost
#undef  TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST                brew_register_move_cost
#undef  TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST                      brew_address_cost
#undef  TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND                     brew_print_operand
#undef  TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS             brew_print_operand_address
#undef  TARGET_CONSTANT_ALIGNMENT
#define TARGET_CONSTANT_ALIGNMENT                constant_alignment_word_strings
// Sibcalls (tail calls). The sibcall patterns restrict an indirect target to
// SIBCALL_REGS (constraint "j"), since the epilogue restores the call-saved
// registers before the jump.
#undef  TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL           brew_function_ok_for_sibcall
// We don't have anything against speculation-related security measures
#undef  TARGET_HAVE_SPECULATION_SAFE_VALUE
#define TARGET_HAVE_SPECULATION_SAFE_VALUE       speculation_safe_value_not_needed

/////////////////////////////////////////////////////////////////////
// Trampolines for Nested Functions
/////////////////////////////////////////////////////////////////////

// THIS IS THE EXECUTABLE STACK-BASED IMPLEMENTATION
// Here, the trampline template will be copied to a stack-based variable,
// it is going to be patched up with the proper values for the static chain
// and the actual function address and called as a regular function.
// As a consequence, we need to:
//  1. Make sure the stack is executable, or at least the region of the stack,
//     where the trampoline resides is, at least for the duration of the call;
//  2. Make sure we flush the write-queue and instruction caches, since we're
//     dealing with self-modifying code here.
//     FIXME: this is TODO!!!!

// for TARGET_ASM_TRAMPOLINE_TEMPLATE.
// Output assembler code for a block containing the constant parts
// of a trampoline, leaving space for the variable parts.
// Note that STATIC_CHAIN_REGNUM is $r2
static void
brew_asm_trampoline_template(FILE *f)
{
  // Both instructions are 6 bytes, we need to align the data-field
  // to 32-bit boundary (the start is already aligned) because
  // we will store 32-bit quantities in there run-time and we don't
  // have support for unaligned stores. Thus the NOP-s...
  fprintf(f, "\tnop\n");
  fprintf(f, "\t%s <- 0xdeadbeef\n", reg_names[STATIC_CHAIN_REGNUM]);
  fprintf(f, "\tnop\n");
  fprintf(f, "\t$pc <- 0xdeadbeef\n");
}

// for TARGET_TRAMPOLINE_INIT
static void
brew_trampoline_init(rtx m_tramp, tree fndecl, rtx chain_value)
{
  // Copy static trampoline template over to
  // destination location
  emit_block_move(
    m_tramp,
    assemble_trampoline_template(),
    GEN_INT(TRAMPOLINE_SIZE),
    BLOCK_OP_NORMAL
  );

  // Generate two moves that fill-in the addresses in the trampoline
  rtx mem;
  int pool = 4;
  mem = adjust_address(m_tramp, SImode, pool);
  emit_move_insn(mem, chain_value);
  mem = adjust_address(m_tramp, SImode, pool + 8);
  rtx fnaddr = XEXP(DECL_RTL(fndecl), 0);
  emit_move_insn (mem, fnaddr);

  // Clear cache for the memory of the trampoline
  //rtx a_tramp = XEXP(m_tramp, 0); // address of the trampoline
  //maybe_emit_call_builtin___clear_cache(
  //  a_tramp,
  //  plus_constant(
  //    Pmode,
  //    a_tramp,
  //    TRAMPOLINE_SIZE
  //  )
  //);
}



#undef  TARGET_ASM_TRAMPOLINE_TEMPLATE
#define TARGET_ASM_TRAMPOLINE_TEMPLATE              brew_asm_trampoline_template
#undef  TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT                      brew_trampoline_init

// The low bit is ignored when loading $pc ($pc doesn't have bit-0 implemented) so is safe to use.
//#undef  TARGET_CUSTOM_FUNCTION_DESCRIPTORS
//#define TARGET_CUSTOM_FUNCTION_DESCRIPTORS          1



/////////////////////////////////////////////////////////////////////////////
//
// C++ stuff
//
/////////////////////////////////////////////////////////////////////////////

/* The TARGET_ASM_OUTPUT_MI_THUNK worker.  */
static void
brew_asm_output_mi_thunk(
  FILE *stream,
  tree thunkdecl ATTRIBUTE_UNUSED,
  HOST_WIDE_INT delta,
  HOST_WIDE_INT vcall_offset,
  tree funcdecl
)
{
  const char *fnname = IDENTIFIER_POINTER(DECL_ASSEMBLER_NAME(thunkdecl));

  assemble_start_function(thunkdecl, fnname);
  // Make sure unwind info is emitted for the thunk if needed.
  final_start_function(emit_barrier (), stream, 1);

  // On brew, 'this' or any function argument gets bumped from $r4 to $r5 if
  // the return of the function is an aggregate.
  // In those cases the first 'argument' is a pointer to the storage of the
  // return value.
  // Well, changed my mind: dedicated $r10 for the structure value pointer
  // so this is fixed at this point.
  // The rationale is this: shifting of the arguments by one, based on the
  // return value wracks havoc (maybe?) with vaarg functions, and I don't
  // want to complicate that code even further. I don't even know how to get
  // to that information in the vaarg spilling code. If needed, apparently
  // ARM(32) used the shifting style structure value pointer, so maybe
  // something can be learned from there...
  // Also, the behavior of the structure value pointer is controlled by
  //  brew_struct_value_rtx and TARGET_STRUCT_VALUE_RTX.
  //int this_arg = aggregate_value_p(TREE_TYPE(TREE_TYPE(funcdecl)),funcdecl) ? BREW_REG_ARG0 + 1 : BREW_REG_ARG0;
  int this_arg = BREW_REG_ARG0;
  if(delta != 0)
    {
      fprintf(
        stream,
        "\t%s <- %s + (" HOST_WIDE_INT_PRINT_DEC ") # DELTA ADJUSTMENT\n",
        reg_names[this_arg],
        reg_names[this_arg],
        delta
      );
    }
  if (vcall_offset != 0)
    {
      // load vtbl address into tmp
      fprintf(
        stream,
        "\t%s <- mem32[%s] # VCALL ADJUSTMENT\n",
        reg_names[BREW_REG_THUNK],
        reg_names[this_arg]
      );
      // load required entry from vtable
      fprintf(
        stream,
        "\t%s <- mem32[%s + " HOST_WIDE_INT_PRINT_DEC "]\n",
        reg_names[BREW_REG_THUNK],
        reg_names[BREW_REG_THUNK],
        vcall_offset
      );
      // update this ptr
      fprintf(
        stream,
        "\t%s <- %s + %s\n",
        reg_names[this_arg],
        reg_names[this_arg],
        reg_names[BREW_REG_THUNK]
      );
    }
  fprintf (stream, "\t$pc <- ");
  assemble_name(stream, XSTR (XEXP (DECL_RTL (funcdecl), 0), 0));
  fprintf (stream, " # THUNK JUMP\n");

  final_end_function ();
  assemble_end_function(thunkdecl, fnname);
}

#undef  TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK      brew_asm_output_mi_thunk
#undef  TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK  hook_bool_const_tree_hwi_hwi_const_tree_true

/* Mark every object file with a .note.GNU-stack section, so the linker
   doesn't assume an executable stack (and warn about it) when such objects
   are linked with ones that carry the note.  The note is made executable
   only if the file creates trampolines on the stack.  */
#undef  TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END             file_end_indicate_exec_stack

/* Implement TARGET_SCHED_SET_SCHED_FLAGS.  Don't let the scheduler break
   dependencies: to move an increment above a memory access that uses the
   register, it rewrites $rA into $rA + -N, which turns a 16-bit load or
   store into a 32-bit one, and a fetch-bound core loses more on the longer
   code than it gains on the latency.  */

static void
brew_set_sched_flags (spec_info_t spec_info)
{
  current_sched_info->flags |= DONT_BREAK_DEPENDENCIES;
  spec_info->mask = 0;
}

#undef  TARGET_SCHED_SET_SCHED_FLAGS
#define TARGET_SCHED_SET_SCHED_FLAGS    brew_set_sched_flags

/* True if INSN accesses memory (a load, a store or an atomic sequence).  */

static bool
brew_mem_insn_p (rtx_insn *insn)
{
  if (!NONDEBUG_INSN_P (insn) || recog_memoized (insn) < 0)
    return false;
  switch (get_attr_type (insn))
    {
    case TYPE_LOAD: case TYPE_LOADN: case TYPE_STORE: case TYPE_STOREN:
    case TYPE_MULTI:
      return true;
    default:
      return false;
    }
}

/* True if INSN is an ALU operation, a shift or a multiplication.  */

static bool
brew_alu_insn_p (rtx_insn *insn)
{
  if (!NONDEBUG_INSN_P (insn) || recog_memoized (insn) < 0)
    return false;
  enum attr_type t = get_attr_type (insn);
  return t == TYPE_ALU || t == TYPE_SHIFT || t == TYPE_MULT;
}

/* True if BB can be reached by a jump (not only by falling through).  */

static bool
brew_jump_target_p (basic_block bb)
{
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, bb->preds)
    if (!(e->flags & EDGE_FALLTHRU))
      return true;
  return false;
}

/* Implement TARGET_SCHED_REORDER.  After a taken branch Espresso refills
   its instruction queue with a fetch burst, and a load or store that
   reaches the memory unit while the burst runs waits for it, blocking the
   instructions behind it; one instruction ahead of it gives the burst time
   to end.  The pipeline description can't see this, since it describes no
   fetch and starts every block afresh.  So in the final schedule (after
   register allocation), if a block that a jump can reach would start with
   a memory access, start it with a ready ALU instruction instead.  */

static int
brew_sched_reorder (FILE *, int, rtx_insn **ready, int *n_readyp, int clock)
{
  int n = *n_readyp;
  if (reload_completed && clock == 0 && n > 1 && brew_mem_insn_p (ready[n - 1])
      && brew_jump_target_p (BLOCK_FOR_INSN (ready[n - 1])))
    for (int i = n - 2; i >= 0; i--)
      if (brew_alu_insn_p (ready[i]))
	{
	  rtx_insn *insn = ready[i];
	  memmove (&ready[i], &ready[i + 1], (n - 1 - i) * sizeof (*ready));
	  ready[n - 1] = insn;
	  break;
	}
  return 1;
}

#undef  TARGET_SCHED_REORDER
#define TARGET_SCHED_REORDER            brew_sched_reorder






struct gcc_target targetm = TARGET_INITIALIZER;

// Fences for an atomic read-modify-write loop (sync.md) with memory model
// MODEL: Brew doesn't order loads and stores among themselves, so release
// semantics need a full fence before the loop and acquire semantics one
// after it.
void
brew_atomic_pre_fence (HOST_WIDE_INT model)
{
  enum memmodel m = memmodel_from_int (model);
  if (is_mm_release (m) || is_mm_acq_rel (m) || is_mm_seq_cst (m))
    emit_insn (gen_brew_fence ());
}

void
brew_atomic_post_fence (HOST_WIDE_INT model)
{
  enum memmodel m = memmodel_from_int (model);
  if (is_mm_acquire (m) || is_mm_consume (m) || is_mm_acq_rel (m)
      || is_mm_seq_cst (m))
    emit_insn (gen_brew_fence ());
}

// 8- and 16-bit atomics (sync.md) work on the aligned word containing
// MEM: set *WMEM to that word, *SHIFT to the field's bit offset (Brew is
// little-endian), *MASK to the field's bits in the word and *NOTMASK to
// the other bits.
static void
brew_subword_setup (rtx mem, rtx *wmem, rtx *shift, rtx *mask, rtx *notmask)
{
  machine_mode mode = GET_MODE (mem);
  rtx addr = force_reg (Pmode, XEXP (mem, 0));
  rtx aligned = force_reg (Pmode, expand_simple_binop (Pmode, AND, addr,
                                                       GEN_INT (-4), NULL_RTX,
                                                       1, OPTAB_DIRECT));
  *wmem = change_address (mem, SImode, aligned);
  set_mem_alias_set (*wmem, 0);

  rtx byte = expand_simple_binop (SImode, AND, addr, GEN_INT (3), NULL_RTX,
                                  1, OPTAB_DIRECT);
  *shift = force_reg (SImode, expand_simple_binop (SImode, ASHIFT, byte,
                                                   GEN_INT (3), NULL_RTX, 1,
                                                   OPTAB_DIRECT));
  rtx field = force_reg (SImode, GEN_INT (GET_MODE_MASK (mode)));
  *mask = force_reg (SImode, expand_simple_binop (SImode, ASHIFT, field,
                                                  *shift, NULL_RTX, 1,
                                                  OPTAB_DIRECT));
  *notmask = force_reg (SImode, expand_simple_unop (SImode, NOT, *mask,
                                                    NULL_RTX, 1));
}

// VAL (of MEM's mode) zero-extended and shifted into the field.
static rtx
brew_subword_value (rtx val, machine_mode mode, rtx shift)
{
  rtx word = force_reg (SImode, convert_modes (SImode, mode, val, 1));
  return force_reg (SImode, expand_simple_binop (SImode, ASHIFT, word, shift,
                                                 NULL_RTX, 1, OPTAB_DIRECT));
}

// Store the field of the word OLD_WORD into TARGET (of MODE).
static void
brew_subword_result (rtx target, rtx old_word, rtx shift, machine_mode mode)
{
  rtx val = force_reg (SImode, expand_simple_binop (SImode, LSHIFTRT,
                                                    old_word, shift, NULL_RTX,
                                                    1, OPTAB_DIRECT));
  emit_move_insn (target, gen_lowpart (mode, val));
}

void
brew_expand_subword_cas (rtx bool_out, rtx old_out, rtx mem, rtx expected,
                         rtx desired, HOST_WIDE_INT model)
{
  machine_mode mode = GET_MODE (mem);
  rtx wmem, shift, mask, notmask;

  brew_subword_setup (mem, &wmem, &shift, &mask, &notmask);
  rtx exp_word = brew_subword_value (expected, mode, shift);
  rtx des_word = brew_subword_value (desired, mode, shift);
  rtx old_word = gen_reg_rtx (SImode);

  brew_atomic_pre_fence (model);
  emit_insn (gen_brew_masked_cassi (old_word, wmem, mask, notmask, exp_word,
                                    des_word));
  brew_atomic_post_fence (model);

  rtx old_field = force_reg (SImode, expand_simple_binop (SImode, AND,
                                                          old_word, mask,
                                                          NULL_RTX, 1,
                                                          OPTAB_DIRECT));
  emit_store_flag_force (bool_out, EQ, old_field, exp_word, SImode, 1, 1);
  brew_subword_result (old_out, old_word, shift, mode);
}

// CODE is the operation: PLUS, MINUS, AND, IOR, XOR, NOT (nand) or SET
// (exchange).
void
brew_expand_subword_rmw (rtx old_out, rtx mem, rtx val, enum rtx_code code,
                         HOST_WIDE_INT model)
{
  machine_mode mode = GET_MODE (mem);
  rtx wmem, shift, mask, notmask;

  brew_subword_setup (mem, &wmem, &shift, &mask, &notmask);
  rtx val_word = brew_subword_value (val, mode, shift);
  rtx old_word = gen_reg_rtx (SImode);

  brew_atomic_pre_fence (model);
  emit_insn (gen_brew_masked_rmwsi (old_word, wmem, mask, notmask, val_word,
                                    GEN_INT ((int) code)));
  brew_atomic_post_fence (model);
  brew_subword_result (old_out, old_word, shift, mode);
}

// The loop of brew_masked_rmwsi: compute the new field into %6 (keeping
// only the field's bits), merge it with the rest of the old word (%7) and
// store it conditionally.
const char *
brew_output_masked_rmw (rtx *operands)
{
  const char *op;

  switch ((enum rtx_code) INTVAL (operands[5]))
    {
    case SET:   op = "%6 <- %4"; break; // %4 has no bits outside the field
    case PLUS:  op = "%6 <- %0 + %4\n\t%6 <- %6 & %2"; break;
    case MINUS: op = "%6 <- %0 - %4\n\t%6 <- %6 & %2"; break;
    case AND:   op = "%6 <- %0 & %4"; break;
    case IOR:   op = "%6 <- %0 | %4\n\t%6 <- %6 & %2"; break;
    case XOR:   op = "%6 <- %0 ^ %4\n\t%6 <- %6 & %2"; break;
    case NOT:   op = "%6 <- %0 & %4\n\t%6 <- ~%6\n\t%6 <- %6 & %2"; break;
    default: gcc_unreachable ();
    }
  output_asm_insn ("1:\n\t%0 <- memll[%1]", operands);
  output_asm_insn (op, operands);
  return "%7 <- %0 & %3\n\t%6 <- %6 | %7\n\tmemsc[%1] <- %6\n\tif %6 != 0 $pc <- 1b";
}

#include "gt-brew.h"
