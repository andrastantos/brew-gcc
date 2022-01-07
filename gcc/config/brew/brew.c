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
#include "builtins.h"
#include "explow.h"
#include "tm_p.h"
#include "insn-config.h"
#include "recog.h"

/* This file should be included last.  */
#include "target-def.h"


/* Worker function for TARGET_RETURN_IN_MEMORY.  */

static bool
brew_return_in_memory (const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
  const HOST_WIDE_INT size = int_size_in_bytes (type);
  return (size == -1 || size > 2 * UNITS_PER_WORD);
}

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its
   FUNCTION_DECL; otherwise, FUNC is 0.  

   We always return values in register $r0 for brew.  */

static rtx
brew_function_value (const_tree valtype, 
                      const_tree fntype_or_decl ATTRIBUTE_UNUSED,
                      bool outgoing ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (TYPE_MODE (valtype), BREW_R4);
}

/* Define how to find the value returned by a library function.

   We always return values in register $r0 for brew.  */

static rtx
brew_libcall_value (machine_mode mode,
                     const_rtx fun ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (mode, BREW_R4);
}

/* Handle TARGET_FUNCTION_VALUE_REGNO_P.

   We always return values in register $r4 for brew.  */

static bool
brew_function_value_regno_p (const unsigned int regno)
{
  return (regno == BREW_R4);
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
brew_emit_cbranch(machine_mode mode, rtx *operands)
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
     branches or nops as the case may be */
  switch (code)
    {
    case NE: return "if %s1 != %s2 $pc <- %l3";
    case EQ: return "if %s1 == %s2 $pc <- %l3";
    case GE: return "if %s1 >= %s2 $pc <- %l3";
    case GT: return "if %s1 > %s2 $pc <- %l3";
    case LE: return "if %s1 <= %s2 $pc <- %l3";
    case LT: return "if %s1 < %s2 $pc <- %l3";
    case GEU: if (compare_to_zero) return "$pc <- %l3"; else return "if %1 >= %2 $pc <- %l3";
    case GTU: return "if %1 > %2 $pc <- %l3";
    case LEU: return "if %1 <= %2 $pc <- %l3";
    case LTU: if (compare_to_zero) return ""; else return "if %1 < %2 $pc <- %l3";
    default:
      gcc_unreachable ();
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
    case NE:  return reverse ? brew_emit_bcond(mode, EQ,  false, operands) : "if %s1 != %s2 $pc <- %l3";
    case EQ:  return reverse ? brew_emit_bcond(mode, NE,  false, operands) : "if %s1 == %s2 $pc <- %l3";
    case GE:  return reverse ? brew_emit_bcond(mode, LT,  false, operands) : "if %s1 >= %s2 $pc <- %l3";
    case GT:  return reverse ? brew_emit_bcond(mode, LE,  false, operands) : "if %s1 > %s2 $pc <- %l3";
    case LE:  return reverse ? brew_emit_bcond(mode, GT,  false, operands) : "if %s1 <= %s2 $pc <- %l3";
    case LT:  return reverse ? brew_emit_bcond(mode, GE,  false, operands) : "if %s1 < %s2 $pc <- %l3";
    case GEU: return reverse ? brew_emit_bcond(mode, LTU, false, operands) : "$pc <- %l3";
    case GTU: return reverse ? brew_emit_bcond(mode, LEU, false, operands) : "if %1 > %2 $pc <- %l3";
    case LEU: return reverse ? brew_emit_bcond(mode, GTU, false, operands) : "if %1 <= %2 $pc <- %l3";
    case LTU: return reverse ? brew_emit_bcond(mode, GEU, false, operands) : "";
    default:
      gcc_unreachable ();
    }
}

bool brew_mov_operand(machine_mode mode, rtx operand, bool is_dst)
{
  int ret_val = 0;
  do {
    if (MEM_P(operand))
      {
        // Accept memory references by label, reg, or const
        if (
          GET_CODE(XEXP(operand, 0)) == LABEL_REF ||
          GET_CODE(XEXP(operand, 0)) == REG ||
          GET_CODE(XEXP(operand, 0)) == CONST_INT
        )
          {
            ret_val = 1;
            break;
          }
        // Accept register-offset references too (even PC-relative)
        if (
          GET_CODE(XEXP(operand, 0)) == PLUS &&
          (
            GET_CODE(XEXP(XEXP(operand, 0), 0)) == REG ||
            GET_CODE(XEXP(XEXP(operand, 0), 0)) == PC
          ) && (
            GET_CODE(XEXP(XEXP(operand, 0), 1)) == CONST_INT ||
            GET_CODE(XEXP(XEXP(operand, 0), 1)) == LABEL_REF
          )
        )
          {
            ret_val = 1;
            break;
          }
        // Other memory references are not OK
        ret_val = 0;
        break;
      }
    // Normal registers are OK and PC as well as a source
    if (REG_P(operand))
      {
        ret_val = 1;
        break;
      }
    if (!is_dst && GET_CODE(operand) == PC)
      {
        ret_val = 1;
        break;
      }
    // All else is only allowed as a source
    if (!is_dst)
      ret_val = general_operand(operand, mode);
  } while (false);
  return ret_val;
}

// TODO: There are a few lessons here
//       1. We shouldn't use plus or plus_const or similar here. Apparently when this thing gets called
//          it's already too late for such intelligence. We should actually generate instructions.
//       2. we shouldn't use pc_rtx either. I think that's a virtual register and this late in the game
//          we want to use the hard register $pc.
//       3. We still have the problem of a mode inconsistency: the address of the function call is in QI
//          mode (probably due to FUNCTION_MODE), but the target ($pc) is in SImode.
void brew_expand_call(machine_mode mode, rtx *operands)
{
  gcc_assert (MEM_P (operands[0]));
  // $r3 <- $pc + 12
  rtx temp_reg = gen_reg_rtx(mode);
  emit_insn(gen_addsi3(
    temp_reg,
    gen_rtx_REG(mode, BREW_PC), // pc_rtx,
    GEN_INT(12)
  ));
  // $sp <- $sp - 4
  emit_insn(gen_subsi3(
    stack_pointer_rtx,
    stack_pointer_rtx,
    GEN_INT(4)
  ));
  // mem[$sp] <- $r3
  emit_insn(gen_move_insn(
    gen_rtx_MEM(Pmode,
      plus_constant(Pmode, stack_pointer_rtx, 0, false) // Not an in-place addition
    ),
    temp_reg // gen_rtx_REG(Pmode, BREW_R12)
  ));
  /*
  emit_insn(gen_movsi(
    gen_rtx_MEM(Pmode,
      plus_constant(Pmode, stack_pointer_rtx, 0, false) // Not an in-place addition
    ),
    temp_reg // gen_rtx_REG(Pmode, BREW_R12)
  ));
  */

  //emit_insn(gen_movsi(
  //  gen_rtx_MEM(Pmode, stack_pointer_rtx),
  //  temp_reg
  //));
  // $pc <- %0
  // this instruction will be emitted in *call insn.
  //emit_call_insn(gen_movsi(pc_rtx /* gen_rtx_REG(mode, BREW_PC)*/, operands[0]));
  //emit_call_insn(gen_set_pc(operands[0]));
}

/* This is the pattern to generate temp registers

rtx
brew_emit_push(machine_mode mode, rtx *operands)
{
  rtx temp_reg = gen_reg_rtx(mode);
}




rtx
brew_emit_call(machine_mode mode, rtx *operands)
{
  rtx temp_reg = gen_reg_rtx(mode);
}

*/





//////////////////////////////////////////////////////////////
// TODO: there's a bunch of stuff here that needs to be reviews














/* Emit an error message when we're in an asm, and a fatal error for
   "normal" insns. */
static void
brew_operand_lossage (const char *msgid, rtx op)
{
  debug_rtx (op);
  output_operand_lossage ("%s", msgid);
}

/* The PRINT_OPERAND_ADDRESS worker.  */

static void
brew_print_operand_address (FILE *file, machine_mode, rtx x)
{
  switch (GET_CODE (x))
    {
    case REG:
      fprintf (file, "(%s)", reg_names[REGNO (x)]);
      break;
      
    case PLUS:
      switch (GET_CODE (XEXP (x, 1)))
        {
        case CONST_INT:
          fprintf (file, "%s,%ld", 
                   reg_names[REGNO (XEXP (x, 0))],
                   INTVAL(XEXP (x, 1)));
          break;
        case SYMBOL_REF:
          output_addr_const (file, XEXP (x, 1));
          fprintf (file, "%s", reg_names[REGNO (XEXP (x, 0))]);
          break;
        case CONST:
          {
            rtx plus = XEXP (XEXP (x, 1), 0);
            if (GET_CODE (XEXP (plus, 0)) == SYMBOL_REF 
                && CONST_INT_P (XEXP (plus, 1)))
              {
                output_addr_const(file, XEXP (plus, 0));
                fprintf (file,"%s,(%ld)",
                         reg_names[REGNO (XEXP (x, 0))],
                         INTVAL (XEXP (plus, 1)));
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

/* The PRINT_OPERAND worker.  */

static void
brew_print_operand (FILE *file, rtx x, int code)
{
  rtx operand = x;

  /* New code entries should just be added to the switch below.  If
     handling is finished, just return.  If handling was just a
     modification of the operand, the modified operand should be put in
     "operand", and then do a break to let default handling
     (zero-modifier) output the operand.  */

  /* Print an operand as without a modifier letter.  */
  switch (GET_CODE (operand))
    {
    case REG:
      {
        int regno = REGNO(operand);
        if (regno > LAST_PHYSICAL_REG)
          internal_error ("internal error: bad register: %d", regno);
        switch (code)
          {
          case 's':
          case 'f':
            fprintf (file, "%c%c%s", reg_names[regno][0], (char)(code), reg_names[regno]+1);
            break;
          case 0:
            fprintf (file, "%s", reg_names[regno]);
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
        case 's':
        case 'f':
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
        case 's':
        case 'f':
        case 0:
          /* No need to handle all strange variants, let output_addr_const
            do it for us.  */
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

/* Per-function machine data.  */
struct GTY(()) machine_function
 {
   /* Number of bytes saved on the stack for callee saved registers.  */
   int callee_saved_reg_size;

   /* Number of bytes saved on the stack for local variables.  */
   int local_vars_size;

   /* The sum of 2 sizes: locals vars and padding byte for saving the
    * registers.  Used in expand_prologue () and expand_epilogue().  */
   int size_for_adjusting_sp;
 };

/* Zero initialization is OK for all current fields.  */

static struct machine_function *
brew_init_machine_status (void)
{
  return ggc_cleared_alloc<machine_function> ();
}


/* The TARGET_OPTION_OVERRIDE worker.  */
static void
brew_option_override (void)
{
  /* Set the per-function-data initializer.  */
  init_machine_status = brew_init_machine_status;

#ifdef TARGET_BREWBOX  
  target_flags |= MASK_HAS_MULX;
#endif
}

// Determines if we wanted to save/restore the specified register
// in function prologue/epilog
static bool
reg_needs_save_restore(int regno)
{
  return
    (df_regs_ever_live_p(regno) && !call_used_or_fixed_reg_p (regno)) ||
    (regno == BREW_FP);
}

/* Compute the size of the local area and the size to be adjusted by the
 * prologue and epilogue.  */

static void
brew_compute_frame (void)
{
  /* For aligning the local variables.  */
  int stack_alignment = STACK_BOUNDARY / BITS_PER_UNIT;
  int padding_locals;
  int regno;

  /* Padding needed for each element of the frame.  */
  cfun->machine->local_vars_size = get_frame_size ();

  /* Align to the stack alignment.  */
  padding_locals = cfun->machine->local_vars_size % stack_alignment;
  if (padding_locals)
    padding_locals = stack_alignment - padding_locals;

  cfun->machine->local_vars_size += padding_locals;

  cfun->machine->callee_saved_reg_size = 0;

  /* Save callee-saved registers.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (reg_needs_save_restore(regno))
      cfun->machine->callee_saved_reg_size += 4;

  cfun->machine->size_for_adjusting_sp = 
    crtl->args.pretend_args_size
    + cfun->machine->local_vars_size 
    + (ACCUMULATE_OUTGOING_ARGS
       ? (HOST_WIDE_INT) crtl->outgoing_args_size : 0);
}

// NOTE: do_link in bfin.c is a very good example of how to generate push/pop patterns.
//       here we use the same example to generate multiple stores and a final SP adjustment.

// This function expands the instruction sequence for a function prologue.
void
brew_expand_prologue (void)
{
  int regno;
  rtx insn;

  brew_compute_frame ();

  if (flag_stack_usage_info)
    current_function_static_stack_size = cfun->machine->size_for_adjusting_sp;

  int save_cnt = 0;
  /* Save callee-saved registers.  */
  // For each register we save, we'll have to decrement SP by 4 and of course
  // make sure that further references are offsetted by that much.
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    {
      if (reg_needs_save_restore(regno))
        {
          insn = emit_insn(gen_movsi(
            gen_rtx_MEM(Pmode,
              plus_constant(Pmode, stack_pointer_rtx, -4*save_cnt, false) // Not an in-place addition
            ),
            gen_rtx_REG(Pmode, regno)
          ));
          RTX_FRAME_RELATED_P (insn) = 1;
          ++save_cnt;
        }
    }
  gcc_assert(save_cnt*4 == cfun->machine->callee_saved_reg_size);
  /* set up FP */
  insn = emit_insn(gen_movsi(
    hard_frame_pointer_rtx,
    stack_pointer_rtx
  ));
  RTX_FRAME_RELATED_P (insn) = 1;
  /* adjust SP */
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

void
brew_expand_epilogue (void)
{
  int regno;

  // We have to 'pop' the return address from the stack as well so, adjust SP by 4 extra bytes
  int save_cnt = cfun->machine->callee_saved_reg_size / 4 + 1;
  int sp_adjust = cfun->machine->size_for_adjusting_sp + save_cnt * 4;
  if (sp_adjust > 0)
    {
      /* Restore SP */
      emit_insn(
        gen_addsi3(
          stack_pointer_rtx, 
          stack_pointer_rtx, 
          GEN_INT(sp_adjust)
        )
      );
      /* Restore registers */
      for (regno = FIRST_PSEUDO_REGISTER; regno-- > 0; )
        {
          if (reg_needs_save_restore(regno))
            {
              --save_cnt;
              emit_insn(gen_movsi(
                gen_rtx_REG(Pmode, regno),
                gen_rtx_MEM(Pmode,
                  // Again: because of the return address also being on the stack, 
                  // we're offsetting everything by 4 extra bytes
                  plus_constant(Pmode, stack_pointer_rtx, -4*save_cnt -4, false) // Not an in-place addition
                )
              ));
            }
        }
    }
  // Return: we already adjusted SP, so all we have to do is to get PC from MEM[SP]
  emit_jump_insn (gen_returner ());
}

/* Implements the macro INITIAL_ELIMINATION_OFFSET, return the OFFSET.  */
/* That is: used to figure out the offset between $?fp, $?ap and $fp */
int
brew_initial_elimination_offset (int from, int to)
{
  int ret;
  
  if ((from) == FRAME_POINTER_REGNUM && (to) == HARD_FRAME_POINTER_REGNUM)
    {
      /* Compute this since we need to use cfun->machine->local_vars_size.  */
      brew_compute_frame ();
      ret = -cfun->machine->callee_saved_reg_size;
    }
  else if ((from) == ARG_POINTER_REGNUM && (to) == HARD_FRAME_POINTER_REGNUM)
    ret = 0x00;
  else
    abort ();

  return ret;
}

/* Worker function for TARGET_SETUP_INCOMING_VARARGS.  */

static void
brew_setup_incoming_varargs (cumulative_args_t cum_v,
                              const function_arg_info &,
                              int *pretend_size, int no_rtl)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int regno;
  int regs = 8 - *cum;
  
  *pretend_size = regs < 0 ? 0 : GET_MODE_SIZE (SImode) * regs;
  
  if (no_rtl)
    return;
  
  for (regno = *cum; regno < 8; regno++)
    {
      rtx reg = gen_rtx_REG (SImode, regno);
      rtx slot = gen_rtx_PLUS (Pmode,
                               gen_rtx_REG (SImode, ARG_POINTER_REGNUM),
                               GEN_INT (UNITS_PER_WORD * (3 + (regno-2))));
      
      emit_move_insn (gen_rtx_MEM (SImode, slot), reg);
    }
}


/* Return the fixed registers used for condition codes.  */
/*
static bool
brew_fixed_condition_code_regs (unsigned int *p1, unsigned int *p2)
{
  gc_assert(false);
  return false;
}
*/

static bool
arg_can_be_in_register(const function_arg_info &arg)
{
  if (!arg.named)
    return false;

  if (arg.aggregate_type_p())
    return false;

  HOST_WIDE_INT arg_size = arg.promoted_size_in_bytes();
  return arg_size <= 4 && arg_size > 0;
}

// The following two functions work in tandem:
//   brew_function_arg decides in what form the argument should be passed.
//     It returns either a REG rtx specifying the register to be used or
//     NULL_RTX if the arg to be passed on the stack.
//     In helping with deciding *which* register to chose, an argument
//     cum_v.p (retrieved using get_cumulative_args) is used. This is
//     defined as a simply 'unsigned int' to count the number of args
//     that have already been passed in registers.
//   brew_function_arg_advance is used to update cum_v.p after the 
//     decision in the first function is made. It must therefore come
//     to the same conclusion in terms of using a register or stack
//     and increment cum_v.p as needed.

// Return the next register to be used to hold a function argument or
// NULL_RTX if there's no more space.
static rtx
brew_function_arg (cumulative_args_t cum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  if (*cum < 4 && arg_can_be_in_register(arg))
    return gen_rtx_REG (arg.mode, BREW_R4 + *cum);
  else 
    return NULL_RTX;
}

#define BREW_FUNCTION_ARG_SIZE(MODE, TYPE)        \
  ((MODE) != BLKmode ? GET_MODE_SIZE (MODE)        \
   : (unsigned) int_size_in_bytes (TYPE))

static void
brew_function_arg_advance (cumulative_args_t cum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  if (*cum < 4 && arg_can_be_in_register(arg))
    *cum += 1;
}

/* Return non-zero if the function argument described by ARG is to be
   passed by reference.  */

static bool
brew_pass_by_reference (cumulative_args_t, const function_arg_info &arg)
{
  if (arg.aggregate_type_p())
    return true;
  HOST_WIDE_INT size = arg.type_size_in_bytes();
  return size > 16 || size <= 0;
}

/* Some function arguments will only partially fit in the registers
   that hold arguments. Given a new arg, return the number of bytes
   that fit in argument passing registers. */
// For now, we're simply choosing an all-or-nothing approach:
//   we either completely put the argument in registers or completely
//   on the stack.
static int
brew_arg_partial_bytes (cumulative_args_t cum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int bytes_left, size;

  if (*cum >= 4)
    return 0;

  if (brew_pass_by_reference (cum_v, arg))
    return 4;
  if (!arg_can_be_in_register(arg))
    return 0;
  return arg.promoted_size_in_bytes();
}

/* Worker function for TARGET_STATIC_CHAIN.  */

static rtx
brew_static_chain (const_tree ARG_UNUSED (fndecl_or_type), bool incoming_p)
{
  rtx addr, mem;

  if (incoming_p)
    addr = plus_constant (Pmode, arg_pointer_rtx, 2 * UNITS_PER_WORD);
  else
    addr = plus_constant (Pmode, stack_pointer_rtx, -UNITS_PER_WORD);

  mem = gen_rtx_MEM (Pmode, addr);
  MEM_NOTRAP_P (mem) = 1;

  return mem;
}

/* Worker function for TARGET_ASM_TRAMPOLINE_TEMPLATE.  */

static void
brew_asm_trampoline_template (FILE *f)
{
  fprintf (f, "\tpush  $sp, $r0\n");
  fprintf (f, "\tldi.l $r0, 0x0\n");
  fprintf (f, "\tsto.l 0x8($fp), $r0\n");
  fprintf (f, "\tpop   $sp, $r0\n");
  fprintf (f, "\tjmpa  0x0\n");
}

/* Worker function for TARGET_TRAMPOLINE_INIT.  */

static void
brew_trampoline_init (rtx m_tramp, tree fndecl, rtx chain_value)
{
  rtx mem, fnaddr = XEXP (DECL_RTL (fndecl), 0);

  emit_block_move (m_tramp, assemble_trampoline_template (),
                   GEN_INT (TRAMPOLINE_SIZE), BLOCK_OP_NORMAL);

  mem = adjust_address (m_tramp, SImode, 4);
  emit_move_insn (mem, chain_value);
  mem = adjust_address (m_tramp, SImode, 16);
  emit_move_insn (mem, fnaddr);
}

/* Return true for reg+memory references */
bool
brew_offset_address_p (rtx x)
{
  x = XEXP (x, 0);

  if (GET_CODE (x) == PLUS)
    {
      x = XEXP (x, 1);
      if (GET_CODE (x) == CONST_INT)
        {
          return true;
        }
    }
  return false;
}

/* Helper function for `brew_legitimate_address_p'.  */

static bool
brew_reg_ok_for_base_p (const_rtx reg, bool strict_p)
{
  int regno = REGNO (reg);

  if (strict_p)
    return HARD_REGNO_OK_FOR_BASE_P (regno)
           || HARD_REGNO_OK_FOR_BASE_P (reg_renumber[regno]);
  else    
    return !HARD_REGISTER_NUM_P (regno)
           || HARD_REGNO_OK_FOR_BASE_P (regno);
}

/* Worker function for TARGET_LEGITIMATE_ADDRESS_P.  */

static bool
brew_legitimate_address_p (machine_mode mode ATTRIBUTE_UNUSED,
                            rtx x, bool strict_p,
                            addr_space_t as)
{
  gcc_assert (ADDR_SPACE_GENERIC_P (as));

  if (GET_CODE(x) == PLUS
      && REG_P (XEXP (x, 0))
      && brew_reg_ok_for_base_p (XEXP (x, 0), strict_p)
      && CONST_INT_P (XEXP (x, 1))
      && IN_RANGE (INTVAL (XEXP (x, 1)), -32768, 32767))
    return true;
  if (REG_P (x) && brew_reg_ok_for_base_p (x, strict_p))
    return true;
  if (GET_CODE (x) == SYMBOL_REF
      || GET_CODE (x) == LABEL_REF
      || GET_CODE (x) == CONST)
    return true;
  return false;
}

/* The Global `targetm' Variable.  */

/* Initialize the GCC target structure.  */

#undef  TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES        hook_bool_const_tree_true

#undef  TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY                brew_return_in_memory
#undef  TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK        must_pass_in_stack_var_size
#undef  TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE        brew_pass_by_reference
#undef  TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES        brew_arg_partial_bytes
#undef  TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG                brew_function_arg
#undef  TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE        brew_function_arg_advance

#undef TARGET_LRA_P
#define TARGET_LRA_P hook_bool_void_false

#undef  TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P
#define TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P        brew_legitimate_address_p

#undef  TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS         brew_setup_incoming_varargs

//#undef        TARGET_FIXED_CONDITION_CODE_REGS
//#define        TARGET_FIXED_CONDITION_CODE_REGS brew_fixed_condition_code_regs

/* Define this to return an RTX representing the place where a
   function returns or receives a value of data type RET_TYPE, a tree
   node representing a data type.  */
#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE brew_function_value
#undef TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE brew_libcall_value
#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P brew_function_value_regno_p

#undef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED hook_bool_void_true

#undef TARGET_STATIC_CHAIN
#define TARGET_STATIC_CHAIN brew_static_chain
#undef TARGET_ASM_TRAMPOLINE_TEMPLATE
#define TARGET_ASM_TRAMPOLINE_TEMPLATE brew_asm_trampoline_template
#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT brew_trampoline_init

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE brew_option_override

#undef  TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND brew_print_operand
#undef  TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS brew_print_operand_address

#undef  TARGET_CONSTANT_ALIGNMENT
#define TARGET_CONSTANT_ALIGNMENT constant_alignment_word_strings

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-brew.h"
