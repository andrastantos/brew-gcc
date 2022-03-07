/* Prototypes for moxie.c functions used in the md file & elsewhere.
   Copyright (C) 2009-2021 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

extern void  brew_expand_prologue(void);
extern void  brew_expand_epilogue(bool is_sibcall);
extern int   brew_initial_elimination_offset (int, int);
extern const char *brew_emit_cbranch(machine_mode mode, rtx *operands, int insn_len);
extern const char *brew_emit_bcond(machine_mode mode, int condition, bool reverse, rtx *operands);
extern void brew_expand_call(machine_mode mode, rtx *operands);
extern bool brew_soft_float();
extern rtx brew_dynamic_chain_address(rtx frameaddr);
extern rtx brew_return_addr_rtx(unsigned int count, rtx frame);
extern rtx brew_eh_return_handler_rtx();
extern rtx brew_eh_return_stackadj_rtx();