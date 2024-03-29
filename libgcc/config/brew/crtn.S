# crtn.S for brew
# 
#   Copyright (C) 2009-2021 Free Software Foundation, Inc.
# 
# This file is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3, or (at your option) any
# later version.
# 
# This file is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
# 
# Under Section 7 of GPL version 3, you are granted additional
# permissions described in the GCC Runtime Library Exception, version
# 3.1, as published by the Free Software Foundation.
#
# You should have received a copy of the GNU General Public License and
# a copy of the GCC Runtime Library Exception along with this program;
# see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
# <http://www.gnu.org/licenses/>.

# This file just makes sure that the .fini and .init sections do in
# fact return.  Users may put any desired instructions in those sections.
# This file is the last thing linked into any executable.

# We don't use .init and .fini sections to collect code-fragments.
# We use .init_array and .fini_array for that purpose
# .init is only used in crt0.S (inside Newlib) to put the entry-point
# at the beginning of the executable.

#  .file    "crtn.S"
#
#  .section  ".init"
#  $r3 <- mem[$sp]
#  $sp <- $sp + 4
#  $pc <- $r3
#  
#  .section  ".fini"
#  $r3 <- mem[$sp]
#  $sp <- $sp + 4
#  $pc <- $r3
