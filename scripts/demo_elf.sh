#!/bin/sh

#===============================================================================#
#                                                                               #
#                rmem executable model                                          #
#                =====================                                          #
#                                                                               #
#  This file is:                                                                #
#                                                                               #
#  Copyright Shaked Flur, University of Cambridge  2017                         #
#  Copyright Peter Sewell, University of Cambridge 2017                         #
#  Copyright Jon French, University of Cambridge   2017                         #
#                                                                               #
#  All rights reserved.                                                         #
#                                                                               #
#  The rmem tool is distributed under the 2-clause BSD license in LICENCE.txt.  #
#  For author information see README.md.                                        #
#                                                                               #
#===============================================================================#


./ppcmem -model pop -suppress_internal true -pp_style compact -dwarf true -dwarf_source_path tests-adhoc/atomics_test_2.c -elf_threads 3 -allow_partial true -new_run -cmds 'set always_print on' $@

#-suppress_newpage true 
