#===============================================================================#
#                                                                               #
#                rmem executable model                                          #
#                =====================                                          #
#                                                                               #
#  This file is:                                                                #
#                                                                               #
#  Copyright Shaked Flur, University of Cambridge 2017-2018                     #
#                                                                               #
#  All rights reserved.                                                         #
#                                                                               #
#  It is part of the rmem tool, distributed under the 2-clause BSD licence in   #
#  LICENCE.txt.                                                                 #
#                                                                               #
#===============================================================================#

PPCTESTSDIR = ../litmus-tests-power-private/tests
AArch64TESTSDIR = ../litmus-tests-armv8a-private/tests
RISCVTESTSDIR = ../litmus-tests-riscv/tests
x86TESTSDIR = ../litmus-tests-x86-private/tests
# MIPSTESTSDIR = ../litmus-tests-mips-private/tests

$(INSTALLDIR)/tests:
	mkdir -p $(INSTALLDIR)/tests
# TODO: when the tests repository is made public we can just checkout a copy
	cp -ar $(PPCTESTSDIR) $(INSTALLDIR)/tests/PPC
	cp -ar $(AArch64TESTSDIR) $(INSTALLDIR)/tests/AArch64
	cp -ar $(RISCVTESTSDIR) $(INSTALLDIR)/tests/RISCV
	cp -ar $(x86TESTSDIR) $(INSTALLDIR)/tests/x86

# $(eval $(call gen-at,<arch>,<name>,<pp name>,<@file>)) will add rules
# for generating the tests file <name> in the install folder for architecture
# <arch>, including all the files pointed by <@file>; <pp name> will be
# used in the web-interface;
define gen-at
$$(INSTALLDIR)/tests/$(1)/$(2): $(INSTALLDIR)/tests
	cd $$(dir $$@) && msort7 $(4) > $$@

install_$(1)_tests: $$(INSTALLDIR)/tests/$(1)/$(2)

.PHONY: library_json_$(1)_$(2)
library_json_$(1)_$(2):
	echo '      { "folder": "$(2)", "name": "$(3)" }'

library_json_$(1)-categories: library_json_$(1)_$(2)
endef

# $(eval $(call gen-prefix,<arch>,<name>,<pp name>,<tests-path>,<prefixes>)) will
# add rules for generating the tests file <name> in the install folder
# for architecture <arch>, including all the litmus files whose name start
# with any of the strings in <prefixes> (space separated list) in <tests-path>;
# the variable <arch>_PRUNES can be set to exclude subfolders of <tests-path>
# from the search; <pp name> will be used in the web-interface;
define gen-prefix
$$(INSTALLDIR)/tests/$(1)/$(2): $(INSTALLDIR)/tests
	cd $$(dir $$@) && find $(4) \( -false $$(foreach p,$$($(1)_PRUNES),-o -path $$(p)) \) -prune -o \( -false $(foreach p,$(5),-o -name "$(p).litmus" -o -name "$(p)+*.litmus") \) -print | msort7 > $$@

install_$(1)_tests: $$(INSTALLDIR)/tests/$(1)/$(2)

.PHONY: library_json_$(1)_$(2)
library_json_$(1)_$(2):
	echo '      { "folder": "$(2)", "name": "$(3)" }'

library_json_$(1)-categories: library_json_$(1)_$(2)
endef

gen-basic-shapes += $(eval $(call gen-prefix,$(1),MP.files,MP: Message Passing,non-mixed-size,MP))
gen-basic-shapes += $(eval $(call gen-prefix,$(1),LB.files,LB: Load Buffering,non-mixed-size,LB))
gen-basic-shapes += $(eval $(call gen-prefix,$(1),SB.files,SB: Store Buffer forwarding,non-mixed-size,SB))
gen-basic-shapes += $(eval $(call gen-prefix,$(1),R.files,R,non-mixed-size,R))
gen-basic-shapes += $(eval $(call gen-prefix,$(1),S.files,S,non-mixed-size,S))
gen-basic-shapes += $(eval $(call gen-prefix,$(1),2+2W.files,2+2W,non-mixed-size,2+2W))
gen-basic-shapes += $(eval $(call gen-prefix,$(1),RWC.files,RWC: Read to Write Causality,non-mixed-size,RWC))
gen-basic-shapes += $(eval $(call gen-prefix,$(1),WRC.files,WRC: Write to Read Causality,non-mixed-size,WRC))
gen-basic-shapes += $(eval $(call gen-prefix,$(1),WWC.files,WWC,non-mixed-size,WWC))
gen-basic-shapes += $(eval $(call gen-prefix,$(1),ISA2.files,ISA2: B cumulativity,non-mixed-size,ISA2))
gen-basic-shapes += $(eval $(call gen-prefix,$(1),Z6.files,Z6.*,non-mixed-size,Z6.0 Z6.1 Z6.2 Z6.3 Z6.4 Z6.5))
gen-basic-shapes += $(eval $(call gen-prefix,$(1),3.LB.files,3.LB,non-mixed-size,3.LB))
gen-basic-shapes += $(eval $(call gen-prefix,$(1),3.SB.files,3.SB,non-mixed-size,3.SB))
gen-basic-shapes += $(eval $(call gen-prefix,$(1),3.2W.files,3.2W,non-mixed-size,3.2W))

define arch-json
.PHONY: library_json_$(1) library_json_$(1)-categories
library_json_$(1):
	echo '  {'
	echo '    "arch": "$(1)",'
	echo '    "title": "$(2)",'
	echo '    "categories": ['
#	the sed part (which evaluates to: sed '$!s/$/,/') adds a comma
#	at the end of each line except the last one (JSON does not allow
#	trailing commas)
	$(MAKE) -s --no-print-directory library_json_$(1)-categories | sed '$$$$!s/$$$$/,/'
	echo '    ]'
#	|COMMA| will later be replaced with an actual comma (except for
#	the last one, which will be removed instead)
	echo '  }|COMMA|'
endef


.PHONY: $(INSTALLDIR)/litmus_library.json
$(INSTALLDIR)/litmus_library.json:
#	the sed part replaces "|COMMA|" with an actual comma, except
#	for the last line where it is removed (JSON does not allow trailing commas).
	{ echo '[' &&\
	  $(MAKE) -s --no-print-directory $(foreach isa,$(ISA_LIST),library_json_$(isa)) | sed '$$!s/|COMMA|/,/; s/|COMMA|//' &&\
	  echo ']';\
	} > $@


######################################################################

$(eval $(call arch-json,PPC,Power))
.PHONY: install_PPC_tests
# the following is needed because in the Makefile we use PPCGEN and the
# Power tests folder is called PPC
.PHONY: install_PPCGEN_tests library_json_PPCGEN
install_PPCGEN_tests: install_PPC_tests
library_json_PPCGEN: library_json_PPC

$(eval $(call gen-at,PPC,tutorial.files,Tutorial,@tutorial))
$(eval $(call gen-at,PPC,PLDI12.files,Synchronising C/C++ and POWER (PLDI12),@pldi12))
$(eval $(call gen-at,PPC,POPL17.files,Mixed-size Concurrency (POPL17),mixed-size/@popl17))
$(eval $(call gen-at,PPC,HAND.files,Hand-written,non-mixed-size/HAND/@all))
$(eval $(call gen-at,PPC,EXTRA_EXTRA.files,Extra,non-mixed-size/EXTRA_EXTRA/@all))

$(eval $(call gen-prefix,PPC,CO.files,Coherence,non-mixed-size,CO-2+2W CO-IRIW CO-LB CO-MP CO-R CO-S CO-SB CO-SBI CoWR CoWR2 CoRW CoWW2 CoWW CoRR5 CoRW1 CoRR1))
$(call gen-basic-shapes,PPC)
$(eval $(call gen-prefix,PPC,3-threads.files,Other 3 thread,non-mixed-size,WRR+2W WRW+2W W+RWC WRW+WR))
$(eval $(call gen-prefix,PPC,4-threads.files,4 thread,non-mixed-size,IRIW IRRWIW IRWIW W+RW+R+WR W+RW+WW+RR WW+RW+RR+WR))

######################################################################

$(eval $(call arch-json,AArch64,ARMv8 (AArch64)))
.PHONY: install_AArch64_tests

$(eval $(call gen-at,AArch64,tutorial.files,Tutorial,  non-mixed-size/TUTORIAL/@uni))
$(eval $(call gen-at,AArch64,POPL16.files,Modelling the ARMv8 architecture (POPL16),non-mixed-size/@popl16))
$(eval $(call gen-at,AArch64,POPL17.files,Mixed-size Concurrency (POPL17),mixed-size/HAND/@popl17))
$(eval $(call gen-at,AArch64,POPL18.files,Simplifying ARM Concurrency (POPL18),@popl18))
$(eval $(call gen-at,AArch64,HAND.files,Hand-written,non-mixed-size/HAND/@all))
$(eval $(call gen-at,AArch64,SHAKED.files,More hand-written,non-mixed-size/SHAKED/@all))
$(eval $(call gen-at,AArch64,linuxComp.files,Linux compiled,non-mixed-size/LinuxCompiled/@all))

AArch64_PRUNES += non-mixed-size/HAND
AArch64_PRUNES += non-mixed-size/SHAKED
AArch64_PRUNES += non-mixed-size/TUTORIAL
AArch64_PRUNES += non-mixed-size/LinuxCompiled

$(eval $(call gen-prefix,AArch64,CO.files,Coherence,non-mixed-size,CoRR CoRW1 CoRW2 CO-SBI CoWR0 CoWW))
$(call gen-basic-shapes,AArch64)
$(eval $(call gen-prefix,AArch64,3-threads.files,Other 3 thread,non-mixed-size,W+RWC WRR+2W WRW+2W WRW+WR))
$(eval $(call gen-prefix,AArch64,4-threads.files,4 thread,non-mixed-size,IRIW IRRWIW IRWIW W+RR+WR+WR W+RR+WR+WW W+RR+WW+RR W+RR+WW+RW W+RR+WW+WR W+RR+WW+WW W+RW+RR+WR W+RW+RR+WW W+RW+RW+RR W+RW+RW+RW W+RW+RW+WR W+RW+RW+WW W+RW+WR+WR W+RW+WR+WW W+RW+WW+RR W+RW+WW+RW W+RW+WW+WR W+RW+WW+WW))

######################################################################

$(eval $(call arch-json,RISCV,RISC-V (experimental)))
.PHONY: install_RISCV_tests

$(eval $(call gen-at,RISCV,HAND.files,Hand-written,non-mixed-size/HAND/@all))
$(eval $(call gen-at,RISCV,HAND-mixed-size.files,Mixed-size hand-written,mixed-size/HAND/@all))

RISCV_PRUNES = non-mixed-size/HAND

$(eval $(call gen-prefix,RISCV,CO.files,Coherence,non-mixed-size,CoRR CoRR+X CoRW1 CoRW2 CoRW2+X CO-SBI CoWR0 CoWW))
$(call gen-basic-shapes,RISCV)
$(eval $(call gen-prefix,RISCV,3-threads.files,Other 3 thread,non-mixed-size,WRR+2W WRW+2W W+RWC WRW+WR))
$(eval $(call gen-prefix,RISCV,4-threads.files,4 thread,non-mixed-size,IRIW IRRWIW IRWIW))

######################################################################

$(eval $(call arch-json,x86,x86 (experimental)))
.PHONY: install_x86_tests
# the following is needed because in the Makefile we use X86 and the x86
# tests folder is called x86
.PHONY: install_X86_tests library_json_X86
install_X86_tests: install_x86_tests
library_json_X86: library_json_x86

$(eval $(call gen-at,x86,HAND.files,Hand-written,non-mixed-size/HAND/@all))
$(eval $(call gen-at,x86,basic.files,DIY7 generated,non-mixed-size/BASIC_2_THREAD/@all))
# $(call gen-basic-shapes,x86)
