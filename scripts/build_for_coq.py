#=======================================================================================#
#                                                                                       #
#                rmem executable model                                                  #
#                =====================                                                  #
#                                                                                       #
#  This file is:                                                                        #
#                                                                                       #
#  Copyright Shaked Flur, University of Cambridge                                2017   #
#  Copyright Susmit Sarkar, University of St Andrews                             2014   #
#  Copyright Peter Sewell, University of Cambridge                               2014   #
#  Copyright Dominic Mulligan, University of Cambridge (when this work was done) 2013   #
#                                                                                       #
#  All rights reserved.                                                                 #
#                                                                                       #
#  It is part of the rmem tool, distributed under the 2-clause BSD licence in           #
#  LICENCE.txt.                                                                         #
#                                                                                       #
#=======================================================================================#

from subprocess import call

files = [
  "src_concurrency_model/MachineDefUtils",
  "src_concurrency_model/MachineDefFreshIds",
  "src_concurrency_model/MachineDefValue",
  "src_concurrency_model/MachineDefTypes",
  "src_concurrency_model/MachineDefInstructionSemantics",
  "src_concurrency_model/MachineDefStorageSubsystem",
  "src_concurrency_model/MachineDefThreadSubsystem",
  "src_concurrency_model/MachineDefSystem",
  "src_concurrency_model/MachineDefAxiomaticCore"
]

processed_files = []

coqc_command = ["coqc", "-require", "../../bitbucket/lem/coq-lib/coq_ext_lib_standard"]

for fname in files:
  lem_command = ["../../bitbucket/lem/lem", "-coq", "-lib", "../../bitbucket/lem/library"]
  fname_lem = fname + ".lem"
  fname_coq = fname + ".v"
  processed_files.append(fname_lem)
  for p in processed_files:
    lem_command.append(p)
  call(lem_command)
  coqc_command.append(fname_coq)
  call(coqc_command)
