#!/bin/sh

#==================================================================================#
#                                                                                  #
#                rmem executable model                                             #
#                =====================                                             #
#                                                                                  #
#  This file is:                                                                   #
#                                                                                  #
#  Copyright Ohad Kammar, University of Cambridge (when this work was done) 2013   #
#  Copyright Susmit Sarkar, University of St Andrews                        2014   #
#  Copyright Shaked Flur, University of Cambridge                           2017   #
#  Copyright Peter Sewell, University of Cambridge                          2014   #
#  Copyright Kayvan Memarian, University of Cambridge                       2012   #
#                                                                                  #
#  All rights reserved.                                                            #
#                                                                                  #
#  The rmem tool is distributed under the 2-clause BSD license in LICENCE.txt.     #
#  For author information see README.md.                                           #
#                                                                                  #
#==================================================================================#

BISECT_FILE=src_model/coverage ./bytecode $@
cd src_model; bisect-report -html ../coverage_report coverage*.out
rm coverage*.out
