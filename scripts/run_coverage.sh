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
#  It is part of the rmem tool, distributed under the 2-clause BSD licence in      #
#  LICENCE.txt.                                                                    #
#                                                                                  #
#==================================================================================#

BISECT_FILE=src_model/coverage ./bytecode $@
cd src_model; bisect-report -html ../coverage_report coverage*.out
rm coverage*.out
