#! /bin/bash

#===============================================================================#
#                                                                               #
#                rmem executable model                                          #
#                =====================                                          #
#                                                                               #
#  This file is:                                                                #
#                                                                               #
#  Copyright Shaked Flur, University of Cambridge 2016-2017                     #
#                                                                               #
#  All rights reserved.                                                         #
#                                                                               #
#  It is part of the rmem tool, distributed under the 2-clause BSD licence in   #
#  LICENCE.txt.                                                                 #
#                                                                               #
#===============================================================================#

ERROR=false

function process_at() {
  local at_file="$1"
  local include_trace="$2"

  local at_file_path="$(dirname "$at_file")"

  if [[ -f "$at_file" ]]; then
    while IFS= read line ; do
      # trim while spaces from the front...
      line="${line##*([[:space:]])}"
      # ...and back of line
      line="${line%%*([[:space:]])}"
      if [[ -n "$line" ]] && [[ "${line:0:1}" != "#" ]]; then # ignore comments
        # remove the path part:
        local base="${line##*/}"
        if [[ "${base:0:1}" == "@" ]] ; then
          process_at "${at_file_path}/${line}" "$include_trace/${line}" # process @files recursively
        else
          if [[ -f "$at_file_path/$line" ]]; then
            if [[ -f "$dest/$base" ]]; then
              ERROR=true
              printf "the file '%s' already exists in $dest, included from %s\n" "$base" "$include_trace"
            else
              cp "$at_file_path/$line" "$dest/$base" # copy litmus files
            fi
          else
            ERROR=true
            printf "can't find the file '%s', included from '%s'\n" "${at_file_path}/${line}" "$include_trace"
          fi
        fi
      fi
    done < "$at_file"
  else
    ERROR=true
    printf "can't find the file '%s', included from '%s'\n" "$at_file" "$include_trace"
  fi
}

if (( $# == 2 )); then
  root_file="$1"
  dest="$2"

  mkdir -p "$dest"
  process_at "$root_file" "$root_file"
  if "$ERROR"; then exit 1; fi

else
  echo "Usage: $(basename "$0") <@file> <destination>"
  echo "copy all the files mentioned in <@file> to <destination>"
  exit 1
fi
