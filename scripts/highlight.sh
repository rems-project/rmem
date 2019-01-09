#! /usr/bin/env bash
# we can't use /bin/bash because OSX comes with old bash
# to install new bash:
# $ ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
# $ brew update && brew install bash

#===============================================================================#
#                                                                               #
#                rmem executable model                                          #
#                =====================                                          #
#                                                                               #
#  This file is:                                                                #
#                                                                               #
#  Copyright Shaked Flur, University of Cambridge          2016-2017            #
#  Copyright Robert Norton-Wright, University of Cambridge      2016            #
#                                                                               #
#  All rights reserved.                                                         #
#                                                                               #
#  The rmem tool is distributed under the 2-clause BSD license in LICENCE.txt.  #
#  For author information see README.md.                                        #
#                                                                               #
#===============================================================================#

if [[ "$(uname)" == "Darwin" ]]; then
  function canon_file()    { printf "%s/%s" "$(cd "$(dirname "$1")" && pwd -P)" "$(basename "$1")"; }
else
  function canon_file()    { readlink -q -m "$1"; }
fi

function relative_file() {
  canon_file "$1" | sed "s|^$(pwd)/||"
}

function warn_error_files() {
  while (( $# > 0 )) ; do
    WARN_ERROR_FILE+=":$(canon_file "${1}"):"
    shift
  done
}

function warn_error_numbers() {
  while (( $# > 0 )) ; do
    WARN_ERROR_NUMBER+=":${1}:"
    shift
  done
}

# colours for use with println (i.e. 'echo -e')
COL_RES="\e\x5B000m"
BCOL_WAR="\e\x5B045m"
FCOL_WAR="\e\x5B095m"
FCOL_FIL="\e\x5B092m"
BCOL_ERR="\e\x5B101m"
FCOL_ERR="\e\x5B091m"
FCOL_EXE="\e\x5B094m"
FCOL_COD="\e\x5B093m"

NEED_LINE_BREAK=false


function prints() {
  if "$NEED_LINE_BREAK" ; then printf "\n"; fi
  NEED_LINE_BREAK=false
  printf "$@"
}

function stresc() { if [[ -n "$1" ]] ; then printf "%q" "$1"; fi; }
function println() {
  if "$NEED_LINE_BREAK" ; then printf "\n"; fi
  NEED_LINE_BREAK=false
  printf "$@"
  printf "\n"
}

function printlndot() {
  if "$DOTS" ; then
    printf "."
    NEED_LINE_BREAK=true
  else
    println "$@"
  fi
}

# for a file name $1 and line numbers $2 and $3 ($2 <= $3)
# print lines $2 to $3 from $1
function get_lines() { head -n $3 $1 | tail -n +$2; }

function print_code() {
  local file="$1"
  local fstln="$2"
  local lstln="$3"
  local fstchar="$4"
  local lstchar="$5"

  if [[ ! -f "$file" && -f "_build/$file" ]]; then
    local file="_build/$file"
    prints "(actually ${FCOL_FIL}\"%s\"${COL_RES})\n" "$file"
  fi

  local code="$(get_lines "$file" "$fstln" "$lstln")"
  local c=$(( lstln + 1 ))
  while (( lstchar > ${#code} )) ; do
    code+="$(printf "\r%s" "$(get_lines "$file" "$c" "$c")")"
    #"$(echo -e '\r')$(get_lines "$file" "$c" "$c")"
    c=$(( c + 1 ))
  done

  prints "%s${FCOL_COD}%s${COL_RES}%s\n" \
    "$(echo "${code:0:${fstchar}}" | tr '\r' '\n')" \
    "$(echo "${code:${fstchar}:$((lstchar - fstchar))}" | tr '\r' '\n')" \
    "$(echo "${code:${lstchar}}" | tr '\r' '\n')"
}

declare -A files
declare -A warnings

function update_file() {
  local count=${files["$1"]}
  if [[ -n "$count" ]] ; then
    files["$1"]=$((count + 1))
  else
    files["$1"]=1
  fi
}

function update_warning() {
  local count=${warnings["$1"]}
  if [[ -n "$count" ]] ; then
    warnings["$1"]=$((count + 1))
  else
    warnings["$1"]=1
  fi
}

function process_input() {
  ERROR=false

  # iterate lines from $INPUT_FILE/stdin
  while IFS= read line ; do
    local cmd=($(echo "$line" | awk '{print $1, $2, $NF;}'))
    exec="$(basename "${cmd[0]}")"

    if [[ "$exec" == "ocamlfind" ]] ; then
      local file_name="$(canon_file "${cmd[2]}")"
      unset location
      if [[ -n "$FILTER_FILES" && "$FILTER_FILES" =~ ":${file_name}:" ]] ; then
        println "%s %s [...] ${FCOL_FIL}%s${COL_RES}" "${cmd[0]}" "${cmd[1]}" "${cmd[2]}"
      elif ! "$HIGHLIGHTS_ONLY" ; then
        printlndot "%s %s [...] %s" "${cmd[0]}" "${cmd[1]}" "${cmd[2]}"
      fi

    elif [[ "$exec" == "ocamlbuild" ]] ; then
      local file_name="$(canon_file "${cmd[2]}")"
      unset location
      if [[ -n "$FILTER_FILES" && "$FILTER_FILES" =~ ":${file_name}:" ]] ; then
        println "%s [...] ${FCOL_FIL}%s${COL_RES}" "${cmd[0]}" "${cmd[2]}"
      elif ! "$HIGHLIGHTS_ONLY" ; then
        printlndot "%s [...] %s" "${cmd[0]}" "${cmd[2]}"
      fi

    elif [[ "$exec" == "cp" ]] ; then
      local file_name="$(canon_file "${cmd[1]}")"
      unset location
      if [[ -n "$FILTER_FILES" && "$FILTER_FILES" =~ ":${file_name}:" ]] ; then
        println "%s [...] ${FCOL_FIL}%s${COL_RES}" "${exec}" "${cmd[2]}"
      elif ! "$HIGHLIGHTS_ONLY" ; then
        printlndot "%s [...] %s" "${exec}" "${cmd[2]}"
      fi

    elif [[ "$exec" == "pp2ml.native" ]] ; then
      local file_name="$(canon_file "${cmd[1]}")"
      unset location
      if [[ -n "$FILTER_FILES" && "$FILTER_FILES" =~ ":${file_name}:" ]] ; then
        println "%s [...] ${FCOL_FIL}%s${COL_RES}" "${exec}" "${cmd[2]}"
      elif ! "$HIGHLIGHTS_ONLY" ; then
        printlndot "%s [...] %s" "${exec}" "${cmd[2]}"
      fi

    elif [[ "$exec" == "ocamllex.opt" || "$exec" == "ocamlyacc" ]] ; then
      local file_name="$(canon_file "${cmd[2]}")"
      unset location
      if [[ -n "$FILTER_FILES" && "$FILTER_FILES" =~ ":${file_name}:" ]] ; then
        println "%s [...] ${FCOL_FIL}%s${COL_RES}" "${exec}" "${cmd[2]}"
      elif ! "$HIGHLIGHTS_ONLY" ; then
        printlndot "%s [...] %s" "${exec}" "${cmd[2]}"
      fi

    elif echo "$line" | grep -e '^File "[^"]*",' -q ; then
      unset file_name
      local location=($(echo "$line" | sed -n -e 's/^File "\([^"]*\)", line \([0-9]*\), characters \([0-9]*\)-\([0-9]*\).*/\1 \2 \3 \4/p' -e 's/^File "\([^"]*\)", line \([0-9]*\):.*/\1 \2 0 0/p'))
      if [[ -n "${location[0]}" ]] ; then
        local file_name="$(canon_file "${location[0]}")"
      fi

      if [[ -z "$FILTER_FILES" || -z "$file_name" || "$FILTER_FILES" =~ ":${file_name}:" ]] ; then
        println "$(echo "$line" | sed -n "s/^File \(\"[^\"]*\"\)/File $(stresc ${FCOL_FIL})\\1$(stresc ${COL_RES})/p")"
      elif ! "$HIGHLIGHTS_ONLY" ; then
        println "%s" "$line"
      fi

    elif echo "$line" | grep -e '^Warning [0-9]*:' -q ; then
      local warning_num="$(echo "$line" | sed -n 's/^Warning \([0-9]*\):.*/\1/p')"

      if [[ (-z "$FILTER_FILES" || -z "$file_name" || "$FILTER_FILES" =~ ":${file_name}:") &&
            (-z "$FILTER_WARNINGS" || "$FILTER_WARNINGS" =~ ":${warning_num}:") ]]
      then
        update_warning "$warning_num"

        if [[ -n "$file_name" ]] ; then
          update_file "$file_name"

          if "$WARNING_CODE" && [[ -n "${location[0]}" ]] ; then
            print_code "${location[0]}" "${location[1]}" "${location[1]}" "${location[2]}" "${location[3]}"
          fi

        else
          update_file "${FCOL_ERR}UNKNOWN${COL_RES}"
        fi

        println "$(echo "$line" | sed -n "s/^\(Warning [0-9]*:\)/$(stresc ${FCOL_WAR})\\1$(stresc ${COL_RES})/p")"
      elif ! "$HIGHLIGHTS_ONLY" ; then
        println "%s" "$line"
      fi

    elif [[ "${cmd[0]}" == "Error:" ]] ; then
      if [[ -n "${location[0]}" ]] ; then
        print_code "${location[0]}" "${location[1]}" "${location[1]}" "${location[2]}" "${location[3]}"
      fi

      ERROR=true
      println "$(echo "$line" | sed -n "s/^\(Error:\)/$(stresc ${FCOL_ERR})\\1$(stresc ${COL_RES})/p")"

    elif [[ "${cmd[0]}" == "Fatal" && "${cmd[1]}" == "error:" ]] ; then
      unset file_name
      unset location
      ERROR=true
      println "$(echo "$line" | sed -n "s/^\(Fatal error:\)/$(stresc ${FCOL_ERR})\\1$(stresc ${COL_RES})/p")"

    elif [[ "${cmd[0]}" == "+" && "${cmd[1]}" == "ocamlfind" ]] ; then
      local file_name="$(canon_file "${cmd[2]}")"
      unset location
      if [[ -z "$FILTER_FILES" || -z "$file_name" || "$FILTER_FILES" =~ ":${file_name}:" ]] ; then
        println "${FCOL_EXE}$(echo "$line" | awk '{print $1, $2, $3, "[...]", $NF;}')${COL_RES}"
      elif ! "$HIGHLIGHTS_ONLY" ; then
        printlndot "$(echo "$line" | awk '{print $1, $2, $3, "[...]", $NF;}')"
      fi

    elif [[ "${cmd[0]}" == "+" ]] ; then
      local file_name="$(canon_file "${cmd[2]}")"
      unset location
      if [[ -z "$FILTER_FILES" || -z "$file_name" || "$FILTER_FILES" =~ ":${file_name}:" ]] ; then
        println "${FCOL_EXE}+ %s [...] %s${COL_RES}" "${cmd[1]}" "${cmd[2]}"
      elif ! "$HIGHLIGHTS_ONLY" ; then
        printlndot "+ %s [...] %s" "${cmd[1]}" "${cmd[2]}"
      fi

    else
      unset file_name
      unset location
      if ! "$HIGHLIGHTS_ONLY" ; then println "%s" "$line"; fi
    fi
  done < "${INPUT_FILE:-/dev/stdin}"
}

# see ocamlc -warn-help for more details
declare -A warnings_help
warnings_help[1]="Suspicious-looking start-of-comment mark."
warnings_help[2]="Suspicious-looking end-of-comment mark."
warnings_help[3]="Deprecated feature."
warnings_help[4]="Fragile pattern matching: ..."
warnings_help[5]="Partially applied function: ..."
warnings_help[6]="Label omitted in function application."
warnings_help[7]="Method overridden."
warnings_help[8]="Partial match: missing cases in pattern-matching."
warnings_help[9]="Missing fields in a record pattern."
warnings_help[10]="Expression on the left-hand side of a sequence ..."
warnings_help[11]="Redundant case in a pattern matching (unused match case)."
warnings_help[12]="Redundant sub-pattern in a pattern-matching."
warnings_help[13]="Instance variable overridden."
warnings_help[14]="Illegal backslash escape in a string constant."
warnings_help[15]="Private method made public implicitly."
warnings_help[16]="Unerasable optional argument."
warnings_help[17]="Undeclared virtual method."
warnings_help[18]="Non-principal type."
warnings_help[19]="Type without principality."
warnings_help[20]="Unused function argument."
warnings_help[21]="Non-returning statement."
warnings_help[22]="Camlp4 warning."
warnings_help[23]="Useless record \"with\" clause."
warnings_help[24]="Bad module name: ..."
warnings_help[25]="Pattern-matching with all clauses guarded."
warnings_help[26]="Suspicious unused variable: ..."
warnings_help[27]="Innocuous unused variable: ..."
warnings_help[28]="Wildcard pattern given as argument to a constant constructor."
warnings_help[29]="Unescaped end-of-line in a string constant (non-portable code)."
warnings_help[30]="Two labels or constructors of the same name are ..."
warnings_help[31]="A module is linked twice in the same executable."
warnings_help[32]="Unused value declaration."
warnings_help[33]="Unused open statement."
warnings_help[34]="Unused type declaration."
warnings_help[35]="Unused for-loop index."
warnings_help[36]="Unused ancestor variable."
warnings_help[37]="Unused constructor."
warnings_help[38]="Unused exception constructor."
warnings_help[39]="Unused rec flag."
warnings_help[40]="Constructor or label name used out of scope."
warnings_help[41]="Ambiguous constructor or label name.2"
warnings_help[42]="Disambiguated constructor or label name."
warnings_help[43]="Nonoptional label applied as optional."
warnings_help[44]="Open statement shadows an already defined identifier."
warnings_help[45]="Open statement shadows an already defined label or constructor."

function warnings_summary() {
  for w in "${!warnings[@]}"; do
    if [[ "$WARN_ERROR_NUMBER" =~ ":${w}:" ]] ; then
      echo "${BCOL_ERR}|${warnings["$w"]}|Warning ${w}:|${warnings_help["${w}"]}${COL_RES}"
    else
      echo "${COL_RES}|${warnings["$w"]}|Warning ${w}:|${warnings_help["${w}"]}"
    fi
  done
}

function files_summary() {
  for f in "${!files[@]}"; do
    if [[ "$WARN_ERROR_FILE" =~ ":${f}:" ]] ; then
      echo "${BCOL_ERR}|${files["$f"]}|$(relative_file "${f}")${COL_RES}"
    else
      echo "${COL_RES}|${files["$f"]}|$(relative_file "${f}")"
    fi
  done
}

function print_summary() {
  local sum="$(files_summary | sort -g -t '|' -k2 | column -s '|' -t)"
  if [[ -n "$sum" ]] ; then
    println "${BCOL_WAR}Warnings summary by file:${COL_RES}"
    println "$sum"
  fi

  sum="$(warnings_summary | sort -g -t '|' -k2 | column -s '|' -t)"
  if [[ -n "$sum" ]] ; then
    println "${BCOL_WAR}Warnings summary by warning number:${COL_RES}"
    println "$sum"

    local total=0
    for w in "${!warnings[@]}"; do
      total=$(( total + ${warnings["$w"]} ))
    done
    println "${BCOL_WAR}total: %d${COL_RES}" "${total}"
  fi

  if "$ERROR" ; then println "${BCOL_ERR}ERROR${COL_RES}"; fi
}

# defaults for command line options
INPUT_FILE=
FILTER_FILES=
FILTER_WARNINGS=
BEEP=true
WARNING_CODE=false
HIGHLIGHTS_ONLY=false
DOTS=true

# parse command line arguments
function parse_args() {
  while (( $# > 0 )) ; do
    case "$1" in
      -h|--help|-help)
          echo "Usage: $(basename "$0") [OPTION]..."
          echo "Read ocamlbuild output from stdin or file and make it more readable."
          echo
          echo "Options:"
          echo "  -i, --input <file>             read ocamlbuild output from <file>."
          echo "  -c, --show-commands            don't replace commands with dots."
          echo "  -f, --highlight-file <file>    highlight messages related to <file> (repeatable)."
          echo "  -w, --highlight-warning <int>  highlight warning <int> (repeatable)."
          echo "  -a, --highlight-all            highlight all warnings and all files (default)."
          echo "  -o, --show-highlights-only     don't print lines that are not highlighted."
          echo "  -m, --no-colours               don't use colours."
          echo "  -s, --show-warning-location    show the code pointed by the warning."
          echo "  -b, --beep                     beep at the end (default)."
          echo "  -n, --no-beep                  don't beep at the end."
          echo "  -h, --help, -help              show this message."
          if [[ -z "${HIGHLIGHT_BEEP+x}" ]] && ! which beep > /dev/null ; then
          echo
          echo "Beep: to be notified at the end of the run (-b) set the environment variable HIGHLIGHT_BEEP to some sound making instruction ('export HIGHLIGHT_BEEP=' to get rid of this message)."
          fi
          exit 0
          ;;
      -m|--no-colours) COL_RES="" ; BCOL_WAR=""; FCOL_WAR=""; FCOL_FIL=""
                       BCOL_ERR=""; FCOL_ERR=""; FCOL_EXE=""; FCOL_COD="" ;;
      -c|--show-commands)        DOTS=false ;;
      -a|--highlight-all)        FILTER_FILES=""; FILTER_WARNINGS="" ;;
      -o|--show-highlights-only) HIGHLIGHTS_ONLY=true ;;
      -s|--show-warning-code)    WARNING_CODE=true ;;
      -i|--input)
          shift
          if (( $# > 0 )) ; then
              INPUT_FILE="$1"
          else
              echo "missing parameter (see --help)"
              exit 1
          fi
          ;;
      -f|--file)
          shift
          if (( $# > 0 )) ; then
              FILTER_FILES+=":$(canon_file "$1"):"
          else
              echo "missing parameter (see --help)"
              exit 1
          fi
          ;;
      -w|--warning)
          shift
          if (( $# > 0 )) ; then
              FILTER_WARNINGS+=":${1}:"
          else
              echo "missing parameter (see --help)"
              exit 1
          fi
          ;;
      -n|--no-beep) BEEP=false ;;
      -b|--beep)    BEEP=true ;;
      *) echo "unrecognised option '$1' (see --help)"; exit 1 ;;
    esac

    shift
  done
}

function check_install() {
  local missing
  for t in basename head tail grep sed awk tr ; do
    if ! which "$t" > /dev/null; then missing+=" $t"; fi
  done

  if [[ -n "$missing" ]] ; then
    echo "(highlight) missing:${missing}"
    exit 1
  fi
}

if ! which beep > /dev/null ; then function beep() { eval "${HIGHLIGHT_BEEP}"; }; fi

function cleanup() {
  if "$NEED_LINE_BREAK" ; then printf "\n"; fi
  exit 0
}

# trap ctrl-c and call cleanup()
trap cleanup INT

check_install
parse_args "$@"
process_input
print_summary
! "$BEEP" || { beep; ! "$ERROR" || beep; }
cleanup
