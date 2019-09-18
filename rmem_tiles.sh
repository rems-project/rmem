#!/bin/bash

SCRIPTNAME=rmem_tiles.sh
SCRIPTPATH="$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"

usage() {
  cat - <<__EOF__
Usage: $SCRIPTNAME [-t] [-s session] [--] <rmem-arguments>
       $SCRIPTNAME -r [-s session]
       $SCRIPTNAME -k [-s session]
       $SCRIPTNAME -h
In the 1st form, run RMEM, in interactive mode, in a screen (or tmux) session with multiple pre-configured layouts. In the 2nd and 3rd forms, reattach and kill, respectively, a detached session. In the 4th form, show this message.

The first unrecognised argument and all the following arguments are passed to RMEM.

  -t, --tmux             Use tmux instead of screen.
  -s, --session session  Use 'session' as the session name (default: rmem).
  -r, --reattach         Reattach to a previously detached session.
  -k, --kill             Kill a previously detached session.
  -h, -help, --help      Show this message.
  --                     Signal the end of options. All arguments beyond this one are passed to RMEM.

Key bindings in the screen/tmux session: ('Cmd' is the screen-command/tmux-prefix key, by default this is Ctrl-a in screen and Ctrl-b in tmux)
Cmd \       Quit (kills everything)
Tab         Change layout (loop forward)
Shift-Tab   Change layout (loop backward)

*** NOTE: after entering copy mode use the arrows and PgUp PgDn to navigate
PgUp        Scroll up the Console one page (enters copy mode, Esc to exit)
Ctrl-PgUp   Scroll up the system state one page (enters copy mode)
Ctrl-PgDn   Exit copy mode and go back to the Console

Ctrl-ARROW  Change pane

Cmd d       Detach
Cmd Esc     Enter copy mode (for scrolling, Esc to exit)
Cmd ?       Show key bindings
__EOF__

  SESSIONS=($(ls -1 /tmp/rmem_tiles/*.state.pipe /tmp/rmem_tiles/*.trace.pipe 2>/dev/null | sort -u | sed 's/\([^.]*\)\..*/"\1"/'))
  if [ -n "$SESSIONS" ]; then
    echo
    echo "Existing sessions: ${SESSIONS[@]}"
  else
    echo
    echo "No existing sessions"
  fi
}

create_pipes() {
  mkfifo "$STATEPIPE" "$TRACEPIPE"
}

remove_pipes() {
  rm -f "$STATEPIPE" "$TRACEPIPE"
}

check_session_exists() {
  if  [ -a "$STATEPIPE" ] || \
      [ -a "$TRACEPIPE" ] || \
      (which screen &>/dev/null && screen -S "$SESSION" -Q select .) || \
      (which tmux &>/dev/null && tmux has-session -t "$SESSION")
  then
    echo "Error: the session \"$SESSION\" already exists."
    echo "  reattach: $SCRIPTNAME -r -s \"$SESSION\""
    echo "  kill:     $SCRIPTNAME -k -s \"$SESSION\""
    echo "or use a different session name, see '$SCRIPTNAME -h'"
    exit 1
  fi
}

run_reattach() {
  if [ "$1" == "screen" ] || ([ -z "$1" ] && which screen &>/dev/null && screen -S "$SESSION" -Q select .); then
    screen -r "$SESSION"
    if ! screen -S "$SESSION" -Q select .; then
      remove_pipes
    fi
  elif [ "$1" == "tmux" ] || ([ -z "$1" ] && which tmux &>/dev/null && tmux has-session -t "$SESSION"); then
    tmux attach-session -t "$SESSION"
    if ! tmux has-session -t "$SESSION"; then
      remove_pipes
    fi
  else
    echo "Error: the session \"$SESSION\" does not exist (hint: use '$SCRIPTNAME -k -s \"$SESSION\"' to remove leftovers)"
    exit 1
  fi
}

run_kill() {
  if which screen &>/dev/null && screen -S "$SESSION" -Q select .; then
    screen -S "$SESSION" -X quit
  fi
  if which tmux &>/dev/null && tmux has-session -t "$SESSION"; then
    tmux kill-session -t="$SESSION"
  fi
  remove_pipes
}

SCRRENLAYOUTS=(\
  "bindkey '\\011' layout next"\
  "bindkey '^[[Z' layout prev"\
  \
  "layout new console"\
  "select Console"\
  "layout save console"\
  \
  "layout new state"\
  "select \"System state\""\
  "split"\
  "focus down"\
  "resize 25%"\
  "select Console"\
  "layout save state"\
  \
  "layout new full"\
  "select \"System state\""\
  "split -v"\
  "focus right"\
  "resize 40%"\
  "select Trace"\
  "focus left"\
  "split"\
  "focus down"\
  "resize -v 25%"\
  "select Console"\
  "layout save full"\
)

run_screen() {
  if ! which screen &>/dev/null; then
    echo "Error: screen is not installed (hint: --tmux)"
    exit 1
  fi
  check_session_exists
  create_pipes

  screen -dmS "$SESSION" -t "System state" cat "$STATEPIPE"
  screen -S "$SESSION" -X exec echo "'Ctrl-a \\\\' to quit"
  screen -S "$SESSION" -X screen -t Trace cat "$TRACEPIPE"
  screen -S "$SESSION" -X exec echo "'Ctrl-a \\\\' to quit"
  screen -S "$SESSION" -X zombie qw onerror
  screen -S "$SESSION" -X screen -t Console "${RMEM[@]}"
  # PgUp - go into copy mode and scroll up one page
  screen -S "$SESSION" -X bindkey "^[[5~" eval copy "stuff ^U"
  # C-PgUp - switch to top pane, change to copy mode, and scroll up one page
  screen -S "$SESSION" -X bindkey "^[[5;5~" eval "focus top" copy "stuff ^U"
  # C-PgDn - exit copy mode and switch to the bottom pane
  screen -S "$SESSION" -X bindkey -m "^[[6;5~" eval "stuff ^\\" "focus down"
  # C-up
  screen -S "$SESSION" -X bindkey "^[[1;5A" focus up
  # C-down
  screen -S "$SESSION" -X bindkey "^[[1;5B" focus down
  # C-right
  screen -S "$SESSION" -X bindkey "^[[1;5C" focus right
  # C-left
  screen -S "$SESSION" -X bindkey "^[[1;5D" focus left

  ## screen does not like it when you mess with the layout while being
  ## detached. Hence, initially Tab and Shift-Tab are bound to the layout setup.
  # Tab
  screen -S "$SESSION" -X bindkey '\011' eval "${SCRRENLAYOUTS[@]}" "layout select state"
  # Shift-Tab
  screen -S "$SESSION" -X bindkey '^[[Z' eval "${SCRRENLAYOUTS[@]}" "layout select full"

  # Show only the window title (without the number)
  screen -S "$SESSION" -X caption string " %t"

  run_reattach screen
}

run_tmux() {
  if ! which tmux &>/dev/null; then
    echo "Error: tmux is not installed"
    exit 1
  fi
  check_session_exists
  create_pipes

  tmux new-session -d -s "$SESSION" -n "SystemState" cat "$STATEPIPE"
  tmux send-keys -t "$SESSION" \'Ctrl-b SPACE \\\' SPACE to SPACE quit
  tmux new-window -t "$SESSION" -n Trace cat "$TRACEPIPE"
  tmux send-keys -t "$SESSION" \'Ctrl-b SPACE \\\' SPACE to SPACE quit

  tmux set-hook -t "$SESSION" pane-died "if -F '#{==:#{pane_dead_status},0}' kill-pane \"set-option display-time 0 ; display-message \\\"'Ctrl-b \\\\\\\\' to quit\\\"\""
  tmux set-option -g -t "$SESSION" remain-on-exit on
  tmux new-window -t "$SESSION" -n Console "${RMEM[@]}"
  tmux set-option -w -t "$SESSION" remain-on-exit on
  tmux set-option -u -g -t "$SESSION" remain-on-exit

  tmux set-option -t "$SESSION" '@rmem_layout' console

  ## bind-key seems to work in the server level, hence this will affect
  ## all the sessions
  tmux bind-key -n Tab \
    if -F "#{==:#{@rmem_layout},console}" \
    " join-pane -d -s :SystemState.0 -t :Console.0 -v -b -p 75 ; \
      set -t : @rmem_layout state\
    " \
    " if -F \"#{==:#{@rmem_layout},state}\" \
        \"break-pane -s :Console.1 -n SystemState ; \
          join-pane -d -s :Trace.0 -t :Console.0 -h -p 40 ; \
          join-pane -d -s :SystemState.0 -t :Console.0 -v -b -p 75 ; \
          set -t : @rmem_layout full\
        \" \
        \"break-pane -s :Console.2 -n Trace ; \
          break-pane -s :Console.1 -n SystemState ; \
          select-window -t :Console ; \
          set -t : @rmem_layout console\
        \"\
    "
  tmux bind-key -n BTab \
    if -F "#{==:#{@rmem_layout},console}" \
      " join-pane -d -s :Trace.0 -t :Console.0 -h -p 40 ; \
        join-pane -d -s :SystemState.0 -t :Console.0 -v -b -p 75 ; \
        set -t : @rmem_layout full\
      " \
      " if -F \"#{==:#{@rmem_layout},full}\" \
          \"break-pane -s :Console.2 -n Trace ; \
            select-window -t :Console ; \
            set -t : @rmem_layout state\
          \" \
          \"break-pane -s :Console.1 -n SystemState ; \
            select-window -t :Console ; \
            set -t : @rmem_layout console\
          \"\
      "
  tmux bind-key '\' confirm-before -p "Really quit (y/n)" kill-session
  tmux bind-key Escape copy
  tmux bind-key -r -n C-Up    select-pane -U
  tmux bind-key -r -n C-Down  select-pane -D
  tmux bind-key -r -n C-Left  select-pane -L
  tmux bind-key -r -n C-Right select-pane -R
  tmux bind-key -n PgUp copy -u
  tmux bind-key -n C-PgUp select-pane -U '\;' copy -u
  tmux bind-key -T copy-mode C-PgDn send-keys -X cancel '\;' select-pane -D

  run_reattach tmux
}

RUN=run_screen
SESSION=rmem

if [ "$#" == "0" ]; then
  usage
  exit 0
fi

while [ "$#" -gt 0 ]; do
  case "$1" in
    -h|-help|--help)
      usage
      exit 0
      ;;
    -s|--session)
      if [ "$#" -ge 2 ]; then
        SESSION="$2"
        shift 2
      else
        echo "Error: '$1' expects a session name (see '$SCRIPTNAME -h')"
        exit 1
      fi
      ;;
    -t|--tmux)
      shift
      RUN=run_tmux
      ;;
    -r|--reattach)
      shift
      RUN=run_reattach
      ;;
    -k|--kill)
      shift
      RUN=run_kill
      ;;
    --)
      shift
      break
      ;;
    *)
      break
      ;;
  esac
done

mkdir -p /tmp/rmem_tiles/
STATEPIPE="/tmp/rmem_tiles/${SESSION}.state.pipe"
TRACEPIPE="/tmp/rmem_tiles/${SESSION}.trace.pipe"

# Try to find the rmem executable
RMEM="$SCRIPTPATH/rmem"
if ! which "$RMEM" &>/dev/null; then
  if which rmem &>/dev/null || command -V rmem; then
    RMEM=rmem
  else
    echo "Error: cannot find rmem"
    exit 1
  fi
fi
RMEM=("$RMEM" -state_output "$STATEPIPE" -trace_output "$TRACEPIPE" "${@}" -interactive true)

"$RUN"
