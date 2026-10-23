#!/bin/bash

minutes=$1
sleep $((minutes * 60))
pid=$(pgrep -x simplex-chat-test | head -1)
if [ -z "$pid" ]; then
  exit 0
fi

children=$(pgrep -P "$pid")
pids="$pid $children"
for child in $children; do
  pids="$pids $(pgrep -P "$child")"
done

echo "::group::simplex-chat-test is still running after $minutes minutes"
for p in $pids; do
  ps -o pid,ppid,stat,etime,command -p "$p"
done
for p in $pids; do
  echo "=== stacks of $p"
  if [ "$(uname)" = Darwin ]; then
    sample "$p" 3 -file /dev/stdout
  else
    for task in /proc/"$p"/task/*; do
      echo "--- thread ${task##*/} $(cat "$task"/comm) $(cat "$task"/wchan)"
      sudo cat "$task"/stack
    done
  fi
done
echo "::endgroup::"
kill "$pid"
