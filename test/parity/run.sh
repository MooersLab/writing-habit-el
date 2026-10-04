#!/bin/bash
# Compare the Elisp weekly table model with the Python one, line for line.
#
#   test/parity/run.sh WRITING_SCHEDULE_DIR WRITING_HABIT_PY_DIR WRITING_SCHEDULE_PY_DIR
#
# The three arguments are checkouts of writing-schedule (Elisp), writing-habit
# (Python), and writing-schedule-py.  Every shipped template and example table
# is read, edited in each supported way, and summarized by both models.  Any
# difference is printed and the script exits nonzero.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
ws=$1; py=$2; wspy=$3
files=$(ls "$ws"/templates/*.org "$py"/examples/*.org "$root"/examples/*.org "$ws"/projects-and-tasks.org)
tmp=$(mktemp -d)
PYTHONPATH="$py/src:$wspy/writing_schedule" python3 "$here/probe.py" $files > "$tmp/py.txt"
${EMACS:-emacs} -Q --batch -L "$root" -L "$ws" -l "$here/probe.el" $files > "$tmp/el.txt"
if diff "$tmp/py.txt" "$tmp/el.txt"; then
  echo "Parity: $(wc -l < "$tmp/py.txt") probes agree across $(echo $files | wc -w) tables."
else
  exit 1
fi
