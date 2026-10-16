#!/bin/sh
# Typecheck the formalization with Dedukti: base.dk, sec01.dk, sec02.dk, ...
# in order, and tests.dk if there is one (computations that must reduce).
#
#     scripts/check.sh
#
# With "eta": true in experiment.json, conversion is modulo eta (dk check --eta).
set -e
[ -f base.dk ] || { echo "no base.dk: run scripts/init.sh first"; exit 1; }
ALL=${TMPDIR:-/tmp}/autoformalize-all-$$.dk
FILES="base.dk $(ls sec*.dk 2>/dev/null | sort | tr "\n" " ")"
[ -f tests.dk ] && FILES="$FILES tests.dk"
cat $FILES > "$ALL"
ETA=$(python3 -c "import json; print('--eta' if json.load(open('experiment.json')).get('eta') else '')")
if dk check $ETA "$ALL"; then
  rm -f "$ALL"
  echo "checked: $FILES"
else
  echo "failed; the line numbers refer to $ALL, made of"
  n=0
  for f in $FILES; do
    k=$(wc -l < "$f")
    echo "  lines $((n + 1))-$((n + k)): $f (line L of the error is line L-$n of $f)"
    n=$((n + k))
  done
  exit 1
fi
