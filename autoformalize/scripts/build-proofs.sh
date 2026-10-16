#!/bin/sh
# Write the proofs of each unit as a LaTeX document and a PDF: each theorem's
# statement, and its proof as numbered lines of English, each step justified
# by the rule it applies (RunInformath -proof-text).
#
#     scripts/build-proofs.sh                # -> proofs/secNN.tex, proofs/secNN.pdf
#     scripts/build-proofs.sh -proof-terms   # also the Dedukti proof terms
#
# Only theorems with a proof (def name : ... Proof P := proof-term) get one; a
# unit without any gets no document.  A unit builds on base.dk and the units
# before it, and all symbol tables are used.
set -e
[ -n "$INFORMATH_ROOT" ] || { echo "export INFORMATH_ROOT=<your informath checkout> first"; exit 1; }
UNITS=$(ls sec*.dk | sed 's/\.dk$//' | sort)
TABLES=$( (ls base.dkgf 2>/dev/null; for u in $UNITS; do ls $u.dkgf 2>/dev/null; done) | paste -sd, -)
mkdir -p proofs
BASE=proofs/.base.dk
cp base.dk $BASE
for u in $UNITS; do
  RunInformath -proof-text -base=$BASE -add-symboltables=$TABLES "$@" $u.dk > proofs/$u.tex
  cat $u.dk >> $BASE          # a later unit builds on the earlier ones
  if ! grep -q 'begin{tabular}' proofs/$u.tex; then
    rm -f proofs/$u.tex proofs/$u.pdf
    echo "$u: no proofs"
    continue
  fi
  if (cd proofs && xelatex -interaction=batchmode $u.tex >/dev/null 2>&1); then
    echo "proofs/$u.tex, proofs/$u.pdf"
  else
    echo "proofs/$u.tex: LaTeX errors, see proofs/$u.log"
  fi
done
rm -f $BASE proofs/*.aux proofs/*.out
