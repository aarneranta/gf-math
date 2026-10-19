#!/bin/sh
# Set up the foundation of an experiment, in the directory of experiment.json:
#
#     scripts/init.sh            # base.dk = Informath's baseconstants.dk + nd.dk
#     scripts/init.sh own        # base.dk = a skeleton for a foundation of one's own
#
# The first is right for ordinary mathematics: sets, numbers, the logical
# operators, with natural deduction for the proofs.  The second is for a
# document whose foundation differs, such as a type theory in which Elem must
# be a definition (see godement-index and hott-llm for the two cases).  Either
# way base.dk is the experiment's own copy, to be extended as needed; base.dkgf
# holds the symbol table entries of what base.dk adds, and overrides of
# Informath's share/baseconstants.dkgf, which RunInformath always reads first.
set -e
eval "$(python3 "$(dirname "$0")/common.py" env)" || exit 1   # "informath" of experiment.json
[ -n "$INFORMATH_ROOT" ] || { echo "export INFORMATH_ROOT=<your informath checkout> first"; exit 1; }
[ -f experiment.json ] || { echo "no experiment.json here; see README.md"; exit 1; }
HERE=$(cd "$(dirname "$0")/.." && pwd)
if [ -f base.dk ]; then echo "base.dk exists already; remove it to start over"; exit 1; fi
case "${1:-classical}" in
  classical)
    { echo "(; The foundation: informath/share/baseconstants.dk and the natural deduction rules ;)"
      echo
      cat "$INFORMATH_ROOT/share/baseconstants.dk"
      echo
      cat "$HERE/base/nd.dk"; } > base.dk ;;
  own)
    cat > base.dk <<'EOF'
(; The foundation of this experiment.  Informath recognizes Set, Elem, Prop
   and Proof by name: Set is the type of sets (or types), Elem A the type of
   the elements of A, Prop the type of propositions and Proof P the type of
   the proofs of P.  Add the logical operators and the rules the document
   needs; see informath/share/baseconstants.dk for the names Informath
   knows (and, or, if, not, iff, forall, exists, ...). ;)

Set : Type.
def Elem : Set -> Type.
Prop : Type.
Proof : Prop -> Type.
EOF
    ;;
  *) echo "usage: scripts/init.sh [classical|own]"; exit 1 ;;
esac
[ -f base.dkgf ] || cat > base.dkgf <<'EOF'
# Symbol table entries of the constants base.dk adds to Informath's base, and
# overrides of share/baseconstants.dkgf (a later entry wins).
EOF
[ -f .gitignore ] || printf '.base-*.dk\n.*-statements.dk\n.retry-statements.dk\n.informal-table.dkgf\nproofs/.base.dk\n__pycache__/\n' > .gitignore
dk check base.dk
echo "base.dk and base.dkgf ready"
