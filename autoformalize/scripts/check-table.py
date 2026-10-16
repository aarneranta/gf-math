#!/usr/bin/env python3
"""Check the symbol tables against the formalization with RunInformath.

    python3 scripts/check-table.py               # base.dkgf and every secNN.dkgf
    python3 scripts/check-table.py sec03.dkgf    # just this one

For each table:

* every entry must parse with Informath's grammar; if the table as a whole
  does not build, each entry is tried alone, and the ones that fail are shown
  with Informath's message (CANNOT PARSE: a word missing from the grammar, or
  a shape the grammar has no example form for);
* each form of a constant must fit the constant's type: its value category
  (Prop, Kind, Exp, Proof) and the arguments the form shows (MISMATCHING
  TYPES), and the #k it shows must be arguments the constant has (ARGUMENT
  OUT OF RANGE);
* a constant of the unit that is not a theorem should have an entry, or it
  can only be read as \\mathrm{c}(...) (NO ENTRY).  Theorems need none.

The exit status is 1 if anything but NO ENTRY was found.
"""
import os, re, subprocess, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import config, units, dk_file, table_file, blocks_of, is_theorem, need_informath

ADVICE = {
    'CANNOT PARSE': 'reword the entry with the shapes of the symbol-table skill, '
                    'or add the word to the grammar (grammar-extension skill)',
    'MISMATCHING TYPES': 'the form takes another number or kind of arguments than '
                         'the constant shows; see the symbol-table skill',
    'ARGUMENT OUT OF RANGE': 'the form uses a #k beyond the arguments of the constant',
    'NO ENTRY': 'give it a verbal and a symbolic form, unless it is only an '
                'auxiliary constant',
}


def run(table, base):
    r = subprocess.run(['RunInformath', '-base=' + base, table], capture_output=True, text=True)
    return r.returncode, r.stdout, r.stderr


def entries(table):
    """The entry lines of a table: not comments, not # lines."""
    return [l for l in open(table).read().split('\n')
            if l.strip() and not l.startswith('#') and ':' in l]


def unparsable(table, base):
    tmp = '.check-one.dkgf'
    out = []
    for line in entries(table):
        open(tmp, 'w').write(line + '\n')
        code, _, err = run(tmp, base)
        if code:
            msg = err.strip().split('\n')[0]
            out.append((line, msg))
    os.remove(tmp)
    return out


def check(table, base, theorems):
    problems = []
    code, out, err = run(table, base)
    if code:
        for line, msg in unparsable(table, base):
            problems.append(('CANNOT PARSE', '%s   -- %s' % (line, msg)))
        if not problems:
            problems.append(('CANNOT PARSE', err.strip().split('\n')[0]))
        return problems
    for line in out.split('\n'):
        m = re.match(r'## (MISSING IN TABLE|MISMATCHING TYPES|ARGUMENT OUT OF RANGE): (.*)', line)
        if not m:
            continue
        kind, rest = m.groups()
        if kind == 'MISSING IN TABLE':
            name = rest.strip()
            if name in theorems or name not in theorems.universe:
                continue
            problems.append(('NO ENTRY', name))
        else:
            problems.append((kind, rest))
    return problems


class Names(set):
    """The theorems of a unit, with the set of all its declared names."""
    universe = set()


def main():
    need_informath()
    config()
    args = sys.argv[1:]
    todo = []
    if not args or 'base.dkgf' in args:
        if os.path.exists('base.dkgf'):
            todo.append(('base.dkgf', 'base.dk'))
    for n in units():
        if (not args or table_file(n) in args) and os.path.exists(table_file(n)):
            todo.append((table_file(n), dk_file(n)))
    bad = 0
    for table, dk in todo:
        theorems = Names()
        decls = blocks_of(open(dk).read())
        theorems.update(name for name, b in decls if is_theorem(b))
        # base.dk is mostly Informath's base, whose entries are in share/:
        # only the errors of base.dkgf are reported, not what has no entry
        theorems.universe = set() if table == 'base.dkgf' else {name for name, _ in decls}
        problems = check(table, dk, theorems)
        print('%s: %s' % (table, 'OK' if not problems else '%d to look at' % len(problems)))
        for kind in ADVICE:
            ps = [p for k, p in problems if k == kind]
            if ps:
                print('  %s (%s):' % (kind, ADVICE[kind]))
                for p in ps:
                    print('    ' + p)
        bad += sum(1 for k, _ in problems if k != 'NO ENTRY')
    sys.exit(1 if bad else 0)


if __name__ == '__main__':
    main()
