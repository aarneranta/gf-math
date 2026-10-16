#!/usr/bin/env python3
"""Read the formalization back with Informath, declaration by declaration.

    python3 scripts/build-informal.py              # English    -> secNN-informal.tsv
    python3 scripts/build-informal.py -symbolic    # formulas   -> secNN-symbolic.tsv
    python3 scripts/build-informal.py -closest     # the English reading closest
                                                   # to the source -> secNN-informal.tsv
    python3 scripts/build-informal.py sec02.dk     # just these units (any mode)

Each .tsv has one line per declaration: its name and its reading.  Also
writes informal-macros.tex, the \\newcommand definitions of the macros that
the readings use.  All modes read the symbol tables base.dkgf, sec01.dkgf, ...
as one table, in this order: a macro is named after its position in it, so
the readings of all units and the macros have to be rebuilt together after
a LaTeX form is added or removed anywhere.

A unit is read back from its statements alone (the proofs dropped), with
base.dk and the units before it as its base.  RunInformath prints one
sentence per declaration, in file order, labelled with the name of a theorem
or with "Definition", and the sentences are aligned with the declarations by
these labels.  A unit gets UNIT_TIMEOUT seconds; what it leaves unpaired is
read back declaration by declaration, with DECL_TIMEOUT seconds each, and
with -force-symbolic if it gives no sentence otherwise.  A declaration that
runs out of time is reported as TIMED OUT: it is too big to word, and is
best split into smaller statements, as the source usually does.

-closest asks Informath for the 50 best readings of each declaration
(-trees -nbest=50 -godement: also with typed variables, "is said to be",
"is called" ...) and keeps the one whose words are closest to the text of the
item the declaration is tagged with; secNN-closest.tsv records the
similarity, the item and the number of readings compared.  Run it after the
plain mode, whose readings it keeps for what has no tagged item.
"""
import difflib, json, os, re, subprocess, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import config, units, dk_file, tables, blocks_of, is_theorem, tags, need_informath

UNIT_TIMEOUT = 120   # seconds
DECL_TIMEOUT = 30
CLOSEST_FLAGS = ['-trees', '-nbest=50', '-godement']
MACROS = 'informal-macros.tex'
LABEL = re.compile(r'^([A-Za-z0-9\\_]+)\.\s')
# \textbf{Definition}. $...$  or  \textbf{Axiom} \texttt{\detokenize{name}}. $...$
SYMBOLIC_LABEL = re.compile(r'^\\textbf\{(\w+)\}(?: \\texttt\{\\detokenize\{([^}]*)\}\})?\.\s*')


def write_bases():
    """.base-NN.dk: base.dk and the units before NN, which unit NN builds on."""
    acc = open('base.dk').read()
    for n in units():
        open('.base-%02d.dk' % n, 'w').write(acc)
        acc += '\n' + open(dk_file(n)).read()


def declarations(path):
    """(name, expected label) for each declaration and rewrite rule, in file order."""
    out = []
    for name, block in blocks_of(open(path).read()):
        if ' rule ' in name:
            out.append((name, 'Rule'))
        else:
            out.append((name, name[0].upper() + name[1:] if is_theorem(block) else 'Definition'))
    return out


def statements_only(path):
    """The unit with the proofs of its theorems dropped, as a temporary .dk."""
    out = []
    for b in re.split(r'\n(?=(?:def )?[A-Za-z_0-9]+ :)', open(path).read()):
        if re.match(r'(?:def )?[A-Za-z_0-9]+ :', b) and ':=' in b and is_theorem(b):
            head = b.split(':=')[0].rstrip()
            b = (head[4:] if head.startswith('def ') else head) + '.'
        out.append(b)
    tmp = '.' + path[:-3] + '-statements.dk'   # the suffix decides how it is read
    open(tmp, 'w').write('\n'.join(out))
    return tmp


def combined_table():
    tmp = '.informal-table.dkgf'
    open(tmp, 'w').write('\n'.join(open(t).read() for t in tables()))
    return tmp


def readback(dk, base, table, symbolic=False, flags=(), timeout=None):
    """The output lines, or None if RunInformath ran out of time."""
    cmd = ['RunInformath', '-add-symboltables=' + table] + list(flags)
    if symbolic:
        cmd.append('-to-symbolic-latex')
    cmd += ['-base=' + base, dk]
    try:
        r = subprocess.run(cmd, capture_output=True, text=True, timeout=timeout)
    except subprocess.TimeoutExpired:
        return None
    if r.returncode:
        sys.exit('RunInformath failed:\n  %s\n%s' % (' '.join(cmd), r.stderr[-2000:]))
    return [l for l in r.stdout.split('\n') if l.strip()]


def one_by_one(src, unit_dk, base, table, names, flags=(), symbolic=False):
    """Read the named declarations back one at a time, with the whole unit in
    the base.  Returns name -> output lines, the names that needed
    -force-symbolic, and the names that ran out of time."""
    blocks = dict(blocks_of(open(src).read()))
    tmp = '.retry-statements.dk'
    bases = base + ',' + unit_dk
    found, forced, timedout = {}, [], []
    for name in names:
        open(tmp, 'w').write(blocks[name].strip() + '\n')
        lines = readback(tmp, bases, table, symbolic, flags, timeout=DECL_TIMEOUT)
        if not lines:   # no sentence, or out of time: formulas where words fail
            lines = readback(tmp, bases, table, symbolic, list(flags) + ['-force-symbolic'],
                             timeout=DECL_TIMEOUT)
            if lines is None:
                timedout.append(name)
                continue
            if lines:
                forced.append(name)
        if lines:
            found[name] = lines
    if os.path.exists(tmp):
        os.remove(tmp)
    return found, forced, timedout


def macros(table):
    """The macros of the LaTeX forms of the tables, and the #MACRO lines of
    Informath's share/baseconstants.dkgf (\\Natural and the like), which
    RunInformath reads before the tables but does not print here."""
    r = subprocess.run(['RunInformath', '-base=base.dk', table], capture_output=True, text=True)
    if r.returncode:
        sys.exit('RunInformath failed on the symbol tables:\n' + r.stderr[-2000:])
    ms = {l for l in r.stdout.split('\n') if l.startswith('\\newcommand')}
    share = os.path.join(os.environ['INFORMATH_ROOT'], 'share', 'baseconstants.dkgf')
    ms |= {l[len('#MACRO '):].strip() for l in open(share) if l.startswith('#MACRO ')}
    return sorted(ms)


def align(decls, lines, symbolic=False):
    """name -> sentence, and the names that got none."""
    if symbolic:
        labels = [(lambda m: ('Rule' if m.group(1) == 'Rules' else m.group(2) or m.group(1)) if m else '?')
                  (SYMBOLIC_LABEL.match(l)) for l in lines]
        expected = [n if e not in ('Definition', 'Rule') else e for n, e in decls]
    else:
        labels = [(LABEL.match(l).group(1).replace('\\_', '_') if LABEL.match(l)
                   else 'Rule' if l.startswith('By cases') else '?') for l in lines]
        expected = [e for _, e in decls]
    pairs, missing = {}, []
    sm = difflib.SequenceMatcher(a=expected, b=labels, autojunk=False)
    for tag, i1, i2, j1, j2 in sm.get_opcodes():
        if tag == 'equal':
            for k in range(i2 - i1):
                pairs[decls[i1 + k][0]] = lines[j1 + k]
        else:
            missing += [decls[k][0] for k in range(i1, i2)]
    return pairs, missing


def sentence(line, symbolic=False):
    """The reading without the label Informath puts in front."""
    return SYMBOLIC_LABEL.sub('', line).strip() if symbolic else LABEL.sub('', line)


def words(s, tex=False):
    """A text as lower-case words, each formula one word FORMULA."""
    if tex:
        s = re.sub(r'\\begin\{(equation|align|gather|multline)\*?\}.*?\\end\{\1\*?\}', ' FORMULA ', s, flags=re.S)
        s = re.sub(r'\\\[.*?\\\]', ' FORMULA ', s, flags=re.S)
    s = re.sub(r'\$\$.*?\$\$|\$.*?\$', ' FORMULA ', s, flags=re.S)
    s = re.sub(r'\\[A-Za-z]+\*?', ' ', s)
    return re.findall(r'[a-z]+|FORMULA', s.lower().replace('formula', 'FORMULA'))


def closest(variants, target):
    t = words(target, tex=True)
    best = None
    for lin in variants:
        score = difflib.SequenceMatcher(None, words(sentence(lin)), t, autojunk=False).ratio()
        if best is None or score > best[1]:
            best = (lin, score)
    return best


def variants_of(lines):
    """name -> its readings, from the lines of -trees: name, sentence, tree."""
    out = {}
    for l in lines:
        parts = l.split('\t')
        if len(parts) == 3 and parts[1] not in out.setdefault(parts[0], []):
            out[parts[0]].append(parts[1])
    return out


def item_texts():
    """item id and label -> the item's text; section number -> its running text."""
    if not os.path.exists('items.json'):
        sys.exit('no items.json: run scripts/extract-items.py first')
    data = json.load(open('items.json'))
    out = dict(data.get('texts', {}))     # the running text of each section
    for it in data['items']:
        out[it['id']] = it['tex']
        if it['label']:
            out[it['label']] = it['tex']
    return out


def plain_main(only, table, symbolic, c):
    for n in units():
        dk = dk_file(n)
        if only and dk not in only:
            continue
        out = dk[:-3] + ('-symbolic.tsv' if symbolic else '-informal.tsv')
        src = statements_only(dk)
        decls = declarations(src)
        lines = readback(src, '.base-%02d.dk' % n, table, symbolic, timeout=UNIT_TIMEOUT)
        if lines is None:
            print('%s: no result in %d s, reading back one declaration at a time' % (dk, UNIT_TIMEOUT))
            pairs, missing = {}, [name for name, _ in decls]
        else:
            pairs, missing = align(decls, lines, symbolic)
        forced, timedout = [], []
        if missing:
            found, forced, timedout = one_by_one(src, dk, '.base-%02d.dk' % n, table,
                                                 [m for m in missing if ' rule ' not in m],
                                                 symbolic=symbolic)
            pairs.update({k: v[0] for k, v in found.items()})
            missing = [m for m in missing if m not in found and m not in timedout]
        with open(out, 'w') as f:
            for name, _ in decls:
                if name in pairs:
                    f.write('%s\t%s\n' % (name, sentence(pairs[name], symbolic)))
        os.remove(src)
        report(dk, '%d of %d declarations paired -> %s' % (len(pairs), len(decls), out),
               forced, timedout, missing)


def closest_main(only, table, c):
    texts = item_texts()
    for n in units():
        dk = dk_file(n)
        if only and dk not in only:
            continue
        out = dk[:-3] + '-informal.tsv'
        src = statements_only(dk)
        decls = [name for name, label in declarations(src) if label != 'Rule']
        lines = readback(src, '.base-%02d.dk' % n, table, flags=CLOSEST_FLAGS, timeout=UNIT_TIMEOUT)
        found = variants_of(lines) if lines is not None else {}
        missing = [m for m in decls if m not in found]
        forced, timedout = [], []
        if missing:
            more, forced, timedout = one_by_one(src, dk, '.base-%02d.dk' % n, table, missing,
                                                flags=CLOSEST_FLAGS)
            for ls in more.values():
                found.update(variants_of(ls))
        tg = tags(dk, c)
        plain = {}
        if os.path.exists(out):
            for line in open(out):
                if '\t' in line:
                    k, t = line.rstrip('\n').split('\t', 1)
                    plain[k] = t
        chosen, rows = {}, []
        for name in decls:
            vs = found.get(name)
            if not vs:
                continue
            tag = tg.get(name, '')
            lin, score = closest(vs, texts[tag]) if tag in texts else (vs[0], 0.0)
            chosen[name] = sentence(lin)
            rows.append('%s\t%.2f\t%s\t%d' % (name, score, tag, len(vs)))
        with open(out, 'w') as f:
            for name, _ in declarations(src):
                t = chosen.get(name, plain.get(name))
                if t:
                    f.write('%s\t%s\n' % (name, t))
        open(dk[:-3] + '-closest.tsv', 'w').write('\n'.join(rows) + '\n')
        os.remove(src)
        report(dk, '%d of %d declarations chosen among readings -> %s' % (len(chosen), len(decls), out),
               forced, timedout, [m for m in decls if m not in found and m not in timedout])


def report(dk, line, forced, timedout, missing):
    print('%s: %s' % (dk, line))
    for name in forced:
        print('  FORCED SYMBOLIC: %s' % name)
    for name in timedout:
        print('  TIMED OUT: %s' % name)
    for name in missing:
        print('  NO SENTENCE: %s' % name)


def main():
    need_informath()
    c = config()
    args = sys.argv[1:]
    only = [a for a in args if not a.startswith('-')]
    write_bases()
    table = combined_table()
    try:
        if '-closest' in args:
            closest_main(only, table, c)
        else:
            plain_main(only, table, '-symbolic' in args, c)
        ms = macros(table)
        open(MACROS, 'w').write('\n'.join(ms) + '\n')
        print('%d macro definitions -> %s' % (len(ms), MACROS))
    finally:
        os.remove(table)
        for n in units():
            if os.path.exists('.base-%02d.dk' % n):
                os.remove('.base-%02d.dk' % n)


if __name__ == '__main__':
    main()
