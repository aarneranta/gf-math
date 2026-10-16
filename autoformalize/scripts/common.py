"""What the scripts of an autoformalization experiment share: the settings in
experiment.json, the formalization files, and the way declarations are found
and tagged in them.

The formalization is base.dk (the foundation: Informath's baseconstants.dk and
the natural deduction rules, or a base of the experiment's own) and one file
secNN.dk per top-level unit (section, or chapter) NN of the source, each with
its symbol table secNN.dkgf.  A unit builds on base.dk and the units before
it.  A declaration is attached to an item of the source by a comment

    (; //TAG:<item id or label> ;)

on the line above it, TAG being the "tag" of experiment.json.
"""
import json, os, re, sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE) if os.path.basename(HERE) == 'scripts' else HERE


def config():
    path = os.path.join(os.getcwd(), 'experiment.json')
    if not os.path.exists(path):
        sys.exit('no experiment.json here; see README.md')
    c = json.load(open(path))
    c.setdefault('name', os.path.splitext(os.path.basename(c['source']))[0])
    c.setdefault('title', c['name'])
    c.setdefault('tag', 'DOC')
    c.setdefault('eta', False)
    return c


def units():
    """The unit numbers that have a secNN.dk, in order."""
    return sorted(int(f[3:-3]) for f in os.listdir('.') if re.fullmatch(r'sec\d+\.dk', f))


def dk_file(n):
    return 'sec%02d.dk' % n


def table_file(n):
    return 'sec%02d.dkgf' % n


def tables():
    """The symbol tables in the order that fixes the macro names."""
    return (['base.dkgf'] if os.path.exists('base.dkgf') else []) + \
        [table_file(n) for n in units() if os.path.exists(table_file(n))]


def tag_re(c):
    return re.compile(r'\(; //%s:([^;]*?) ;\)' % re.escape(c['tag']))


# A declaration starts a line with its name and a colon, or "def name :"; a
# rewrite rule starts with "[".  Blocks end at the next such start.
BLOCK = re.compile(r'\n(?=(?:def |thm )?[A-Za-z_0-9]+ :|\[)')
DECL = re.compile(r'(?:def |thm )?([A-Za-z_0-9]+) :')
RULE = re.compile(r'\[[^\]]*\]\s*([A-Za-z_0-9]+)')


def rule_names(blocks):
    """The name of each rewrite rule block, as in "fib rule 2"; None for others."""
    count, out = {}, []
    for b in blocks:
        m = RULE.match(b)
        if m:
            count[m.group(1)] = count.get(m.group(1), 0) + 1
            out.append('%s rule %d' % (m.group(1), count[m.group(1)]))
        else:
            out.append(None)
    return out


def blocks_of(text):
    """(name, block) for each declaration and rewrite rule, in file order."""
    bs = BLOCK.split(text)
    out = []
    for b, rule in zip(bs, rule_names(bs)):
        if rule:
            out.append((rule, b))
        else:
            m = DECL.match(b)
            if m:
                out.append((m.group(1), b))
    return out


def is_theorem(block):
    """A declaration whose type is a Proof: a theorem, with or without proof."""
    head = block.split(':=')[0]
    return bool(re.search(r'\bProof\b', head.split('->')[-1] if '->' in head else head))


def tags(path, c):
    """constant -> the item id or label its declaration is tagged with."""
    out, tag = {}, None
    tre = tag_re(c)
    for line in open(path):
        m = tre.search(line)
        if m:
            tag = m.group(1).strip()
            continue
        m = DECL.match(line)
        if m and tag is not None:
            out[m.group(1)] = tag
            tag = None
    return out


def need_informath():
    if not os.environ.get('INFORMATH_ROOT'):
        sys.exit('INFORMATH_ROOT is not set: export INFORMATH_ROOT=<your informath checkout>')
