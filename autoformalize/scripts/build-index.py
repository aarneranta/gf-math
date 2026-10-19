#!/usr/bin/env python3
"""Build the index of the experiment: <name>-index.html, the table of
contents, and pages/secNN.html, one page per unit (chapter or section) with
the items of the source document, each with its Dedukti formalization,
Informath's readings of it, and the proofs in English.

    python3 scripts/extract-items.py        # -> items.json
    python3 scripts/build-informal.py       # -> secNN-informal.tsv
    python3 scripts/build-informal.py -symbolic
    python3 scripts/build-informal.py -closest
    scripts/build-proofs.sh                 # -> proofs/secNN.tex
    python3 scripts/build-index.py          # -> <name>-index.html, pages/secNN.html

A declaration is shown with an item when the comment (; //TAG:<id> ;) above
it names the item by its number (2.3) or its label (thm:main); a section
number (2, or 2.1) attaches it to the running text of that section.  A
proved declaration is shown with its proof as numbered lines of English,
from proofs/secNN.tex, whose tables are paired in order with the proved
declarations of secNN.dk.  If there is a CoNLL-U parse of the source
("conllu" of experiment.json, by default <name>.conllu), each item also gets
the dependency trees of the parsed sentences that occur in its text.

<name>-index.html lists the units with how many of their items are
formalized, and sends an old link <name>-index.html#2.3 on to the page of
item 2.3.  The source's text is rendered by MathJax, with the document's
macros (items-macros.json), those of the readings (informal-macros.tex) and
any given in experiment.json as "macros", which is where to define by hand
the ones that the document defines with LaTeX internals.
"""
import html, json, os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import config, units, dk_file, blocks_of, is_theorem, tag_re

MATHJAX = 'https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml-full.js'


# ---------------------------------------------------------------- the formalization

def formalizations(c):
    """tag -> [(name, Dedukti block, file)] for every tagged declaration."""
    out = {}
    tre = tag_re(c)
    for path in ['base.dk'] + [dk_file(n) for n in units()]:
        text = open(path).read()
        lines = text.split('\n')
        rules = {}
        i = 0
        while i < len(lines):
            m = tre.search(lines[i])
            if not m:
                i += 1
                continue
            tag = m.group(1).strip()
            j, block = i + 1, []
            while j < len(lines) and lines[j].strip():   # up to the next blank line
                if not tre.search(lines[j]) and not re.match(r'\s*\(;.*;\)\s*$', lines[j]):
                    block.append(lines[j])
                j += 1
            b = '\n'.join(block)
            got = blocks_of(b) if b else []
            for name, blk in got:
                if ' rule ' in name:   # numbered within the file, as build-informal.py does
                    head = name.split(' rule ')[0]
                    rules[head] = rules.get(head, 0) + 1
                    name = '%s rule %d' % (head, rules[head])
                out.setdefault(tag, []).append((name, blk.strip(), path))
            i = j
    return out


def readings(kind):
    out = {}
    for n in units():
        path = 'sec%02d-%s.tsv' % (n, kind)
        if os.path.exists(path):
            for line in open(path):
                if '\t' in line:
                    k, t = line.rstrip('\n').split('\t', 1)
                    out[k] = t
    return out


def closeness():
    """name -> (similarity to the source, number of readings compared)."""
    out = {}
    for n in units():
        path = 'sec%02d-closest.tsv' % n
        if os.path.exists(path):
            for line in open(path):
                f = line.rstrip('\n').split('\t')
                if len(f) == 4:
                    out[f[0]] = (float(f[1]), int(f[3]))
    return out


def proof_texts():
    """name -> [(number, sentence, justification)]: the proof of each proved
    declaration as numbered lines of English, from proofs/secNN.tex.  The
    document has one table per proof and no names, in the order of the
    proved declarations of secNN.dk (as RunInformath -proof-text writes
    them); a unit whose numbers do not agree is left out, with a warning."""
    out = {}
    for n in units():
        tex = 'proofs/sec%02d.tex' % n
        if not os.path.exists(tex):
            continue
        names = [name for name, b in blocks_of(open(dk_file(n)).read())
                 if kind_of(name, b) == 'proved']
        tables = re.findall(r'\\begin\{tabular\}\{[^\n]*\}\n(.*?)\\end\{tabular\}',
                            open(tex).read(), re.S)
        if len(tables) != len(names):
            print('%s: %d proofs for %d proved declarations of %s; proofs not shown'
                  % (tex, len(tables), len(names), dk_file(n)))
            continue
        for name, table in zip(names, tables):
            rows = []
            for row in re.split(r'\\\\\s*\n', table):
                # number & sentence & justification (empty for a hypothesis)
                m = re.match(r'\s*(\d+\.)\s*&(.*)&(.*)$', row, re.S)
                if m:
                    rows.append(tuple(g.strip() for g in m.groups()))
            out[name] = rows
    return out


def kind_of(name, block):
    if ' rule ' in name:
        return 'rewrite rule'
    theorem = is_theorem(block)
    defined = ':=' in block or block.startswith('def ')
    if theorem:
        return 'proved' if defined else 'stated'
    return 'defined' if defined else 'primitive'


# ---------------------------------------------------------------- macros

def macros(c):
    out = {}
    if os.path.exists('items-macros.json'):
        out.update(json.load(open('items-macros.json')))
    if os.path.exists('informal-macros.tex'):
        for m in re.finditer(r'\\newcommand\{\\(\w+)\}(?:\[(\d+)\])?\{(.*)\}\s*$',
                             open('informal-macros.tex').read(), re.M):
            out[m.group(1)] = [m.group(3), int(m.group(2))] if m.group(2) else m.group(3)
    out.update(c.get('macros', {}))
    return out


def brace_arg(s, i):
    depth, j = 0, i
    while j < len(s):
        if s[j] == '{':
            depth += 1
        elif s[j] == '}':
            depth -= 1
            if depth == 0:
                return s[i + 1:j], j + 1
        elif s[j] == '\\':
            j += 1
        j += 1
    return s[i + 1:], len(s)


def repair_macros(text, arities):
    """A generated macro applied to fewer arguments than it takes (a constant
    used unapplied) cannot be typeset: show the constant's name instead."""
    def one(m):
        name, rest = m.group(1), text[m.end():]
        n, i = 0, 0
        while n < arities.get(name, 0):
            j = i
            while j < len(rest) and rest[j] == ' ':
                j += 1
            if j < len(rest) and rest[j] == '{':
                _, i = brace_arg(rest, j)
                n += 1
            else:
                break
        if n < arities.get(name, 0):
            return '\\mathsf{%s}' % re.sub(r'MACRo[A-Z]+$', '', name)
        return m.group(0)
    return re.sub(r'\\([A-Za-z]+MACRo[A-Z]+)', one, text)


# ---------------------------------------------------------------- TeX to HTML

MATH = re.compile(r'(\$\$.*?\$\$|\$.*?\$|\\\[.*?\\\]|\\\(.*?\\\)|\\begin\{(equation\*?|align\*?|alignat\*?|gather\*?|multline\*?|tikzcd|prooftree)\}.*?\\end\{\2\})', re.S)


def math_html(m):
    s, env = m.group(0), m.group(2)
    if env in ('tikzcd', 'prooftree') or (env and env.startswith('alignat')):
        return '<span class=no>[%s, not shown]</span>' % ('diagram' if env == 'tikzcd' else 'display')
    if env and env.startswith('equation'):
        s = '$$' + re.sub(r'\\(begin|end)\{equation\*?\}', '', s) + '$$'
    elif env:
        s = '$$' + re.sub(r'\\(begin|end)\{(align|gather|multline)\*?\}', r'\\\1{aligned}', s) + '$$'
    s = re.sub(r'\\label\{[^}]*\}', '', s)
    return html.escape(s, quote=False)


def text_html(t):
    t = re.sub(r'\\(emph|textit)\{([^{}]*)\}', r'<em>\2</em>', t)
    t = re.sub(r'\\(textbf)\{([^{}]*)\}', r'<b>\2</b>', t)
    t = re.sub(r'\\(?:[cC]ref|ref|eqref)\{([^}]*)\}', r'<span class=ref>[\1]</span>', t)
    t = re.sub(r'\\cite(?:\[[^\]]*\])?\{([^}]*)\}', r'<span class=ref>[\1]</span>', t)
    t = re.sub(r'\\footnote\{[^{}]*\}', '', t)
    t = re.sub(r'\\begin\{enumerate\}(\[[^\]]*\])?', '<ol>', t).replace('\\end{enumerate}', '</ol>')
    t = t.replace('\\begin{itemize}', '<ul>').replace('\\end{itemize}', '</ul>')
    t = re.sub(r'\\item\b(\[[^\]]*\])?', '<li>', t)
    t = re.sub(r'\\(begin|end)\{(center|small|footnotesize|flushleft|samepage)\}', '', t)
    t = re.sub(r'\\(vspace|hspace)\*?\{[^}]*\}|\\(medskip|bigskip|smallskip|noindent|par)\b', '', t)
    t = t.replace('~', ' ').replace('---', '&mdash;').replace('--', '&ndash;')
    t = t.replace('``', '&ldquo;').replace("''", '&rdquo;').replace('\\ldots', '&hellip;')
    return re.sub(r'\n\s*\n', '<br><br>', t)


def tex_html(tex):
    out, pos = [], 0
    for m in MATH.finditer(tex):
        out.append(text_html(html.escape(tex[pos:m.start()], quote=False)))
        out.append(math_html(m))
        pos = m.end()
    out.append(text_html(html.escape(tex[pos:], quote=False)))
    return ''.join(out)


def reading_html(text):
    t = html.escape(text, quote=False)
    t = t.replace('\\begin{itemize}', '<ul>').replace('\\end{itemize}', '</ul>')
    t = re.sub(r'\\item\b\s*', '<li>', t)
    return re.sub(r'\\textbf\{([^{}$]*)\}', r'<b>\1</b>', t)


def justification_html(j):
    j = html.escape(j.replace('\\_', '_'), quote=False)
    return re.sub(r'\\texttt\{([^{}]*)\}', r'<code>\1</code>', j)


def proof_html(rows, arities):
    body = ''.join('<tr><td class=n>%s</td><td>%s</td><td class=j>%s</td></tr>'
                   % (html.escape(n), reading_html(repair_macros(t, arities)), justification_html(j))
                   for n, t, j in rows)
    return ('<div class=reading><details%s><summary>Proof in English (%d lines)</summary>'
            '<table class=proof>%s</table></details></div>'
            % (' open' if len(rows) <= 12 else '', len(rows), body))


def forms_html(forms, english, symbolic, close, arities, proofs={}):
    parts = []
    for name, block, path in forms:
        e, s = english.get(name), symbolic.get(name)
        k = kind_of(name, block)
        parts.append(
            '<div class=form><code class=name>%s</code> <span class="k %s">%s</span> '
            '<span class=file>%s</span><pre>%s</pre>%s%s%s</div>' % (
                html.escape(name), k.replace(' ', '-'), k, path, html.escape(block),
                '<div class=reading><span class=lab>English</span> %s%s</div>' % (
                    reading_html(repair_macros(e, arities)),
                    ' <span class=close>closest to the source of %d readings (%.2f)</span>'
                    % (close[name][1], close[name][0]) if name in close and close[name][1] > 1 else '')
                if e else '',
                '<div class=reading><span class=lab>Formula</span> %s</div>'
                % reading_html(repair_macros(s, arities)) if s else '',
                proof_html(proofs[name], arities) if name in proofs else ''))
    return ''.join(parts)


# ---------------------------------------------------------------- UD parses

INDEX_CMD = re.compile(r'\\index\{(?:[^{}]|\{(?:[^{}]|\{[^{}]*\})*\})*\}')


def conllu_file(c):
    """The CoNLL-U parse of the source: "conllu" of experiment.json, or
    <name>.conllu if there is one; None if there is none."""
    path = c.get('conllu', c['name'] + '.conllu')
    return path if os.path.exists(path) else None


def norm(text):
    """Text as a word string for matching: formulas one token, TeX commands
    and index entries gone, letters only, lower case."""
    text = INDEX_CMD.sub(' ', text)
    text = re.sub(r'\$\$.*?\$\$|\$.*?\$|\\\[.*?\\\]', ' latexmath ', text, flags=re.S)
    text = re.sub(r'\\begin\{(equation\*?|align\*?|gather\*?|prooftree|tikzcd|multline\*?)\}.*?\\end\{\1\}',
                  ' latexmath ', text, flags=re.S)
    text = re.sub(r'\\[A-Za-z]+\*?', ' ', text)
    return ' '.join(re.findall(r'[a-z]+', text.lower()))


def ud_sentences(path):
    """[(sent_id, text, tokens)] of the parse, tokens as (id, form, head, deprel, upos)."""
    out, sid, text, toks = [], None, '', []
    for line in open(path):
        line = line.rstrip('\n')
        if line.startswith('# sent_id = '):
            sid = line.split('= ', 1)[1]
        elif line.startswith('# text = '):
            text = line[9:]
        elif line and not line.startswith('#'):
            f = line.split('\t')
            if f[0].isdigit():
                toks.append((int(f[0]), f[1], int(f[6]) if f[6].isdigit() else 0, f[7], f[3]))
        elif not line and toks:
            out.append((sid or str(len(out) + 1), text, toks))
            sid, text, toks = None, '', []
    if toks:
        out.append((sid or str(len(out) + 1), text, toks))
    return out


def match_sentences(items, sents):
    """item id -> the parsed sentences whose words occur, in order, in the
    item's text (or in what is attached to it, such as its proof)."""
    keyed = [(norm(t), sid, t, toks) for sid, t, toks in sents]
    keyed = [k for k in keyed if len(k[0].split()) >= 4]
    out = {}
    for it in items:
        body = norm(it['tex'] + ' ' + ' '.join(a['tex'] for a in it.get('attached', [])))
        found = sorted((body.find(k), sid, t, toks) for k, sid, t, toks in keyed if k in body)
        seen, uniq = set(), []
        for pos, sid, t, toks in found:
            if pos not in seen:
                seen.add(pos)
                uniq.append((sid, t, toks))
        if uniq:
            out[it['id']] = uniq
    return out


def tree_svg(toks):
    """A dependency tree: the words in a row, arcs above them from head to dependent."""
    xs, x = {}, 10
    for i, form, *_ in toks:
        w = max(len(form), 4) * 7 + 12
        xs[i] = x + w / 2
        x += w
    height = 30 + 18 * min(12, max((abs(i - h) for i, _, h, _, _ in toks if h), default=1))
    parts = ['<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" font-size="11" font-family="sans-serif">'
             % (x + 10, height + 40)]
    for i, form, head, rel, upos in toks:
        y = height + 14
        parts.append('<text x="%.0f" y="%d" text-anchor="middle">%s</text>' % (xs[i], y, html.escape(form)))
        parts.append('<text x="%.0f" y="%d" text-anchor="middle" fill="#888" font-size="9">%s</text>'
                     % (xs[i], y + 13, upos))
        if head and head in xs:
            x1, x2 = xs[head], xs[i]
            top = height - 18 * min(12, abs(head - i))
            parts.append('<path d="M%.0f %d C%.0f %d %.0f %d %.0f %d" fill="none" stroke="#0645ad" stroke-width="1"/>'
                         % (x1, height, x1, top, x2, top, x2, height))
            parts.append('<text x="%.0f" y="%d" text-anchor="middle" fill="#0645ad" font-size="9">%s</text>'
                         % ((x1 + x2) / 2, top + 10, rel))
        elif not head:
            parts.append('<text x="%.0f" y="%d" text-anchor="middle" fill="#b00" font-size="9">root</text>'
                         % (xs[i], height - 4))
    parts.append('</svg>')
    return ''.join(parts)


def ud_html(matched):
    if not matched:
        return ''
    inner = ''.join('<div class=ud><span class=sid>%s</span> %s<div class=tree>%s</div></div>'
                    % (html.escape(sid), html.escape(t), tree_svg(toks)) for sid, t, toks in matched)
    return '<details><summary>UD parse (%d sentence%s)</summary>%s</details>' % (
        len(matched), '' if len(matched) == 1 else 's', inner)


# ---------------------------------------------------------------- the pages

PAGES = 'pages'

STYLE = """<style>
 body{font-family:-apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif;margin:0 auto;max-width:1100px;padding:1.5rem;line-height:1.5;color:#222;background:#fff}
 header{border-bottom:1px solid #ddd;margin-bottom:1rem}
 nav{font-size:.9em;margin:.3rem 0}
 nav a{margin-right:1rem}
 a{color:#0645ad}
 h2{margin-top:2.2rem;border-bottom:2px solid #0645ad;padding-bottom:.2rem}
 h3{margin:1.6rem 0 .4rem;color:#444;font-size:1.05rem}
 .entry{padding:.7rem 0;border-bottom:1px solid #eee}
 .id{font-weight:700;color:#0645ad;font-variant-numeric:tabular-nums}
 .kind{font-weight:700}
 .none{color:#b00;font-size:.8em;margin-left:.5rem}
 .done{color:#070;font-size:.8em;margin-left:.5rem}
 .txt{margin:.35rem 0}
 details{margin:.3rem 0}
 summary{cursor:pointer;color:#0645ad;font-size:.9em;user-select:none}
 pre{background:#f6f8fa;border:1px solid #e1e4e8;border-radius:6px;padding:.6rem;overflow:auto;font-size:.8em;white-space:pre-wrap;margin:.3rem 0}
 .form{margin:.5rem 0 .8rem}
 .name{font-weight:700}
 .file{color:#888;font-size:.8em}
 .reading{font-size:.92em;margin:.2rem 0 .2rem 1rem}
 .lab{display:inline-block;min-width:5.5rem;color:#666;font-size:.8em;text-transform:uppercase;letter-spacing:.03em}
 .ref{color:#666;font-size:.85em}
 .close{color:#999;font-size:.75em}
 .no{color:#999}
 .k{font-size:.72em;padding:0 .35rem;border-radius:3px;background:#eef;color:#335}
 .k.stated{background:#fde8e8;color:#922}
 .k.proved,.k.defined,.k.rewrite-rule{background:#e6f4ea;color:#1e6b34}
 table.proof{border-collapse:collapse;font-size:.95em;margin:.3rem 0}
 table.proof td{padding:.1rem .5rem;vertical-align:top;border-bottom:1px solid #f0f0f0}
 table.proof td.n{color:#888;text-align:right;white-space:nowrap}
 table.proof td.j{color:#555;white-space:nowrap;font-size:.9em}
 .ud{margin:.4rem 0;font-size:.85em}
 .sid{color:#888;font-variant-numeric:tabular-nums}
 .tree{overflow-x:auto}
 table.toc{border-collapse:collapse;margin:.6rem 0 1.5rem;font-size:.95em}
 table.toc td,table.toc th{padding:.2rem .7rem;border-bottom:1px solid #eee;text-align:right;font-variant-numeric:tabular-nums}
 table.toc td.t,table.toc th.t{text-align:left}
 table.toc tr.all th{border-top:2px solid #ddd}
</style>"""

MATHJAX_HEAD = """<script>
window.MathJax = {
  tex: { inlineMath: [['$','$'],['\\\\(','\\\\)']], displayMath: [['$$','$$'],['\\\\[','\\\\]']],
         macros: {MACROS} },
  options: { skipHtmlTags: ['script','noscript','style','textarea','pre','code'] }
};
</script>
<script async src="{MATHJAX}"></script>"""

INTRO = """The numbered items of <code>{SOURCE}</code>, numbered as in the document, each with its
formalization in Dedukti (<code>base.dk</code>, <code>sec01.dk</code>, &hellip;) where there
is one, and Informath&rsquo;s readings of it: in <b>English</b> (the reading closest to the
source&rsquo;s text, where several were compared) and as a <b>formula</b> in standard logical
notation{UD}. A declaration is <span class="k defined">defined</span>,
<span class="k proved">proved</span> (its proof checked by Dedukti, and shown in English as
numbered lines, each with the rule and the premisses that justify it),
<span class="k stated">stated</span> without proof, or <span class=k>primitive</span>."""


def page_head(title, scripts=''):
    return ('<!DOCTYPE html>\n<html lang="en"><head><meta charset="utf-8">\n'
            '<meta name="viewport" content="width=device-width, initial-scale=1">\n'
            '<title>%s</title>\n%s\n%s</head><body>\n' % (html.escape(title), scripts, STYLE))


def unit_page(c, top, unit, title, prev, nxt, body, macros_json, has_ud):
    def link(u):
        return '<a href="sec%02d.html">%s %d. %s</a>' % (u[0], top.capitalize(), u[0], html.escape(u[1]))
    nav = ['<a href="../%s-index.html">Contents</a>' % c['name']]
    if prev:
        nav.append('&larr; ' + link(prev))
    if nxt:
        nav.append(link(nxt) + ' &rarr;')
    mathjax = c.get('mathjax', MATHJAX)
    if '://' not in mathjax:
        mathjax = '../' + mathjax      # a local copy, relative to the experiment
    return '\n'.join([
        page_head('%s: %s %d. %s' % (c['title'], top.capitalize(), unit, title),
                  MATHJAX_HEAD.replace('{MACROS}', macros_json).replace('{MATHJAX}', mathjax)),
        '<header><nav>%s</nav>' % ' '.join(nav),
        '<h1 id="u%d">%s %d. %s</h1>' % (unit, top.capitalize(), unit, html.escape(title)),
        '<p>%s</p></header>' % INTRO.replace('{SOURCE}', html.escape(c['source']))
        .replace('{UD}', ', with the UD parse of its sentences' if has_ud else ''),
        '\n'.join(body), '<nav>%s</nav>' % ' '.join(nav), '</body></html>\n'])


def contents_page(c, top, titles, cov, unused, anchors, has_ud):
    rows = ''.join('<tr><td>%s %d</td><td class=t><a href="%s/sec%02d.html">%s</a></td><td>%d / %d</td></tr>'
                   % (top.capitalize(), u, PAGES, u, html.escape(titles.get(str(u), '')), cv[1], cv[0])
                   for u, cv in sorted(cov.items()))
    tot = [sum(cv[i] for cv in cov.values()) for i in range(2)]
    # an old link <name>-index.html#2.3 (an item) or #u2 (a unit) goes on to its page
    redirect = ('<script>\n(function () {\n  var h = decodeURIComponent(location.hash.slice(1)), pages = %s;\n'
                '  if (h in pages) location.replace(pages[h] + location.hash);\n})();\n</script>'
                % json.dumps(anchors))
    return '\n'.join([
        page_head(c['title'], redirect),
        '<header><h1>%s</h1>' % html.escape(c['title']),
        '<p>%s Each %s has a page of its own.</p></header>'
        % (INTRO.replace('{SOURCE}', html.escape(c['source']))
           .replace('{UD}', ', with the UD parse of its sentences' if has_ud else ''), top),
        '<table class=toc><tr><th></th><th class=t>%s</th><th>items formalized</th></tr>' % top,
        rows,
        '<tr class=all><th></th><th class=t>all</th><th>%d / %d</th></tr></table>' % (tot[1], tot[0]),
        ('<h2>Tags that match no item</h2><p>%s</p>' % ', '.join(map(html.escape, unused))) if unused else '',
        '</body></html>\n'])


def main():
    c = config()
    data = json.load(open('items.json'))
    items, titles, top = data['items'], data['titles'], data['top']
    forms = formalizations(c)
    english, symbolic, close = readings('informal'), readings('symbolic'), closeness()
    proofs = proof_texts()
    ms = macros(c)
    arities = {k: (v[1] if isinstance(v, list) else 0) for k, v in ms.items()}
    conllu = conllu_file(c)
    ud = match_sentences(items, ud_sentences(conllu)) if conllu else {}
    used, bodies, cov, anchors = set(), {}, {}, {}
    section = None
    for it in items:
        unit = it['unit']
        page = '%s/sec%02d.html' % (PAGES, unit)
        if unit not in bodies:
            bodies[unit] = []
            anchors['u%d' % unit] = page
            if str(unit) in forms:
                used.add(str(unit))
                bodies[unit].append('<div class=entry><span class=kind>In the text of %s %d</span>%s</div>'
                                    % (top, unit, forms_html(forms[str(unit)], english, symbolic, close,
                                                              arities, proofs)))
        body = bodies[unit]
        anchors[it['id']] = page
        if it['section'] != section and it['section'] != str(unit):
            section = it['section']
            body.append('<h3>%s %s</h3>' % (section, html.escape(titles.get(section, ''))))
            if section in forms:
                used.add(section)
                body.append('<div class=entry><span class=kind>In the text</span>%s</div>'
                            % forms_html(forms[section], english, symbolic, close, arities, proofs))
        # an item numbered like a section (item 1.1 in section 1.1) is found by its label only
        ids = [k for k in (it['id'], it['label']) if k and k not in data['texts']]
        f = sum((forms.get(k, []) for k in ids), [])
        used.update(k for k in ids if k in forms)
        cv = cov.setdefault(unit, [0, 0])
        cv[0] += 1
        cv[1] += bool(f)
        title = ' (%s)' % html.escape(it['title']) if it['title'] else ''
        head = '<span class=id id="%s">%s</span> <span class=kind>%s%s</span>%s' % (
            html.escape(it['id']), html.escape(it['id'] if '*' not in it['id'] else ''),
            html.escape(it['kind']), title,
            '<span class=done>formalized</span>' if f else '<span class=none>not formalized</span>')
        att = ''.join('<details><summary>%s</summary><div class=txt>%s</div></details>'
                      % (a['kind'], tex_html(a['tex'])) for a in it.get('attached', []))
        body.append('<div class=entry>%s<div class=txt>%s</div>%s%s%s</div>' % (
            head, tex_html(it['tex']), att,
            '<details open><summary>Dedukti and Informath</summary>%s</details>'
            % forms_html(f, english, symbolic, close, arities, proofs) if f else '',
            ud_html(ud.get(it['id']))))
    unused = sorted(set(forms) - used)
    os.makedirs(PAGES, exist_ok=True)
    order = sorted(bodies)
    macros_json = json.dumps(ms)
    for i, u in enumerate(order):
        neighbour = lambda v: (v, titles.get(str(v), ''))
        open('%s/sec%02d.html' % (PAGES, u), 'w').write(unit_page(
            c, top, u, titles.get(str(u), ''), neighbour(order[i - 1]) if i else None,
            neighbour(order[i + 1]) if i + 1 < len(order) else None, bodies[u], macros_json, bool(conllu)))
    out = '%s-index.html' % c['name']
    open(out, 'w').write(contents_page(c, top, titles, cov, unused, anchors, bool(conllu)))
    tot = [sum(cv[i] for cv in cov.values()) for i in range(2)]
    print('%d items, %d formalized, %d proofs shown%s -> %s, %s/sec*.html'
          % (tot[0], tot[1], len(proofs), ', UD parses of %d items' % len(ud) if conllu else '',
             out, PAGES))
    if unused:
        print('tags that match no item: ' + ', '.join(unused))


if __name__ == '__main__':
    main()
