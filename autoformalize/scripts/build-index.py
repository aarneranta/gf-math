#!/usr/bin/env python3
"""Build <name>-index.html: the items of the source document, each with its
Dedukti formalization and Informath's readings of it.

    python3 scripts/extract-items.py        # -> items.json
    python3 scripts/build-informal.py       # -> secNN-informal.tsv
    python3 scripts/build-informal.py -symbolic
    python3 scripts/build-informal.py -closest
    python3 scripts/build-index.py          # -> <name>-index.html

A declaration is shown with an item when the comment (; //TAG:<id> ;) above
it names the item by its number (2.3) or its label (thm:main); a section
number (2, or 2.1) attaches it to the running text of that section.  The
source's text is rendered by MathJax, with the document's macros
(items-macros.json), those of the readings (informal-macros.tex) and any
given in experiment.json as "macros", which is where to define by hand the
ones that the document defines with LaTeX internals.
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


def forms_html(forms, english, symbolic, close, arities):
    parts = []
    for name, block, path in forms:
        e, s = english.get(name), symbolic.get(name)
        k = kind_of(name, block)
        parts.append(
            '<div class=form><code class=name>%s</code> <span class="k %s">%s</span> '
            '<span class=file>%s</span><pre>%s</pre>%s%s</div>' % (
                html.escape(name), k.replace(' ', '-'), k, path, html.escape(block),
                '<div class=reading><span class=lab>English</span> %s%s</div>' % (
                    reading_html(repair_macros(e, arities)),
                    ' <span class=close>closest to the source of %d readings (%.2f)</span>'
                    % (close[name][1], close[name][0]) if name in close and close[name][1] > 1 else '')
                if e else '',
                '<div class=reading><span class=lab>Formula</span> %s</div>'
                % reading_html(repair_macros(s, arities)) if s else ''))
    return ''.join(parts)


# ---------------------------------------------------------------- the page

HEAD = """<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>{TITLE} &mdash; formalized</title>
<script>
window.MathJax = {
  tex: { inlineMath: [['$','$'],['\\\\(','\\\\)']], displayMath: [['$$','$$'],['\\\\[','\\\\]']],
         macros: {MACROS} },
  options: { skipHtmlTags: ['script','noscript','style','textarea','pre','code'] }
};
</script>
<script async src="{MATHJAX}"></script>
<style>
 body{font-family:-apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif;margin:0 auto;max-width:1100px;padding:1.5rem;line-height:1.5;color:#222;background:#fff}
 header{border-bottom:1px solid #ddd;margin-bottom:1rem}
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
 .pdf{font-size:.8em;font-weight:400;margin-left:.6rem}
 table.cov{border-collapse:collapse;margin:.6rem 0;font-size:.9em}
 table.cov td,table.cov th{border:1px solid #ddd;padding:.2rem .6rem;text-align:right}
 table.cov td:first-child,table.cov th:first-child{text-align:left}
</style></head><body>
<header>
<h1>{TITLE}</h1>
<p>The numbered items of <code>{SOURCE}</code>, numbered as in the document, each with its
formalization in Dedukti (<code>base.dk</code>, <code>sec01.dk</code>, &hellip;) where there
is one, and Informath&rsquo;s readings of it: in <b>English</b> (the reading closest to the
source&rsquo;s text, where several were compared) and as a <b>formula</b> in standard logical
notation. A declaration is <span class="k defined">defined</span>,
<span class="k proved">proved</span> (its proof checked by Dedukti, and written out in the
proof documents), <span class="k stated">stated</span> without proof, or
<span class=k>primitive</span>.</p>
{COVERAGE}
</header>
"""


def main():
    c = config()
    data = json.load(open('items.json'))
    items, titles, top = data['items'], data['titles'], data['top']
    forms = formalizations(c)
    english, symbolic, close = readings('informal'), readings('symbolic'), closeness()
    ms = macros(c)
    arities = {k: (v[1] if isinstance(v, list) else 0) for k, v in ms.items()}
    used, body, cov = set(), [], {}
    unit, section = None, None
    for it in items:
        if it['unit'] != unit:
            unit = it['unit']
            pdf = 'proofs/sec%02d.pdf' % unit
            body.append('<h2 id="u%d">%s %d. %s%s</h2>' % (
                unit, top.capitalize(), unit, html.escape(titles.get(str(unit), '')),
                ' <a class=pdf href="%s">proofs (PDF)</a>' % pdf if os.path.exists(pdf) else ''))
            if str(unit) in forms:
                used.add(str(unit))
                body.append('<div class=entry><span class=kind>In the text of %s %d</span>%s</div>'
                            % (top, unit, forms_html(forms[str(unit)], english, symbolic, close, arities)))
        if it['section'] != section and it['section'] != str(unit):
            section = it['section']
            body.append('<h3>%s %s</h3>' % (section, html.escape(titles.get(section, ''))))
            if section in forms:
                used.add(section)
                body.append('<div class=entry><span class=kind>In the text</span>%s</div>'
                            % forms_html(forms[section], english, symbolic, close, arities))
        f = forms.get(it['id'], []) + (forms.get(it['label'], []) if it['label'] else [])
        used.update(k for k in (it['id'], it['label']) if k in forms)
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
        body.append('<div class=entry>%s<div class=txt>%s</div>%s%s</div>' % (
            head, tex_html(it['tex']), att,
            '<details open><summary>Dedukti and Informath</summary>%s</details>'
            % forms_html(f, english, symbolic, close, arities) if f else ''))
    unused = sorted(set(forms) - used)
    if unused:
        body.append('<h2>Tags that match no item</h2><p>%s</p>' % ', '.join(map(html.escape, unused)))
    rows = ''.join('<tr><td>%s %d %s</td><td>%d / %d</td></tr>'
                   % (top, u, html.escape(titles.get(str(u), '')), cv[1], cv[0])
                   for u, cv in sorted(cov.items()))
    tot = [sum(cv[i] for cv in cov.values()) for i in range(2)]
    coverage = ('<table class=cov><tr><th>%s</th><th>items formalized</th></tr>%s'
                '<tr><th>all</th><th>%d / %d</th></tr></table>' % (top, rows, tot[1], tot[0]))
    out = '%s-index.html' % c['name']
    page = (HEAD.replace('{TITLE}', html.escape(c['title'])).replace('{SOURCE}', html.escape(c['source']))
            .replace('{MATHJAX}', c.get('mathjax', MATHJAX))
            .replace('{MACROS}', json.dumps(ms)).replace('{COVERAGE}', coverage))
    open(out, 'w').write(page + '\n'.join(body) + '\n</body></html>\n')
    print('%d items, %d formalized -> %s' % (tot[0], tot[1], out))
    if unused:
        print('tags that match no item: ' + ', '.join(unused))


if __name__ == '__main__':
    main()
