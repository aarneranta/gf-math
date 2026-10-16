#!/usr/bin/env python3
"""Extract the numbered items of the source document.

    python3 scripts/extract-items.py          # -> items.json

The source is the "source" of experiment.json, with its \\input and \\include
files expanded.  An item is an environment declared with \\newtheorem
(definition, theorem, lemma, ...), numbered as LaTeX would number it: the
counters shared as the \\newtheorem lines say, and reset by the section or
chapter they are numbered within.  A starred environment gets no number.  A
proof that follows an item is attached to it.  Each item records its unit
(the number of its chapter, or its section if there are no chapters), so
that it goes with secNN.dk, and its \\label, by which a declaration can be
tagged as well as by its number.

The running text of each section, outside its items, is recorded too, as
the text of a declaration tagged with the section's number.

Also writes items-macros.json: the document's own \\newcommand,
\\renewcommand and \\DeclareMathOperator macros, for MathJax.
"""
import json, os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import config

LEVELS = ['part', 'chapter', 'section', 'subsection', 'subsubsection']


def strip_comments(s):
    return re.sub(r'(?<!\\)%.*', '', s)


def expand(path, seen=()):
    """The file with its \\input{f} and \\include{f} replaced by their text."""
    s = strip_comments(open(path).read())
    d = os.path.dirname(path)

    def one(m):
        f = m.group(2).strip()
        for cand in (f, f + '.tex'):
            p = os.path.join(d, cand)
            if os.path.exists(p) and p not in seen:
                return expand(p, seen + (p,))
        return ''
    return re.sub(r'\\(input|include)\{([^}]*)\}', one, s)


def brace(s, i):
    """The {...} group at s[i]: (content, end)."""
    depth, j = 0, i
    while j < len(s):
        if s[j] == '\\':
            j += 2
            continue
        if s[j] == '{':
            depth += 1
        elif s[j] == '}':
            depth -= 1
            if depth == 0:
                return s[i + 1:j], j + 1
        j += 1
    return s[i + 1:], len(s)


def theorem_envs(s):
    """env -> (Name, counter, within): from \\newtheorem{env}[shared]{Name}[within]."""
    envs = {}
    for m in re.finditer(r'\\newtheorem(\*?)\{([^}]*)\}(?:\[([^\]]*)\])?\{([^}]*)\}(?:\[([^\]]*)\])?', s):
        star, env, shared, name, within = m.groups()
        if star:
            envs[env] = (name, None, None)
        elif shared:
            envs[env] = (name, shared, envs.get(shared, (None, shared, None))[2])
        else:
            envs[env] = (name, env, within)
    return envs


def env_end(s, name, start):
    depth, i = 1, start
    pat = re.compile(r'\\(begin|end)\{%s\}' % re.escape(name))
    while depth:
        m = pat.search(s, i)
        if not m:
            return len(s)
        depth += 1 if m.group(1) == 'begin' else -1
        i = m.end()
    return i


def clean(tex):
    tex = re.sub(r'\\index\{(?:[^{}]|\{[^{}]*\})*\}', '', tex)
    tex = re.sub(r'\\label\{[^}]*\}', '', tex)
    return re.sub(r'[ \t]*\n[ \t]*\n\s*', '\n\n', tex).strip()


def macros(s):
    """name -> MathJax macro definition, for the macros MathJax can take."""
    out = {}
    for m in re.finditer(r'\\(?:re|provide)?newcommand\*?\s*\{?\\([A-Za-z]+)\}?', s):
        name, i = m.group(1), m.end()
        arity, default = 0, None
        a = re.match(r'\s*\[(\d+)\]', s[i:])
        if a:
            arity, i = int(a.group(1)), i + a.end()
            d = re.match(r'\s*\[([^\]]*)\]', s[i:])
            if d:
                default, i = d.group(1), i + d.end()
        while i < len(s) and s[i] in ' \t\n':
            i += 1
        if i >= len(s) or s[i] != '{':
            continue
        body, _ = brace(s, i)
        if '@' in body or '\\ifnextchar' in body:
            continue   # LaTeX internals: define it by hand in experiment.json
        body = body.replace('\\xspace', '').replace('\\ensuremath', '')
        out[name] = body if not arity else ([body, arity, default] if default is not None else [body, arity])
    for m in re.finditer(r'\\DeclareMathOperator(\*?)\{\\([A-Za-z]+)\}\{([^}]*)\}', s):
        out[m.group(2)] = ('\\operatorname*{%s}' if m.group(1) else '\\operatorname{%s}') % m.group(3)
    return out


def main():
    c = config()
    s = expand(c['source'])
    envs = theorem_envs(s)
    if not envs:
        sys.exit('no \\newtheorem environments in %s' % c['source'])
    body = s[s.find('\\begin{document}'):] if '\\begin{document}' in s else s
    has_chapters = '\\chapter' in body
    top = 'chapter' if has_chapters else 'section'
    level = {l: 0 for l in LEVELS}
    counters, titles, running = {}, {}, {}
    items, starred = [], {}
    key = ''      # the sectioning unit the running text belongs to
    tok = re.compile(r'\\(part|chapter|section|subsection|subsubsection)(\*?)\s*(?:\[[^\]]*\])?\{'
                     r'|\\begin\{(%s)\}(\[[^\]]*\])?|\\begin\{proof\}(\[[^\]]*\])?'
                     % '|'.join(map(re.escape, envs)))
    pos = 0
    while True:
        m = tok.search(body, pos)
        if key and m:
            running[key] = running.get(key, '') + body[pos:m.start()]
        if not m:
            if key:
                running[key] = running.get(key, '') + body[pos:]
            break
        if m.group(1):
            title, pos = brace(body, m.end() - 1)
            if m.group(2):     # \section*: not numbered
                continue
            lv = m.group(1)
            level[lv] += 1
            for l in LEVELS[LEVELS.index(lv) + 1:]:
                level[l] = 0
            for cname, (val, within) in list(counters.items()):
                if within and LEVELS.index(within) >= LEVELS.index(lv):
                    counters[cname] = (0, within)
            if lv in (top, 'section', 'subsection'):
                key = number(level, lv, has_chapters)
                titles[key] = title
            continue
        if m.group(5) is not None or body[m.start():m.start() + 12] == '\\begin{proof':
            end = env_end(body, 'proof', m.end())
            text = body[m.end():end - len('\\end{proof}')]
            if items:
                items[-1].setdefault('attached', []).append(dict(kind='Proof', tex=clean(text)))
            pos = end
            continue
        env = m.group(3)
        end = env_end(body, env, m.end())
        text = body[m.end():end - len('\\end{%s}' % env)]
        pos = end
        name, counter, within = envs[env]
        unit = level[top]
        if counter is None:
            starred[unit] = starred.get(unit, 0) + 1
            iid = '%d*%d' % (unit, starred[unit])
        else:
            val, w = counters.get(counter, (0, within))
            counters[counter] = (val + 1, w)
            prefix = number(level, w, has_chapters) if w else ''
            iid = (prefix + '.' if prefix else '') + str(val + 1)
        label = re.search(r'\\label\{([^}]*)\}', text)
        items.append(dict(id=iid, kind=name, unit=unit,
                          section=number(level, 'section', has_chapters),
                          label=label.group(1) if label else '',
                          title=(m.group(4) or '')[1:-1], tex=clean(text)))
    texts = {k: clean(re.sub(r'\\end\{document\}', '', v)) for k, v in running.items()}
    json.dump(dict(items=items, titles=titles, texts={k: v for k, v in texts.items() if v},
                   top=top), open('items.json', 'w'),
              indent=1, ensure_ascii=False)
    json.dump(macros(s), open('items-macros.json', 'w'), indent=1, ensure_ascii=False)
    kinds = {}
    for it in items:
        kinds[it['kind']] = kinds.get(it['kind'], 0) + 1
    print('%d items in %d %ss -> items.json %s' % (
        len(items), len({it['unit'] for it in items}), top, kinds))


def number(level, lv, has_chapters):
    """The number of the current sectioning unit lv, as LaTeX prints it: 2.3."""
    order = LEVELS[1:] if has_chapters else LEVELS[2:]
    if lv not in order:
        return ''
    return '.'.join(str(level[l]) for l in order[:order.index(lv) + 1])


if __name__ == '__main__':
    main()
