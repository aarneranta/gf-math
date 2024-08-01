import pgf
import json
import sys

usage = "stdin | python3 (-json|-latex|-index) translatex.py pgf source index?"

# quick and dirty
def unlex(s):
    ss = s.split()
    ss[0] = ss[0].capitalize()
    for i in range(1, len(ss)):
        if ss[i][-1] == '.' and ss[(i+1):]:
            ss[i+1] = ss[i+1].capitalize()
    r = ' '.join(ss)
    r = r.replace(' .', '.')
    r = r.replace(' ,', ',')
    return r
    

def apply_index(termindex, s):
    if not termindex:
        return s
    toks = []
    words = s.split()
    while words:
        w = words.pop(0)
        if w == '\\INDEXEDTERM':
            words.pop(0)  # {
            i = words.pop(0)
            toks.append(termindex[i])
            words.pop(0)  # }
        else:
            toks.append(w)
    return ' '.join(toks)
    

def translate(pgf_name, source_name, lines, termindex):
    grammar = pgf.readPGF(pgf_name)
    source = grammar.languages[source_name]
    targets = [(lang, cnc) for
                 (lang, cnc) in grammar.languages.items()
                   if lang != source_name]

    result = {
        'grammar': pgf_name,
        'source': source_name,
        'targets': [lang for lang, _ in targets],
        'failures': 0,
        'successes': 0,
        'texts': {}
        }

    sentnum = 1
    for s in lines:
        s = s.strip()

        if s:            
            ms = apply_index(termindex, s)
            result['texts'][sentnum] = {}
            result['texts'][sentnum]['parsed'] = s
            result['texts'][sentnum]['unindexed'] = ms
            result['texts'][sentnum]['results'] = []

            try:
                p = source.parse(s)
                for _, t in p:
                    thisresult = {'tree': str(t)}
                    for (lang, cnc) in targets:
                        thisresult[lang] = apply_index(termindex, cnc.linearize(t))
                    result['texts'][sentnum]['results'].append(thisresult)
                    break  ## to get just one parse
                result['successes'] += 1
            except pgf.ParseError as pe:
                result['texts'][sentnum]['cause'] = str(pe)
                retuls['failures'] += 1
            sentnum += 1
    return result


def result2latex(result):
    latex = [r'\begin{document}']
    for sentnum, res in result['texts'].items():
        latex.append(r'\subsection*{' + str(sentnum) + '}')
        latex.append('')
        latex.append(unlex(res['unindexed']))
        latex.append('')
        for trans in res['results']:
            for key in trans:
                if key != 'tree':
                    latex.append(unlex(trans[key]))
                    latex.append('')
                    
    latex.append(r'\end{document}')
    return latex


if __name__ == '__main__':
    
    if sys.argv[3:]:
        MODE =  sys.argv[1]
        PGF_FILE = sys.argv[2]
        CNC_NAME = sys.argv[3]
    else:
        print(usage)
        exit()

    indexfile = sys.argv[4] if sys.argv[4:] else None
    if indexfile:
        with open(indexfile) as file:
            termindex = json.load(file)
    else:
        termindex = None

    if MODE == '-json':
        result = translate(PGF_FILE, CNC_NAME, sys.stdin, termindex)
        print(json.dumps(result, ensure_ascii=False, indent=2))
    if MODE == '-latex':
        result = translate(PGF_FILE, CNC_NAME, sys.stdin, termindex)
        for line in result2latex(result):
            print(line)


