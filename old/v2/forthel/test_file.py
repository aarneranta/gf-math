import pgf
import json
import sys

usage = "stdin | python3 test_tile.py pgf source index?"

if sys.argv[2:]:
    PGF_FILE = sys.argv[1]
    CNC_NAME = sys.argv[2]
else:
    print(usage)

grammar = pgf.readPGF(PGF_FILE)
source = grammar.languages[CNC_NAME]

targets = [(lang, cnc) for
             (lang, cnc) in grammar.languages.items() if lang != CNC_NAME]

indexfile = sys.argv[3] if sys.argv[3:] else None

if indexfile:
    with open(indexfile) as file:
        termindex = json.load(file)
else:
    termindex = None

        
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
    

sent = 1
fails = 0
successes = 0
for s in sys.stdin:
    if s.strip():
        try:
            p = source.parse(s)
            print(apply_index(termindex, s))
            print()
            for _, t in p:
                print('-- TREE', t)
                if targets:
                    for (lang, cnc) in targets:
                        print(apply_index(termindex, cnc.linearize(t)))
                        print()
                    break  ## to get just one parse
            successes += 1
        except pgf.ParseError as pe:
            print('-- NOTREE', sent, s)
            print('-- NOTREE CAUSE', pe)
            fails += 1
        sent += 1
        print()

print('success', successes, 'failure', fails)

