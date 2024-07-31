import pgf
import json
import sys

# usage: stdin | python3 test_tile.py pgf? source? target? index?

PGF_FILE = 'ForthelDemo.pgf'
CNC_NAME = 'ForthelDemoEng'

if sys.argv[2:]:
    PGF_FILE = sys.argv[1]
    CNC_NAME = sys.argv[2]


grammar = pgf.readPGF(PGF_FILE)
cnc = grammar.languages[CNC_NAME]

target = grammar.languages[sys.argv[3]] if sys.argv[3:] else None

indexfile = sys.argv[4] if sys.argv[4:] else None

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
            p = cnc.parse(s)
            print('-- SUCCESS', sent, apply_index(termindex, s))
            for _, t in p:
                print('-- TREE', t)
                if target:
                    print('-- TRANS', apply_index(termindex, target.linearize(t)))
                    break  ## to get just one parse
            successes += 1
        except pgf.ParseError as pe:
            print('-- FAILURE', sent, s)
            print('-- FAIL CAUSE', pe)
            fails += 1
        sent += 1

print('success', successes, 'failure', fails)

