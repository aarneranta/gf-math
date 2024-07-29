import pgf
import sys

# usage: stdin | python3 test_tile.py pgf? source? target?

PGF_FILE = 'ForthelDemo.pgf'
CNC_NAME = 'ForthelDemoEng'

if sys.argv[2:]:
    PGF_FILE = sys.argv[1]
    CNC_NAME = sys.argv[2]


grammar = pgf.readPGF(PGF_FILE)
cnc = grammar.languages[CNC_NAME]

target = grammar.languages[sys.argv[3]] if sys.argv[3:] else None

sent = 1
fails = 0
successes = 0
for s in sys.stdin:
    if s.strip():
        try:
            p = cnc.parse(s)
            print('-- SUCCESS', sent, s)
            for _, t in p:
                print(t)
                if target:
                    print('-- TRANS', target.linearize(t))
                    break  ## to get just one parse
            successes += 1
        except pgf.ParseError as pe:
            print('-- FAILURE', sent, s)
            print('-- FAIL CAUSE', pe)
            fails += 1
        sent += 1

print('success', successes, 'failure', fails)

