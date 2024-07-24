import pgf
import sys

PGF_FILE = 'ForthelDemo.pgf'
CNC_NAME = 'ForthelDemoEng'

grammar = pgf.readPGF(PGF_FILE)
cnc = grammar.languages[CNC_NAME]

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
            successes += 1
        except pgf.ParseError as pe:
            print('-- FAILURE', sent, s)
            print(pe)
            fails += 1
        sent += 1

print('success', successes, 'failure', fails)

