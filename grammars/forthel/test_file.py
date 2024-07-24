import pgf
import sys

PGF_FILE = 'ForthelDemo.pgf'
CNC_NAME = 'ForthelDemoEng'

grammar = pgf.readPGF(PGF_FILE)
cnc = grammar.languages[CNC_NAME]

sent = 0
for s in sys.stdin:
    if s.strip():
        try:
            p = cnc.parse(s)
            print('-- SUCCESS', sent, s)
            for _, t in p:
                print(t)
        except pgf.ParseError as pe:
            print('-- FAILURE', sent, s)
            print(pe)
        sent += 1

