from itertools import groupby
import pgf

# to be run and then filtered manually: redirect to terms.tmp
# gf -make DerivedMathTermsEng.gf DerivedMathTermsFre.gf DerivedMathTermsGer.gf DerivedMathTermsSwe.gf

grammar = pgf.readPGF('DerivedMathTerms.pgf')

eng = grammar.languages['DerivedMathTermsEng']
fre = grammar.languages['DerivedMathTermsFre']
ger = grammar.languages['DerivedMathTermsGer']
swe = grammar.languages['DerivedMathTermsSwe']

langs = [eng, fre, ger, swe]


def iflin(s):
    return 'None' if s.startswith('[') else s

funs = []
for fun in grammar.functions:
    exp = pgf.Expr(fun, [])
    funs.append(([iflin(lang.linearize(exp)) for lang in langs], fun))

funs.sort(key = lambda e: e[0])

def clean_word(w):
    return w != 'None' and len(w.split()) == 1

gfuns = [list(g) for k, g in  groupby(funs, key=lambda e: e[0])]
gfuns = [(g[0][0], [f[1] for f in g]) for g in gfuns]
clean_gfuns = [f for f in gfuns if clean_word(f[0][0]) and clean_word(f[0][1])]

# to create data for manual inspection: redirect to terms.tmp and remove unwanted lines
for fun in clean_gfuns:
    print(fun)

# to test data integrity
"""
with open('terms.tmp') as file:
    for line in file:
        print(type(eval(line)))
"""        
