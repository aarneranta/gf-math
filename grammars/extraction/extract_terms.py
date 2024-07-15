import pgf
import sys

sys.path.append('deptreepy')

import trees 


if sys.argv[1:]:
    langname = sys.argv[1]
else:
    print('usage: extract_terms.py <lang> < <lines_with_terms>')
    print('assumes gf -make Extract<lang>.gf')
    print('also assumes symlink to deptreepy')
    exit()

pgfname = 'Extract' + langname + 'Abs.pgf'
cncname = 'Extract' + langname

def extract_term(cnc, s):

    try:
        p = cnc.parse(s)
        _, t = p.__next__()
        return True, t

    except:
        unknowns = [w for w in s.split() if cnc.lookupMorpho(w) == []]
        return False, unknowns
        

def main():
    gr = pgf.readPGF(pgfname)
    cnc = gr.languages[cncname]
    success, failure = 0, 0
    unknowns = set()
    for s in sys.stdin:
        s = s.strip()
        r, result = extract_term(cnc, s)
        if r:
            print(s, result)
            success += 1
        else:
            print(s, 'UNKNOWN', result) 
            failure += 1
            unknowns = unknowns | set(result)
            

#    print('#', 'SUCCESS', success, 'FAILURE', failure)
    for u in unknowns: print(u)

main()

