import pgf
import sys

# assumes gf -make <cncname>.gf

# langname = 'Eng'
langname = 'Ger'

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
            

    print('#', 'SUCCESS', success, 'FAILURE', failure)
#    for u in unknowns: print(u)

main()

