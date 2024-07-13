import pgf
import sys

# assumes gf -make <cncname>.gf

langname = 'Eng'
# langname = 'Ger'

pgfname = 'Extract' + langname + 'Abs.pgf'
cncname = 'Extract' + langname

def extract_term(cnc, s):

    try:
        p = cnc.parse(s)
        _, t = p.__next__()
        print(s, '\t', t)
        success = True

    except:
        unknowns = [w for w in s.split() if cnc.lookupMorpho(w) == []]
        print(s, '\t', 'UNKNOWN ' + ' '.join(unknowns))
        success = False

    return success
        

def main():
    gr = pgf.readPGF(pgfname)
    cnc = gr.languages[cncname]
    success, failure = 0, 0
    for s in sys.stdin:
        r = extract_term(cnc, s.strip())
        if r:
            success += 1
        else:
            failure += 1

    print('#', 'SUCCESS', success, 'FAILURE', failure)
        

main()

