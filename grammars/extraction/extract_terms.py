import pgf
import sys

sys.path.append('deptreepy')

import trees 


if sys.argv[1:]:
    langname = sys.argv[1]
else:
    print('usage: extract_terms.py <lang> unknowns(PN?)? < <lines_with_terms>')
    print('assumes gf -make Extract<lang>.gf')
    print('also assumes symlink to deptreepy')
    exit()
    
mode = ''
if sys.argv[2:]:
    mode = sys.argv[2]
    
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


def mk_gf_fun(s):
    if all(c.isdigit() or c.isalpha() or c in "_'" for c in s):
        return s
    else:
        return "'" + s + "'"
 

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

    if mode == 'unknowns':
        for u in unknowns:
            print(u)
            
    if mode == 'unknownsPN':
        for u in unknowns:
            if u[0].isupper():
                fun = mk_gf_fun(u + '_PN')
                print('fun', fun, ':', 'PN', ';')
                print('lin', fun, '=', 'mkPN', '"'+u+'"', ';')

main()

