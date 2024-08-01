import sys
import json

TERMINDEX_FILE = 'TERMINDEX.json'

# replace terms in math environment with running indices
# and build a dictionary
# the latex code is read from stdin, and
# the new latex code is printed into stdout;
# the index is printed in the file TERMINDEX.json
# this presupposes, for simplicity, that the file has been tokenized
# with e.g. gf -lexmixed
# both $ and $$ environments are dealt with

# for the sake of Forthel parsing, single variables and their
# lists might not be replaced by indices


def index_term(i, tok):
    return tok + ' \\INDEXEDTERM{ ' + str(i) + ' } ' + tok


def is_varlist(s):
    s = s[1:-1]  # dropping $s
    return all(c.isspace() or c.isalpha() or c==',' for c in s)


def index_in_text(s, index=None, also_variables=False):
    if not index:
        index = {}
    current_number = len(index) + 1
    ss = s.split()
    in_math = False
    newss = []
    segment = []
    i = 0
    while ss:
        tok = ss.pop(0) 
        if tok in ['$', '$$']:
            segment.append(tok)
            if in_math:
                indexed = ' '.join(segment[1:-1])  # $ not in index value
                newtok = ' '.join(segment)  # $ shown in doc
                if is_varlist(newtok) and not also_variables:
                    newss.append(newtok)
                else:
                    iterm = index_term(current_number, tok)
                    newss.append(iterm)
                    index[current_number] = indexed
                    current_number += 1
                segment = []
            in_math = not in_math
        elif in_math:
            segment.append(tok)
        else:
            newss.append(tok)
    return index, ' '.join(newss)


if __name__ == '__main__':
    index = {}
    for line in sys.stdin:
        index, line = index_in_text(line, index)
        print(line)

    with open(TERMINDEX_FILE, 'w') as outfile:
        json.dump(index, outfile, ensure_ascii=False, indent=2)

        


"""
ex = r'all numbers $ x , y $ are $ x * y $ times for $$ \foo $$'

yields

({1: '$ x * y $', 2: '$$ \\foo $$'},
'all numbers $ x , y $ are $ \\INDEXEDTERM{ 1 } $ times for $$ \\INDEXEDTERM{ 2 } $$')
"""            
        
    

