from os import listdir
from os.path import isfile, join
import sys

# def equality = 61
# prop equality ＝ 65309

# usage: process_lagda (all | fons) clean_idents?

MODE = 'all' 
if sys.argv[2:]:
    MODE = sys.argv[2]

FLAGS = sys.argv[3:]


def comment(s):
    return f'-- {s}'


def extract_agda(mdfile):
    agdacode = [comment(mdfile)]
    inside = False
    with open(mdfile) as file:
        for line in file:
            if line.startswith("```agda"):
                inside = True
            elif line.startswith("```"):
                inside = False
            elif inside:
                agdacode.append(line)
    return agdacode


def new_constant(line):
    if not line[0].isspace() and (ws := line.split())[2:] and ws[1] == ':':
        return ws[0]


def clean_idents(s):
    ss = []
    for c in s:
        o = ord(c)
        if o > 256:
            ss.append('_U'+str(o)+'_')
        else:
            ss.append(c)
    return ''.join(ss)


if __name__ == '__main__':
    path = sys.argv[1]
    files = [join(path, file) for file in listdir(path) if file.endswith('lagda.md')]
    for file in files:
        code = extract_agda(file)
        if 'clean_idents' in FLAGS:
            code = [clean_idents(line) for line in code]
        if MODE == 'funs':
            for line in code:
                if c := new_constant(line):
                    print(c)
        else:
            for line in code:
                print(line.rstrip())


        
        
