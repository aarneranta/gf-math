from os import listdir
from os.path import isfile, join
import re

chicago_path = '../../MathGloss/chicago/'

chicago_files = [f for f in listdir(chicago_path) if isfile(join(chicago_path, f))]

contents = {}


def text_only(s: str) -> str:
    r = re.sub("([\$]).*?([\$])", "\g<1>\g<2>", s)  # delete inside $ $
    r = re.sub("([\(]).*?([\)])", "\g<1>\g<2>", r)  # delete inside ( )
    r = r.replace('$$', 'SYMBOL')
    r = ''.join([c for c in r if c not in '()[]*'])
    return r


def get_qid(s):
    q = 'Q'
    for c in s[15:]:
        if c == ']':
            break
        else:
            q += c
    return q


for f in chicago_files:
    with open(join(chicago_path, f), encoding='ascii', errors='ignore') as file:
        b = file.read()
        text = b.split('\n')
        if len(text) > 7:
            contents[f] = (text_only(text[5]), get_qid(text[7]), text[7], 2)
        elif len(text) > 5:
            contents[f] = (text_only(text[5]), 1)
        else:
            contents[f] = (text, 0)


for f in contents:
    print(f, contents[f])

print(len(chicago_files), len(contents))

