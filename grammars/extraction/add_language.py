# add a language to an existing multilingual grammar

import pgf
import sys
import json
from extract_terms import extract_term

# give 2-letter and 3-letter language codes as command line arguments
if sys.argv[2:]:
    LAN = sys.argv[1]
    LANG = sys.argv[2]
else:
    print('usage: add_language <fr> <Fre>')
    exit()


WIKIDATA_FILE = '../../data/qid-lexicon.jsonl'

#### step 1: extract wikidata for that language into qdict ####

with open(WIKIDATA_FILE) as file:
    qdict = {}
    for line in file:
        entry = json.loads(line)
        qid = list(entry.keys())[0]  # there is only one
        qdict[qid] = entry[qid].get(LAN, 'NONE')

# to view the results
## for qid in qdict: print('\t'.join([qid, qdict[qid]]))


#### step 2: parse the data with ExtractLANG

# you have to create ExtractLANG and compile it to a pgf file
#   gf -make ExtractLang.gf
# this may assume the existence of MorphoDictLang in GF RGL

PGF_FILE = 'Extract' + LANG + 'Abs.pgf'
CNC_NAME = 'Extract' + LANG

grammar = pgf.readPGF(PGF_FILE)
concrete = grammar.languages[CNC_NAME]

for s in qdict.values():
    val = extract_term(concrete, s)
    print(s, val[0], str(val[1]))




