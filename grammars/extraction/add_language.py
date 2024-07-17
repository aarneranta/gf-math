# add a language to an existing multilingual grammar

import pgf
import sys
import json
from extract_terms import extract_term
from words_from_ud import words_main
from gf_utils import print_gf_files

# make a symlink to deptreepy
sys.path.append('deptreepy')
import trees

# give 2-letter and 3-letter language codes as command line arguments
if sys.argv[3:]:
    LAN = sys.argv[1]
    LANG = sys.argv[2]
    PHASES = sys.argv[3:]
else:
    print('usage: add_language <fr> <Fre> <phase>+')
    exit()


WIKIDATA_FILE = '../../data/qid-lexicon.jsonl'
PGF_FILE = 'Extract' + LANG + 'Abs.pgf'
CNC_NAME = 'Extract' + LANG
QDICT_FILE = 'qdict_' + LANG + '.json'
UNPARSED_TERMS_FILE = 'unparsed_terms' + LANG + '.tmp'
UNKNOWN_WORDS_FILE = 'unknown_words' + LANG + '.tmp'
CONLLU_PARSED_FILE = 'terms_' + LANG + '.conllu'
MORPHO_FILE = 'morphofuns_' + LANG + '.tmp'
MATH_WORDS_ABS = 'MathWords' + LANG + 'Abs'
MATH_WORDS_CNC_PREFIX = 'MathWords'


#### step 1: extract wikidata for that language into qdict ####

if '1' in PHASES:
  with open(WIKIDATA_FILE) as file:
    qdict = {}
    for line in file:
        entry = json.loads(line)
        qid = list(entry.keys())[0]  # there is only one
        qdict[qid] = entry[qid].get(LAN, 'NONE')
    with open(QDICT_FILE, 'w') as outfile:
        json.dump(qdict, outfile, ensure_ascii=False)


#### step 2: parse the data with ExtractLANG to find unknown words

# you have to create ExtractLANG and compile it to a pgf file
#   gf -make ExtractLang.gf
# this may assume the existence of MorphoDictLang in GF RGL

grammar = pgf.readPGF(PGF_FILE)
concrete = grammar.languages[CNC_NAME]

if '1' not in PHASES:
    with open(QDICT_FILE) as file:
        qdict = json.load(file)


if '2' in PHASES:
# parse just to find the unknown words
  unknowns = []
  tohandle = []
  for s in qdict.values():

    val = extract_term(concrete, s)
    if val[0] == False and s[0].isupper():  # try again if capitalized
        s1 = s[0].upper() + s[1:]
        val = extract_term(concrete, s1)
    if val[0] == False:
        unknowns.extend(val[1])
        tohandle.append(s)

  unknown_words = set(unknowns)
  terms_toparse = set(tohandle)

# save unparsed terms and unknown words in files
  with open(UNKNOWN_WORDS_FILE, 'w') as file:
      for w in unknown_words:
          file.write(w + '\n')

  with open(UNPARSED_TERMS_FILE, 'w') as file:
    for s in terms_toparse:
        file.write(s + '\n\n')  # UDPipe need spaces between sentences


### Step 3: parse the unknown terms with UDPipe and extract a lexicon

##  this is at the moment done in the deptreepy directory by

# cat <currentdir>/unparsed_termsFre.tmp | ./deptreepy.py txt2conllu >terms_Fre.conllu
# mv terms_Fre.conllu <currentdir>

# this assumes that you have set the model in deptreepy/udpipe2_params.yaml
# to something suitable for your language; see
# https://lindat.mff.cuni.cz/services/udpipe/api/models


### Step 4: extract a lexicon of unknown words from the UD parse result

## external now: generate MORPHO_FILE_LANG by pg -funs from MorphoDictLANG
## also: refine the extraction rules in words_from_ud.gf_lin()

if '4' in PHASES:
    absrules, cncrules = words_main(
        CONLLU_PARSED_FILE, MORPHO_FILE, UNKNOWN_WORDS_FILE, LANG)
    mdict = {}
    for i in range(len(absrules)):
        mdict[i] = {
            'cat': absrules[i][1],
            'fun': absrules[i][0],
            'status': True,
            LANG: {'str': '', 'lin': cncrules[i][1], 'status': True}
            }

    print_gf_files(
        MATH_WORDS_ABS, '', ['Cat'],
        ['Paradigms'], [], mdict,
        cncprefix=MATH_WORDS_CNC_PREFIX
        )

### Step 5: parse the terms again with the extended lexicon

## do: gf -make ExtractLANG.gf

if '5' in PHASES:
    
  for s in qdict.values():

    val = extract_term(concrete, s)
    if val[0] == False and s[0].isupper():  # try again if capitalized
        s1 = s[0].upper() + s[1:]
        val = extract_term(concrete, s1)
        
    print(val[0], str(val[1]))



    





