# add a language to an existing multilingual grammar

# prerequisites:
#   ln -s <deptreepy-dir>
#   ln -s <morphodict-dir>
#   gf -make morphodict/MorphoDict<LANG>.gf
# make sure that your language is in deptreepy/udpipe2_models.py

import pgf
import sys
import os  ## for calling deptreepy
import json
from extract_terms import extract_term
from words_from_ud import words_main, gf_lin
from gf_utils import print_gf_files

# first make a symlink to deptreepy
sys.path.append('deptreepy')
import trees
import operations

# give 2-letter and 3-letter language codes as command line arguments
if sys.argv[3:]:
    LAN = sys.argv[1]
    LANG = sys.argv[2]
    STEPS = sys.argv[3:]
else:
    print('usage: add_language <fr> <Fre> <phase>+')
    exit()


WIKIDATA_FILE = '../../data/qid-lexicon.jsonl'
NO_WIKILABEL = 'NOWIKILABEL'
PGF_FILE = 'Extract' + LANG + 'Abs.pgf'
CNC_NAME = 'Extract' + LANG
QLIST_FILE = 'qlist_' + LANG + '.json'
QDICT_FILE = 'qdict_' + LANG + '.json'
CORPUS_FILE = 'corpus.tmp'
CONLLU_PARSED_FILE = 'terms_' + LANG + '.conllu'
UD_LEXICON_FILE = 'lexicon_' + LANG + '.json'
MORPHODICT_CNC =  'MorphoDict' + LANG
MORPHODICT_FILE = MORPHODICT_CNC + 'Abs.pgf'

UNPARSED_TERMS_FILE = 'unparsed_terms' + LANG + '.tmp'
UNKNOWN_WORDS_FILE = 'unknown_words' + LANG + '.tmp'
MORPHO_FILE = 'morphofuns_' + LANG + '.tmp'
MATH_WORDS_ABS = 'MathWords' + LANG + 'Abs'
MATH_WORDS_CNC_PREFIX = 'MathWords'


print('Processing language', LAN, '=', LANG, '\n')

#### Step 1: extract wikidata for that language into qlist ####

# at this point, not a dict, so to maintain an order
def extract_wikidata(lang=LAN, wikifile=WIKIDATA_FILE, qlistfile=QLIST_FILE):
  with open(wikifile) as file:
    qlist = []
    for line in file:
        entry = json.loads(line)
        qid = list(entry.keys())[0]  # there is only one
        qlist.append([qid, entry[qid].get(lang, NO_WIKILABEL).strip()])
    with open(qlistfile, 'w') as outfile:
        json.dump(qlist, outfile, ensure_ascii=False)
    return qlist
        
if '1' in STEPS:
    print('\nStep 1\n')
    print('reading', WIKIDATA_FILE)
    qlist = extract_wikidata()
    print('wrote file', QDICT_FILE)
    print('statistics terms:', len(qlist))
    print('statistics no label:',
          len([v for v in qlist if v[1] == NO_WIKILABEL]))
          

#### Step 2: parse with UDPipe ####

if '2' in STEPS:
    if '1' not in STEPS:
        with open(QLIST_FILE) as file:
            qlist = json.load(file)


def parse_with_udpipe(qlis=qlist, lang=LANG):

    with open(CORPUS_FILE, 'w') as outfile:
        for it in qlis:
            outfile.write(it[1] + '\n\n')
    print("wrote file", CORPUS_FILE)

    cmd = ("cat " + CORPUS_FILE + " | deptreepy/deptreepy.py 'txt2conllu " +
          LANG + "'> " + CONLLU_PARSED_FILE)
    print('executing', cmd)
    os.system(cmd)
        
if '2' in STEPS:
    print('\nStep 2\n')
    parse_with_udpipe()
    print("wrote file", CONLLU_PARSED_FILE)

    
#### Step 3: use the UDPipe parse to clean up corpus and add to lexicon ####

if '3' in STEPS:
    if '1' not in STEPS:
        with open(QLIST_FILE) as file:
            qlist = json.load(file)


def conllu2wordinfo(stanza):
    wordlines = trees.read_wordlines(stanza.split('\n'))
    return {w.FORM: w.as_dict() for w in wordlines if w.ID.isdigit()}

        
def get_conllu_dict(qlis=qlist, conllu=CONLLU_PARSED_FILE):
    with open(conllu) as file:
        intxt = file.read()
        stanzas = [span for span in intxt.split("\n\n") if span.strip()]

    stanzadict = {s.split('\n')[2][9:].strip(): s for s in stanzas}

    sqitems = []
    notparsed = []
    for qs in qlis:
        s = qs[1]
        stanza = stanzadict.get(s, 'NOCONLLU')
        sqitems.append((qs[0], (s, conllu2wordinfo(stanza))))
        if stanza == 'NOCONLLU':
            notparsed += [s]

    if notparsed:
        print('statistics no conllu parse:', len(notparsed))

    return sqitems


# this mainly to fix wrongly capitalized words in Wikidata labels
def fix_case(sqitems):
    fixed = []
    for i in range(len(sqitems)):
        it = sqitems[i]
        s = it[1][0]
        ws = s.split()
        dict = it[1][1]
        ns = []
        for w in ws:
            if w in dict:
                lemma = dict[w]['LEMMA']
                form = dict[w]['FORM']
            else:
                lemma = w
                form = w
            if lemma[0].lower() == w[0].lower():  ## parser may introduce else
                nw = lemma[0] + w[1:]  # fixing the case to the lemma
            if w != nw:
            ##    print(w, '->', nw)
                fixed.append(w)
            ns.append(nw)
        sqitems[i] = (it[0], (ns, dict))

    # replace the old qlist file if needed
    if len(fixed):
        print('statistics case fixed:', len(fixed))
        new_qlist = [[qid, ' '.join(s)] for (qid, (s,_)) in sqitems]
        with open(QLIST_FILE, 'w') as outfile:
            json.dump(new_qlist, outfile, ensure_ascii=False)
        print('wrote corrected file', QLIST_FILE)

    # for use in lexicon extraction
    return sqitems


def majority_element(xs):
    if len(xs) == 1:
        return xs[0]
    else:
        freqs = {}
        for x in xs:
            freqs[x] = freqs.get(x, 0) + 1
        return max([x for x in freqs], key=lambda x: freqs[x])
    
        
def build_raw_gf_dict(sqitems):
    rdict = {}
    for it in sqitems:
        dict = it[1][1]
        for w in dict:
            info = tuple(dict[w][f] for f in ['LEMMA', 'POS', 'FEATS'])
            lin = gf_lin(w, info, LANG)
            lemma = lin[0]
            if lemma != 'UNK':
                rdict[lemma] = rdict.get(lemma, []) + [lin]
    rdict = {lemma: majority_element(rdict[lemma]) for lemma in rdict}
    return rdict


if '3' in STEPS:
    print('\nStep 3\n')
    print('reading', CONLLU_PARSED_FILE, 'to collect information')
    sqitems = get_conllu_dict()
    print('fixing cases if needed')
    sqitems = fix_case(sqitems)
    rdict = build_raw_gf_dict(sqitems)
    print('statistics words from UD:', len(rdict))
    with open(UD_LEXICON_FILE, 'w') as outfile: 
        json.dump(rdict, outfile, ensure_ascii=False)
    print('wrote file', UD_LEXICON_FILE)


### step 4: build an extension of MorphoDictLANG with UD_LEXICON_FILE ###

if '4' in STEPS:
    if '3' not in STEPS:
        with open(QLIST_FILE) as file:
            qlist = json.load(file)
        with open(UD_LEXICON_FILE) as file:
            ud_lexicon = json.load(file)

            
def read_morpho_funs(pgf_file=MORPHODICT_FILE):
    grammar = pgf.readPGF(pgf_file)
    return set(grammar.functions)


if '4' in STEPS:
    print('\nStep 4\n')
    moset = read_morpho_funs()
    print(moset)



            
if False:
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




    
            
#### step 2: parse the data with ExtractLANG to find unknown words

# you have to create ExtractLANG and compile it to a pgf file
#   gf -make ExtractLang.gf
# this may assume the existence of MorphoDictLang in GF RGL

grammar = pgf.readPGF(PGF_FILE)
concrete = grammar.languages[CNC_NAME]

if '1' not in STEPS:
    with open(QDICT_FILE) as file:
        qdict = json.load(file)


if '2' in STEPS and False:
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

if '4' in STEPS and False:
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

if '5' in STEPS:
    
  for s in qdict.values():

    val = extract_term(concrete, s)
    if val[0] == False and s[0].isupper():  # try again if capitalized
        s1 = s[0].upper() + s[1:]
        val = extract_term(concrete, s1)
        
    print(val[0], str(val[1]))



    





