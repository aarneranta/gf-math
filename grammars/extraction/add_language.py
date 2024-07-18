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
from gf_utils import print_gf_files, mk_fun

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
EXTRACT_PGF_FILE = 'Extract' + LANG + 'Abs.pgf'
EXTRACT_CNC_NAME = 'Extract' + LANG
QLIST_FILE = 'qlist_' + LANG + '.json'
NEW_QLIST_FILE = 'new_qlist_' + LANG + '.json'
QDICT_FILE = 'qdict_' + LANG + '.json'
CORPUS_FILE = 'corpus.tmp'
CONLLU_PARSED_FILE = 'terms_' + LANG + '.conllu'
UD_LEXICON_FILE = 'lexicon_' + LANG + '.json'
MORPHODICT_CNC =  'MorphoDict' + LANG
MORPHODICT_FILE = MORPHODICT_CNC + 'Abs.pgf'
MATH_WORDS_ABS = 'MathWords' + LANG + 'Abs'
MATH_WORDS_CNC_PREFIX = 'MathWords'


print('Processing language', LAN, '=', LANG, '\n')

#### Step 0: preparations ####

DEFAULT_FROM_LANG = 'Eng'

def clone_extract_gf(fromlang=DEFAULT_FROM_LANG, tolang=LANG):
    target_abs = 'Extract'+tolang+'Abs.gf'
    target_cnc = 'Extract'+tolang+'.gf'
    
    if os.path.isfile(target_cnc):
        print('refused to overwrite', target_cnc)
        return
    
    source_abs = 'Extract'+fromlang+'Abs.gf'
    source_cnc = 'Extract'+fromlang+'.gf'

    with open(source_abs) as source:
        txt = source.read().replace(fromlang, tolang)
    with open(target_abs, 'w') as target:
        target.write(txt)
    print('wrote file', target_abs)
        
    with open(source_cnc) as source:
        txt = source.read().replace(fromlang, tolang)
    with open(target_cnc, 'w') as target:
        target.write(txt)
    print('wrote file', target_cnc)
    print('you may want to take a look and edit these two files')
    

if '0' in STEPS:
    print('\nStep 1: Extracting Wikidata labels\n')

    print('cloning extract grammar from', DEFAULT_FROM_LANG)
    clone_extract_gf()

    cmd = 'gf -make -s morphodict/' + MORPHODICT_CNC + '.gf'
    print('executing:', cmd)
    os.system(cmd)
    
    if STEPS == ['0']:
        exit()

        


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
    print('\nStep 1: Extracting Wikidata labels\n')
    print('reading', WIKIDATA_FILE)
    qlist = extract_wikidata()
    print('wrote file', QLIST_FILE)
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
    print('\nStep 2: Parsing data with UDPipe\n')
    print('this will take some time...')
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
        with open(NEW_QLIST_FILE, 'w') as outfile:
            json.dump(new_qlist, outfile, ensure_ascii=False)
    print('wrote corrected file', NEW_QLIST_FILE)

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
            if lin:
                lemma = lin[0]
            else:
                lemma = w
            if not lemma.startswith('UNK'):
                rdict[lemma] = rdict.get(lemma, []) + [lin]
    rdict = {lemma: majority_element(rdict[lemma]) for lemma in rdict}
    return rdict


if '3' in STEPS:
    print('\nStep 3: Analysing data with UD results\n')
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
        with open(NEW_QLIST_FILE) as file:
            qlist = json.load(file)
        with open(UD_LEXICON_FILE) as file:
            rdict = json.load(file)

            
def read_morpho_funs(pgf_file=MORPHODICT_FILE):
    grammar = pgf.readPGF(pgf_file)
    return set(grammar.functions)


def new_word_rules(moset, udlex=rdict):
    absrules = []
    cncrules = []
    for k in udlex:
        info = udlex[k]
        if info:
            fun = mk_fun(info[0] + '_' + info[2])
            if fun not in moset:
                absrules.append((fun, info[2]))
                cncrules.append((fun, info[1]))
    return absrules, cncrules


def build_morphodict(absrules, cncrules):
    mdict = {}
    for i in range(len(absrules)):
        mdict[i] = {
            'cat': absrules[i][1],
            'fun': absrules[i][0],
            'status': True,
            LANG: {'str': '', 'lin': cncrules[i][1], 'status': True}
            }
    return mdict


if '4' in STEPS:
    print('\nStep 4: Building lexicon extension\n')
    
    print('reading', MORPHODICT_FILE)
    moset = read_morpho_funs()
    print('statistics given morphofuns:', len(moset))
    
    absrules, cncrules = new_word_rules(moset)
    print('statistics new extracted morphofuns:', len(absrules))

    mdict = build_morphodict(absrules, cncrules)
    print_gf_files(
        MATH_WORDS_ABS, '', ['Cat'],
        ['Paradigms'], [], mdict,
        cncprefix=MATH_WORDS_CNC_PREFIX
        )


#### Step 5: parse the terms with the extended lexicon ####

if '5' in STEPS:
    if '3' not in STEPS:
        with open(NEW_QLIST_FILE) as file:
            qlist = json.load(file)
        with open(UD_LEXICON_FILE) as file:
            rdict = json.load(file)

            
def parse_terms_gf(qlis=qlist, pgf_file=EXTRACT_PGF_FILE):
    grammar = pgf.readPGF(pgf_file)
    concrete = grammar.languages[EXTRACT_CNC_NAME]

    qdict = {}
    for qs in qlis:
        q = qs[0]
        s = qs[1]

        if s == NO_WIKILABEL:
            val = [False, 'variants {}']
        else:
            val = extract_term(concrete, s)
        qdict[q] = {'str': s, 'lin': str(val[1]), 'status': val[0]}
    return qdict


if '5' in STEPS:
    print('\nStep 5: Parsing with GF\n')
    
    print('make sure you have', EXTRACT_CNC_NAME+'.gf')
    print('building a new', EXTRACT_PGF_FILE)
    cmd = 'gf -make -s Extract' + LANG + '.gf'
    print(cmd)
    os.system(cmd)

    print('parsing terms with', EXTRACT_PGF_FILE)
    print('this will take some time...')

    qdict = parse_terms_gf()
    
    missing = len([0 for v in qdict.values() if v['str'] == NO_WIKILABEL])
    failed = len([0 for v in qdict.values() if not v['status']])
    succeeded = len([0 for v in qdict.values() if v['status']])

    print('statistics missing labels:', missing)
    print('statistics successful GF parses:', succeeded)
    print('statistics failed GF parses:', failed-missing)

    with open(QDICT_FILE, 'w') as outfile:
        json.dump(qdict, outfile, ensure_ascii=False)
    print('wrote file', QDICT_FILE)
    

    


    



    





