#!/usr/bin/env python3

# start a multilingual lexicon or add a language to an existing one

# prerequisites:
#   ln -s <deptreepy-dir>
#   ln -s <morphodict-dir>
#   gf -make morphodict/MorphoDict<LANG>.gf
# make sure that your language is in deptreepy/udpipe2_models.py

import pgf
import sys
import os  ## for calling deptreepy
import json
from words_from_ud import words_main, gf_lin
from gf_utils import *

# first make a symlink to deptreepy
sys.path.append('deptreepy')
import trees
import operations

help_message = """  usage: build_lexicon.py (-first|-add)  <fr> <Fre> (-from=<Eng>)? <STEPNUM>+\n
  Step 0: preparations  
  Step 1: extract wikidata for that language into qlist  
  Step 2: parse with UDPipe  
  Step 3: use the UDPipe parse to clean up corpus and add to lexicon  
  Step 5: parse the terms with the extended lexicon  
  Step 6: (if -first) generate GF modules for abstract and the first concrete  
  Step 7: (if -add) add a new concrete syntax
  Step 8: test your grammar in GF\n
  To start the build:    -first en Eng 0 1 2 3 4 5 6 8
  To add a new language: -add de Ger (-from=Eng)? 0 1 2 3 4 5 7 8
  """

# give 2-letter and 3-letter language codes as command line arguments
if sys.argv[4:] and sys.argv[1].startswith('-'):
    MODE = sys.argv[1]
    LAN = sys.argv[2]
    LANG = sys.argv[3]
    STEPS = sys.argv[4:]
else:
    print(help_message)
    exit()


# json and tmp files are stored in out, GF files in the current dir

WIKIDATA_FILE = '../../data/qid-lexicon.jsonl'
NO_WIKILABEL = 'NOWIKILABEL'
EXTRACT_PGF_FILE = 'Extract' + LANG + 'Abs.pgf'
EXTRACT_CNC_NAME = 'Extract' + LANG
QLIST_FILE = 'out/qlist_' + LANG + '.json'
NEW_QLIST_FILE = 'out/new_qlist_' + LANG + '.json'
QDICT_FILE = 'out/qdict_' + LANG + '.json'
CORPUS_FILE = 'out/corpus.tmp'
CONLLU_PARSED_FILE = 'out/terms_' + LANG + '.conllu'
UD_LEXICON_FILE = 'out/lexicon_' + LANG + '.json'
MORPHODICT_CNC =  'MorphoDict' + LANG
MORPHODICT_FILE = MORPHODICT_CNC + 'Abs.pgf'
MATH_WORDS_ABS = 'MathWords' + LANG + 'Abs'
MATH_WORDS_CNC_PREFIX = 'MathWords'
MATH_TERMS = 'MathTerms'
QDICT_SYNOPSIS_FILE = 'out/math_terms_synopsis.json'


print('Processing language', LAN, '=', LANG, '\n')

#### Step 0: preparations ####

FROM_LANG = 'Eng'

fromlangs = [arg[6:] for arg in sys.argv if arg.startswith('-from=')]
if fromlangs:
    FROM_LANG=fromlangs[0]


def clone_extract_gf(fromlang, tolang):
    target_abs = 'Extract'+tolang+'Abs.gf'
    target_ins = 'ExtractSyntax'+tolang+'.gf'
    target_cnc = 'Extract'+tolang+'.gf'
    
    if os.path.isfile(target_cnc):
        print('refused to overwrite', target_cnc)
        return
    
    source_abs = 'Extract'+fromlang+'Abs.gf'
    source_ins = 'ExtractSyntax'+fromlang+'.gf'
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
    
    with open(source_ins) as source:
        txt = source.read().replace(fromlang, tolang)
    with open(target_ins, 'w') as target:
        target.write(txt)
    print('wrote file', target_ins)
    
    print('you may want to take a look and edit these two files')

if '0' in STEPS:
    print('\nStep 0: Cloning ExtractGrammar and compiling MorphoDict\n')

    print('cloning extract grammar from', FROM_LANG)
    clone_extract_gf(FROM_LANG, LANG)

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
        json.dump(qlist, outfile, ensure_ascii=False, indent=2)
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


def parse_with_udpipe(qlis, lang=LANG):

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
    parse_with_udpipe(qlist)
    print("wrote file", CONLLU_PARSED_FILE)

    
#### Step 3: use the UDPipe parse to clean up corpus and add to lexicon ####

if '3' in STEPS:
    if '1' not in STEPS:
        with open(QLIST_FILE) as file:
            qlist = json.load(file)


def conllu2wordinfo(stanza):
    wordlines = trees.read_wordlines(stanza.split('\n'))
    return {w.FORM: w.as_dict() for w in wordlines if w.ID.isdigit()}

        
def get_conllu_dict(qlis, conllu=CONLLU_PARSED_FILE):
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
            json.dump(new_qlist, outfile, ensure_ascii=False, indent=2)
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
    sqitems = get_conllu_dict(qlist)
    print('fixing cases if needed')
    sqitems = fix_case(sqitems)
    rdict = build_raw_gf_dict(sqitems)
    print('statistics words from UD:', len(rdict))
    with open(UD_LEXICON_FILE, 'w') as outfile: 
        json.dump(rdict, outfile, ensure_ascii=False, indent=2)
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


def new_word_rules(moset, udlex):
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
    
    absrules, cncrules = new_word_rules(moset, rdict)
    print('statistics new extracted morphofuns:', len(absrules))

    mdict = build_morphodict(absrules, cncrules)
    print_gf_files(
        MATH_WORDS_ABS,
        '',
        ['Cat'],
        ['Paradigms'],
        [],
        mdict,
        cncprefix=MATH_WORDS_CNC_PREFIX,
        onlylangs=[LANG]
        )


#### Step 5: parse the terms with the extended lexicon ####

if '5' in STEPS:
    if '3' not in STEPS:
        with open(NEW_QLIST_FILE) as file:
            qlist = json.load(file)
        with open(UD_LEXICON_FILE) as file:
            rdict = json.load(file)

            
def parse_terms_gf(grammar, qlis):
    
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
    cmd = 'gf -make -s -probs=Extract.probs Extract' + LANG + '.gf'
    print(cmd)
    os.system(cmd)

    print('parsing terms with', EXTRACT_PGF_FILE)
    print('this will take some time...')
    
    grammar = pgf.readPGF(EXTRACT_PGF_FILE)
    
    qdict = parse_terms_gf(grammar, qlist)
    
    missing = len([0 for v in qdict.values() if v['str'] == NO_WIKILABEL])
    failed = len([0 for v in qdict.values() if not v['status']])
    succeeded = len([0 for v in qdict.values() if v['status']])

    print('statistics missing labels:', missing)
    print('statistics successful GF parses:', succeeded)
    print('statistics failed GF parses:', failed-missing)

    with open(QDICT_FILE, 'w') as outfile:
        json.dump(qdict, outfile, ensure_ascii=False, indent=2)
    print('wrote file', QDICT_FILE)


#### Step 6: generate GF modules for abstract and the first concrete ####

def mk_mdict(qdict, grammar, lang=LANG, defaultcat='MT'):
    mdict = {}
    for qid in qdict:
        info = qdict[qid]
        status = info['status']
        lin = pgf.readExpr(info['lin']) if status else empty_variants()
        cat = val_type(grammar, lin) if status else defaultcat
        if status:
            lin, cat = peel_tree(grammar, lin, cat)
            lin = fix_integers_in_Extract(lin)
        fun = mk_fun_from_strs([info['str'], qid, str(cat)])
        mdict[qid] = {
            'cat': str(cat),
            'fun': fun,
            'lang': LANG,  # the abstract syntax id lang
            'status': status,
            LANG : {
                'str': info['str'],
                'lin': str(lin),
                'status': status
                }
            }
    return mdict
 

if '6' in STEPS and MODE=='-first':
    print('\nStep 6: Generating initial GF modules\n')

    if '5' not in STEPS:
        with open(QDICT_FILE) as file:
            qdict = json.load(file)
        grammar = pgf.readPGF(EXTRACT_PGF_FILE)

    mdict = mk_mdict(qdict, grammar)
    
    with open(QDICT_SYNOPSIS_FILE, 'w') as file:
        json.dump(mdict, file, ensure_ascii=False, indent=2)
    print('wrote file', QDICT_SYNOPSIS_FILE)

    print_gf_files(
        MATH_TERMS,
        '--# -path=.:morphodict:../extraction',
        ['Cat'],
        ['Extract'],
        [('MT', 'Utt')],
        mdict
        )

    
#### Step 7: add a new concrete syntax ####
        
def add_new_language(qdict_syn, qdict, grammar, lang, defaultcat='MT'):
    for qid in qdict_syn:
        syninfo = qdict_syn[qid]
        cncinfo = qdict[qid] 
        status = cncinfo['status']
        
        lin = pgf.readExpr(cncinfo['lin']) if status else empty_variants()
        sought_cat = syninfo['cat']
        oldcat = val_type(grammar, lin) if status else defaultcat
        if status:
            lin, cat = peel_tree(grammar, lin, oldcat)
            lin = fix_integers_in_Extract(lin)
            status = (str(cat) == sought_cat)
        qdict_syn[qid][lang] = {
                'str': cncinfo['str'] + ('' if status else (' ' + str(lin))),
                'lin': str(lin),
                'status': status
                }
    return qdict_syn
    

if '7' in STEPS and MODE=='-add':
    with open(QDICT_SYNOPSIS_FILE) as file:
        qdict_syn = json.load(file)
    print('loaded', QDICT_SYNOPSIS_FILE)
    
    if '5' not in STEPS:
        with open(QDICT_FILE) as file:
            qdict = json.load(file)
        print('loaded existing', QDICT_FILE)
        grammar = pgf.readPGF(EXTRACT_PGF_FILE)

    qdict = add_new_language(qdict_syn, qdict, grammar, LANG)
        
    with open(QDICT_SYNOPSIS_FILE, 'w') as file:
        json.dump(qdict, file, ensure_ascii=False, indent=2)

    print('wrote file, updated for', LANG, QDICT_SYNOPSIS_FILE)

    print_gf_files(
        MATH_TERMS,
        '--# -path=.:morphodict:../extraction',
        ['Cat'],
        ['Extract'],
        [('MT', 'Utt')],
        qdict,
        abstract=False,
        onlylangs=[LANG]
        )
    if '8' not in STEPS:
        "Now consider running Step 8 to test the grammar!"

    
#### Step 8: run a simple test ###

if '8' in STEPS:
    cmd = ('echo "gr -cat=CN -number=11 | l -table -treebank -bind" | gf -run -s ' +
           MATH_TERMS + LANG + '.gf')
    print('executing:', cmd)
    os.system(cmd)
    print('If the test is successful, consider copying',
          MATH_TERMS + LANG + '.gf', 'to ../mathterms') 


