#!/usr/bin/env python3

import sys
import json
import pgf
from gf_utils import *

if sys.argv[1:]:
    LANGUAGES = sys.argv[1:]

ABSLANG = LANGUAGES[0]  # the language used for abstract fun name
    
SYNOPSIS_FILE = 'out/math_terms_synopsis.json'

SEARCHFUN = 'AdjCN'
SEARCHCATS = ['AP', 'CN']
TARGET_ABSNAME = 'DerivedMathTerms'


def from_SearchFun(tree):
    "return AP and CN from a tree of form AdjCN ap cn"
    fun, args = tree.unpack()
    if fun == SEARCHFUN:
        return [str(arg) for arg in args]

    
def collect_trees(synopsis):
    dict = {}
    for qid in synopsis:
        dict[qid] = {}
        for key in synopsis[qid]:
            if key in LANGUAGES and synopsis[qid][key]['status']:
                lin = synopsis[qid][key]['lin']
                result = from_SearchFun(pgf.readExpr(lin))
                if result:
                    for i in range(len(SEARCHCATS)):
                        cat = SEARCHCATS[i]
                        dict[qid][cat] = dict[qid].get(cat, {})
                        dict[qid][cat][key] = result[i]
                        if qid in dict and ABSLANG in synopsis[qid]:
                            dict[qid][cat]['str'] = synopsis[qid][ABSLANG]['str']
    return dict


def build_rules(qid, entry):
    mentries = []
    for cat in entry:
        mentry = {}
#        print(1, entry[cat])
        absstring = entry[cat]['str']
        fun = mk_fun_from_strs([absstring, qid, 'der', cat])
        mentry['cat'] = cat
        mentry['fun'] = fun
        mentry['status'] = 'derived'
        mentry['lang'] = ABSLANG
        for lang in LANGUAGES:
            if lang in entry[cat]:
                mentry[lang] = {}
                mentry[lang]['lin'] = entry[cat][lang]
                mentry[lang]['str'] = 'NA'
                mentry[lang]['status'] = 'derived'
#        print(2, qid, mentry)
        mentries.append(mentry)
    return (qid, mentries)


with open(SYNOPSIS_FILE) as file:
    synopsis = json.load(file)
    collection = collect_trees(synopsis)
    mdict = {}
    for qid, entry in collection.items():
        _, mentries = build_rules(qid, entry)
        for mentry in mentries:
#            print(3, qid, mentry)
            if 'fun' in mentry:
#                print(4, mentry)
                mdict[qid+'_'+mentry['cat']] = mentry

    print_gf_files(
        TARGET_ABSNAME,
        '--# -path=.:morphodict:../extraction',
        ['Cat'],
        ['Extract'],
        [],
         mdict,
        onlylangs=LANGUAGES
        )
    print('You may want to copy these files to ../mathterms')
    

