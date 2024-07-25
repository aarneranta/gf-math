import sys
import json
import pgf
from gf_utils import *

SYNOPSIS_FILE = 'out/math_terms_synopsis.json'

SEARCHFUN = 'AdjCN'
SEARCHCATS = ['AP', 'CN']
LANGUAGES = {'Eng', 'Fin', 'Fre', 'Ger', 'Ita', 'Por', 'Swe'}
ABSLANG = 'Eng'  # the language used for abstract fun name


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
            dict[qid]['str'] = synopsis[qid][ABSLANG]['str']
#        if qid in dict: print (qid, dict[qid])
    return dict


def build_rules(qid, entry):
    mentry = {}
    absstring = entry['str']
    for cat in entry:
        fun = mk_fun_from_strs([absstring, 'D'+qid, cat])
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
    return (qid, mentry)


with open(SYNOPSIS_FILE) as file:
    synopsis = json.load(file)
    collection = collect_trees(synopsis)
    for qid, entry in collection.items():
        rules = build_rules(qid, entry)
        print(rules)
        



    
