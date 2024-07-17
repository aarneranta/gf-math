import pgf
from gf_utils import *
import json
import sys

QID_FILE = '../../data/en_de.tsv'
EN_FILE = 'en_gf.tsv'
DE_FILE = 'de_gf.tsv'
EN_PGF_FILE = 'ExtractEngAbs.pgf'
DE_PGF_FILE = 'ExtractGerAbs.pgf'

def get_dict(filename):
  with open(filename) as file:
    endict = {}
    for line in file:
        ws = line.split('\t')
        if ws[1:]:
            endict[ws[0].strip()] = ws[1].strip()
    return endict


dedict = get_dict(DE_FILE)
endict = get_dict(EN_FILE)

engrammar = pgf.readPGF(EN_PGF_FILE)
degrammar = pgf.readPGF(DE_PGF_FILE)

LOST = 'LOST'

def is_lost(s):
    return s == LOST


def val_type(grammar, tree):
    return grammar.inferExpr(tree)[1]


def unify_trees(en, de):
    fen, enargs = en.unpack()
    fde, deargs = de.unpack()
    if fen == fde and len(enargs) == len(deargs) == 1:
        return unify_trees(enargs[0], deargs[0])
    elif is_lost(fen):
        if is_lost(fde):
            return en, de, pgf.readExpr(LOST)
        else:
            return en, de, val_type(degrammar, de)
    else:
        return en, de, val_type(engrammar, en)

def peel_tree(grammar, tree, ty):
    if str(ty) == 'CN':
        return tree, ty
    else:
        fun, args = tree.unpack()
        if len(args) == 1:
            return peel_tree(grammar, args[0], val_type(grammar, args[0]))
        else:
            return tree, ty

    
def adjust_tree(en, de, ty):
    if is_lost(str(ty)):
        return en, de, ty
    elif str(ty) == 'N':
        ty = pgf.readExpr('CN')
        en = pgf.Expr('UseN', [en])
        de = pgf.Expr('UseN', [de])
    elif is_lost(str(en)):
        de, ty = peel_tree(degrammar, de, ty)
    elif is_lost(str(de)):
        en, ty = peel_tree(engrammar, en, ty)
    return en, de, ty


def fix_integers(tree):
    "workaround for GF: integer arguments are changed to strings"
    fun, args = tree.unpack()
    if fun == 'NounIntCN':
        return pgf.Expr('nounStrCN', [
            fix_integers(args[0]),
            pgf.readExpr(quote(str(args[1])))])
    elif fun == 'IntCompoundCN':
        return pgf.Expr('strCompoundCN', [
            pgf.readExpr(quote(str(args[0]))),
            fix_integers(args[1])])
    elif args:
        return pgf.Expr(fun, [fix_integers(arg) for arg in args if not str(arg).isdigit()])
    else:
        return tree
    
    
with open(QID_FILE) as file:
    mdict = {}
    wss = []
    for line in file:
        ws = [w.strip() for w in line.split('\t')]
        en = pgf.readExpr(endict.get(ws[1], LOST).strip())
        de = pgf.readExpr(dedict.get(ws[2], LOST).strip())
        en, de, ty = unify_trees(en, de)
        en, de, ty = adjust_tree(en, de, ty)
        ws.append(str(ty))
        ws.append(str(fix_integers(en)))
        ws.append(str(fix_integers(de)))
        wss.append(ws)
        mdict[ws[0]] = {
            'cat': ws[3],
            'fun': mk_fun('_'.join(ws[1].split() + [ws[0], ws[3]])),
            'status': not is_lost(ws[3]),
            'Eng': {'str': ws[1], 'lin': ws[4], 'status': not is_lost(ws[4])},
            'Ger': {'str': ws[2], 'lin': ws[5], 'status': not is_lost(ws[5])},
        }

        
if sys.argv[1:]:
    mode = sys.argv[1]
    if mode == 'json':
        print(json.dumps(mdict, ensure_ascii=False))
    elif mode == 'gf':
        print_gf_files(
            'MathTerms',
            '--# -path=.:morphodict:../extraction',
            ['Cat'],
            ['Extract'],
            [('Term', 'Utt')],
            mdict)

else:
    [print('\t'.join(ws)) for ws in wss]


        


