import pgf

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


def val_type(grammar, tree):
    return grammar.inferExpr(tree)[1]


def unify_trees(en, de):
    fen, enargs = en.unpack()
    fde, deargs = de.unpack()
    if fen == fde and len(enargs) == len(deargs) == 1:
        return unify_trees(enargs[0], deargs[0])
    elif fen == 'LOST':
        if fde == 'LOST':
            return en, de, pgf.readExpr('LOST')
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
    if str(ty) == 'LOST':
        return en, de, ty
    elif str(ty) == 'N':
        ty = pgf.readExpr('CN')
        en = pgf.Expr('UseN', [en])
        de = pgf.Expr('UseN', [de])
    elif str(en) == 'LOST':
        de, ty = peel_tree(degrammar, de, ty)
    elif str(de) == 'LOST':
        en, ty = peel_tree(engrammar, en, ty)
    return en, de, ty

    
with open(QID_FILE) as file:
    for line in file:
        ws = [w.strip() for w in line.split('\t')]
        en = pgf.readExpr(endict.get(ws[1], 'LOST').strip())
        de = pgf.readExpr(dedict.get(ws[2], 'LOST').strip())
        en, de, ty = unify_trees(en, de)
        en, de, ty = adjust_tree(en, de, ty)
        ws.append(str(ty))
        ws.append(str(en))
        ws.append(str(de))
        print('\t'.join(ws))

