import pgf

def mk_fun(s):
    s = '_'.join(s.split())  # spaces replaced by underscores
    
    if (s[0].isalpha() and
        all(ord(c)<256 and (c.isdigit() or c.isalpha() or c in "_'")
            for c in s)):  # test if legal GF identifier
        return s
    else:
        return "'" + s.replace("'", "\\'") + "'"  # if not, single quotes make it legal


def mk_fun_from_strs(ss):
    return mk_fun('_'.join(ss))


def quote(s):
    return '"' + s + '"'


def app(fun, args):
    return ' '.join([fun, quote(args[0])] + args[1:])

def empty_variants():
    return 'variants {}'

def mk_lin(oper, words, params):
    return ' '.join([oper] + [quote(w) for w in words] + params) 


def mk_cat_rule(cat):
    return ' '.join(['cat', cat, ';\n'])


def mk_fun_rule(fun, cat, comment=None):
    co = '--' + comment if comment else ''
    return ' '.join(['fun', fun, ':', cat, ';', co, '\n'])


def mk_lin_rule(fun, lin, comment=None):
    co = '--' + comment if comment else ''
    return ' '.join(['lin', fun, '=', lin, ';', co, '\n'])


def mk_lincat_rule(cat, lin):
    return ' '.join(['lincat', cat, '=', lin, ';\n'])

    
def print_gf_files(absname, path, extends, opens, newcats, mdict, cncprefix=None):
    "dict format: qid: {'cat', 'fun', 'status', *lang: {'str', 'lin', 'status'}}"
    
    with open(absname + '.gf', 'w') as absfile:
        absfile.write(path + '\n')
        absfile.write(' '.join(['abstract', absname, '='] +
                               [', '.join(extends)] +
                               (['**'] if extends else []) +
                               ['{\n']))
        for cat in newcats:
            absfile.write(mk_cat_rule(cat[0]))
        for qid in mdict:
            ## if mdict[qid]['status']:
                absfile.write(mk_fun_rule(mdict[qid]['fun'], mdict[qid]['cat']))
        absfile.write('}\n')
        print('wrote file', absname + '.gf')
        
    for qid in mdict:
        langs = list(mdict[qid].keys())[3:]
        break  # get lang names from the first item

    for lang in langs:
        cncname = (cncprefix if cncprefix else absname) + lang
        with open(cncname + '.gf', 'w') as cncfile:
            cncfile.write(path + '\n')
            cncfile.write(
                ' '.join(['concrete', cncname, 'of', absname, '='] +
                         [', '.join([e + lang for e in extends])] +
                         (['**'] if extends else []) +
                         (['open'] if opens else []) +
                         [', '.join([o + lang for o in opens])] +
                         (['in'] if opens else []) +
                         ['{\n'])
                )
            for cat, lincat in newcats:
                cncfile.write(mk_lincat_rule(cat, lincat))
                
            for qid in mdict:
                fun = mdict[qid]['fun']
                print('mdict[qid]', mdict[qid])
                print('mdict[qid][lang]', mdict[qid][lang])
                if mdict[qid][lang]['status']:
                    rule = mk_lin_rule(fun, mdict[qid][lang]['lin'])
                else:
                    rule = mk_lin_rule(fun, empty_variants(),
                                       comment=mdict[qid][lang]['str'])     
                cncfile.write(rule)
                
            cncfile.write('}\n')
            print('wrote file', cncname + '.gf')

        
#### extract-specific functions ####

# use types in Extract*.gf in gf-math

def is_lost(s):
    return s == []


def val_type(grammar, tree):
    return grammar.inferExpr(tree)[1]


def peel_tree(grammar, tree, ty, sought_cat='CN'):
    "peel out unary applications but stop at a given cat such as CN"
    if str(ty) == sought_cat:
        return tree, ty
    else:
        fun, args = tree.unpack()
        if len(args) == 1:
            return peel_tree(grammar, args[0], val_type(grammar, args[0]))
        else:
            return tree, ty


def fix_integers_in_Extract(tree):
    "workaround for GF: integer arguments are changed to strings"
    fun, args = tree.unpack()
    if fun == 'NounIntCN':
        return pgf.Expr('nounStrCN', [
            fix_integers_in_Extract(args[0]),
            pgf.readExpr(quote(str(args[1])))])
    elif fun == 'IntCompoundCN':
        return pgf.Expr('strCompoundCN', [
            pgf.readExpr(quote(str(args[0]))),
            fix_integers_in_Extract(args[1])])
    elif args:
        return pgf.Expr(fun, [fix_integers_in_Extract(arg)
                              for arg in args if not str(arg).isdigit()])
    else:
        return tree


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


def adjust_tree_in_Extract(en, de, ty):
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


