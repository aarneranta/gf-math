def mk_fun(s):
    if (s[0].isalpha() and
        all(ord(c)<256 and (c.isdigit() or c.isalpha() or c in "_'")
            for c in s)):
        return s
    else:
        return "'" + s + "'"

    
def quote(s):
    return '"' + s + '"'


def app(fun, args):
    return ' '.join([fun, quote(args[0])] + args[1:])


def mk_lin(oper, words, params):
    return ' '.join([oper] + [quote(w) for w in words] + params) 


def mk_cat_rule(cat):
    return ' '.join(['cat', cat, ';\n'])


def mk_fun_rule(fun, cat):
    return ' '.join(['fun', fun, ':', cat, ';\n'])


def mk_lin_rule(fun, lin):
    return ' '.join(['lin', fun, '=', lin, ';\n'])


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
            if mdict[qid]['status']:
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
                if mdict[qid][lang]['status']:
                    cncfile.write(
                        mk_lin_rule(mdict[qid]['fun'], mdict[qid][lang]['lin']))
                else:
                    print('-- BAD STATUS', qid)
            cncfile.write('}\n')
            print('wrote file', cncname + '.gf')

        
