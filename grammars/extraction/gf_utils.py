def mk_fun(s):
    if all(ord(c)<256 and (c.isdigit() or c.isalpha() or c in "_'") for c in s):
        return s
    else:
        return "'" + s + "'"

    
def quote(s):
    return '"' + s + '"'

    
def mk_lin(oper, words, params):
    return ' '.join([oper] + [quote(w) for w in words] + params) 


def mk_fun_rule(fun, cat):
    return ' '.join(['fun', fun, ':', cat, ';\n'])


def mk_lin_rule(fun, lin):
    return ' '.join(['lin', fun, '=', lin, ';\n'])


def print_gf_files(absname, mdict):
    "dict format: qid: {'cat', 'fun', 'status', *lang: {'str', 'lin', 'status'}}"
    
    with open(absname + '.gf', 'w') as absfile:
        absfile.write(' '.join(['abstract', absname, '=', 'Cat', '**', '{\n']))
        absfile.write('cat Term ;\n')  ## TODO generalize
        for qid in mdict:
            if mdict[qid]['status']:
                absfile.write(mk_fun_rule(mdict[qid]['fun'], mdict[qid]['cat']))
        absfile.write('}\n')
        print('wrote file', absname + '.gf')
        
    for qid in mdict:
        langs = list(mdict[qid].keys())[3:]
        break  # get lang names from the first item

    for lang in langs:
        cncname = absname + lang
        with open(cncname + '.gf', 'w') as cncfile:
            cncfile.write(
                ' '.join(['concrete', cncname, 'of', absname, '=',
                          'Cat' + lang, '**', 'open',
                          'Extract' + lang,  ## TODO generalize
                          'in', '{\n']))
            cncfile.write('lincat Term = Utt ;\n')  ## TODO generalize
            for qid in mdict:
                if mdict[qid][lang]['status']:
                    cncfile.write(
                        mk_lin_rule(mdict[qid]['fun'], mdict[qid][lang]['lin']))
            cncfile.write('}\n')
            print('wrote file', cncname + '.gf')

        
