#!/usr/bin/env python3

import sys
import pgf
import graphviz
import networkx


def get_fun_data(pgf_file):
    gr = pgf.readPGF(pgf_file)
    return {f: gr.functionType(f) for f in gr.functions}


def analyse_type(typ):
    typs = str(typ).split(' -> ')  ## hack
    argcats = typs[:-1]
    valcat = typs[-1]
    return str(typ), argcats, valcat


def fun_signatures(fundata):
    dict = {}
    for fun, typ in fundata.items():
        styp, args, val = analyse_type(typ)
        dict[val] = dict.get(val, [])
        dict[val].append((fun, styp))
    return dict
    


def fun_edges(fun, typ):
    typs = str(typ).split(' -> ')  ## hack
    _, argcats, valcat = analyse_type(typ)
    return [(fun + '#' + str(i), (arg, valcat))
              for (arg, i) in zip(argcats, range(1, len(typs)))]


def visualize_edges(edges, withfuns=False):
    print('digraph G{')
    if not withfuns:
        edges = {('', av) for _, av in edges}
    for (f, (a, v)) in edges:
        print('  ' + a + ' -> ' + v + '[ label = "' + f + '" ] ;')
    print('}')


def main():
    if sys.argv[1:]:
        file = sys.argv[1]
    else:
        print('usage: analyse_grammar.py <file.pgf> withfuns?')
        return
    fundata = get_fun_data(file)
    edges = []
    for f in fundata:
        edges.extend(fun_edges(f, fundata[f]))
        
    mode = 'nofuns'
    if sys.argv[2:]:
        mode = sys.argv[2]
    match mode:
        case 'withfuns':
            visualize_edges(edges, True)
        case 'funsigs':
           sigs = fun_signatures(fundata)
           for (cat, funs) in sigs.items():
               print(cat)
               for (fun, typ) in funs:
                   print(' ', fun, ':', typ)
        case _:
            visualize_edges(edges, True)

main()

