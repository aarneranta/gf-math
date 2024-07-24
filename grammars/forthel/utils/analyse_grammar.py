#!/usr/bin/env python3

import sys
import pgf
import graphviz
import networkx


def get_fun_data(pgf_file):
    gr = pgf.readPGF(pgf_file)
    return {f: gr.functionType(f) for f in gr.functions}


def fun_edges(fun, typ):
    typs = str(typ).split(' -> ')  ## hack
    argcats = typs[:-1]
    valcat = typs[-1]
    return [(fun + '#' + str(i), (arg, valcat))
              for (arg, i) in zip(argcats, range(1, len(typs)))]


def visualize_edges(edges, withlabels=False):
    print('digraph G{')
    if not withlabels:
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
    withlabels = sys.argv[2:]
    visualize_edges(edges, withlabels)

main()

