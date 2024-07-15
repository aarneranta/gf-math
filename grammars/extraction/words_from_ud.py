import sys

UDFILE = 'qde-words.tsv'
UNKFILE = 'wde.tmp'

def build_wdict(udfile):
    with open(udfile) as file:
        wdict = {}
        for line in file:
            ws = line.split('\t')
            wdict[ws[0]] = tuple(ws[1:])
    return wdict


def app(fun, args):
    return ' '.join([fun, '"' + args[0] + '"'] + args[1:])


def param_dict(feats):
    if '=' not in feats:
        return {}
    feats = [tuple(fv.split('=')) for fv in feats.split('|')]
    return {f: v for f, v in feats}


def restore_lemma(word, lemma):
    "Ger UDPipe truncates lemmas of compound words, trying to restore them"
    if word.endswith(lemma[1:]):
        return word
    else:
        return lemma

    
def gf_lin(word, udanalysis):
    lemma, pos, feats = udanalysis
    fdict = param_dict(feats)
    lemma = restore_lemma(word, lemma)
    match pos:
        case 'NOUN':
            gender = fdict.get('Gender', 'Neutr')
            return lemma, app('mkN', [lemma, gender]), 'N'
        case 'PROPN':
            gender = fdict.get('Gender', 'Masc')
            return lemma, app('mkPN', [lemma, gender]), 'PN'
        case 'ADJ':
            return lemma, app('mkA', [lemma]), 'A'
        case 'ADV':
            return lemma, app('mkAdv', [lemma]), 'Adv'
        case 'VERB':
            return lemma, app('mkV', [lemma]), 'V'
        case _:
            return ' '.join(['UNK', lemma, pos]), ''


def analyse_unknowns(wdict, unkfile):
    with open(unkfile) as file:
        for line in file:
            word = line.strip()
            if word in wdict:
                analysis = gf_lin(word, wdict[word])
                if analysis[-1]:
                    cat = analysis[-1]
                    fun = mk_gf_fun(analysis[0] + '_' + cat)
                    lin = analysis[1]
                    print('fun', fun, ':', cat, ';')
                    print('lin', fun, '=', lin, ';')


def mk_gf_fun(s):
    if all(c.isdigit() or c.isalpha() or c in "_'" for c in s):
        return s
    else:
        return "'" + s + "'"
        

def main():
    wdict = build_wdict(UDFILE)
    analyse_unknowns(wdict, UNKFILE)

main()




