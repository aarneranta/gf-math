import sys

#UDFILE = 'qde-words.tsv'
#UNKFILE = 'wde.tmp'
#MORPHOFILE = 'defuns.tmp'  # generated by pg -funs from MorphoDictGer
#LANG = 'Ger'

UDFILE = 'qen-words.tsv'
UNKFILE = 'wen.tmp'
MORPHOFILE = 'enfuns.tmp'  # generated by pg -funs from MorphoDictGer
LANG = 'Eng'

def build_wdict(udfile):
    with open(udfile) as file:
        wdict = {}
        for line in file:
            ws = line.split('\t')
            wdict[ws[0]] = tuple(ws[1:])
    return wdict


def quote(s):
    return '"' + s + '"'


def app(fun, args):
    return ' '.join([fun, quote(args[0])] + args[1:])


def param_dict(feats):
    if '=' not in feats:
        return {}
    feats = [tuple(fv.split('=')) for fv in feats.split('|')]
    return {f: v for f, v in feats}


def gf_lin(word, udanalysis, lang=LANG):
    lemma, pos, feats = udanalysis
    fdict = param_dict(feats)

    ugender = fdict.get('Gender', 'Neut')
    genders = {'Masc': 'masculine', 'Fem': 'feminine', 'Neut': 'neuter'}
    gender = genders.get(ugender, 'neuter')
    number = fdict.get('Number', 'Sing')

    if lang == 'Ger':
      match pos:
        case 'NOUN':
            if number == 'Plur':
                return lemma, app('mkN', [lemma, quote(word), gender]), 'N'
            else:
                return lemma, app('mkN', [lemma, gender]), 'N'
        case 'PROPN':
            return lemma, app('mkPN', [lemma, gender]), 'PN'
        case 'ADJ':
            if len(lemma) > 6 and lemma[-2:] in {'er', 'es'}:
                lemma = lemma[:-2]
            return lemma, app('mkA', [lemma]), 'A'
        case 'ADV':
            return lemma, app('mkAdv', [lemma]), 'Adv'
        case 'VERB':
            return lemma, app('mkV', [lemma]), 'V'
        case _:
            return ' '.join(['UNK', lemma, pos]), ''
    else:
      match pos:
        case 'NOUN':
            if number == 'Plur':
                return lemma, app('mkN', [lemma, quote(word)]), 'N'
            else:
                return lemma, app('mkN', [lemma]), 'N'
        case 'PROPN':
            return lemma, app('mkPN', [lemma]), 'PN'
        case 'ADJ':
            return lemma, app('mkA', [lemma]), 'A'
        case 'ADV':
            return lemma, app('mkAdv', [lemma]), 'Adv'
        case 'VERB':
            return lemma, app('mkV', [lemma]), 'V'
        case _:
            return ' '.join(['UNK', lemma, pos]), ''


def analyse_unknowns(wdict, moset, unkfile):
    with open(unkfile) as file:
        for line in file:
            word = line.strip()
            if word in wdict:
                analysis = gf_lin(word, wdict[word], LANG)
                if analysis[-1]:
                    cat = analysis[-1]
                    fun = mk_gf_fun(analysis[0] + '_' + cat)
                    lin = analysis[1]
                    if fun not in moset:
                        print('fun', fun, ':', cat, ';')
                        print('lin', fun, '=', lin, ';')


def mk_gf_fun(s):
    if all(c.isdigit() or c.isalpha() or c in "_'" for c in s):
        return s
    else:
        return "'" + s + "'"
        

def build_moset(file):
    with open(file) as funs:
        return {line.split()[0] for line in funs if line.split()}

    
def main():
    wdict = build_wdict(UDFILE)
    moset = build_moset(MORPHOFILE)
    analyse_unknowns(wdict, moset, UNKFILE)

main()




