import sys
from gf_utils import *

#UDFILE = 'qde-words.tsv'
#UNKFILE = 'wde.tmp'
#MORPHOFILE = 'defuns.tmp'  # generated by pg -funs from MorphoDictGer
#LANG = 'Ger'

UDFILE = 'qen-words.tsv'  ## should be a conllu file
UNKFILE = 'wen.tmp'
MORPHOFILE = 'enfuns.tmp'  # generated by pg -funs from MorphoDictGer
LANG = 'Eng'

def build_wdict(udfile):
    with open(udfile) as file:
        wdict = {}
        for line in file:
            ws = line.split('\t')
            if len(ws) > 5:
                wdict[ws[1]] = (ws[2], ws[3], ws[5])
    return wdict


def param_dict(feats):
    if '=' not in feats:
        return {}
    feats = [tuple(fv.split('=')) for fv in feats.split('|')]
    return {f: v for f, v in feats}


def gf_lin(word, udanalysis, lang=LANG):
    lemma, pos, feats = udanalysis
    fdict = param_dict(feats)

    ugender = fdict.get('Gender', 'Masc')   ## Neut for Ger could be better
    genders = {'Masc': 'masculine', 'Fem': 'feminine', 'Neut': 'neuter'}
    gender = genders.get(ugender, 'masculine')  ## neuter for Ger could be better
    if lang=='Swe':
        ugender = fdict.get('Gender', 'Com')
        genders = {'Com': 'utrum', 'Neut': 'neutrum'}
        gender = genders.get(ugender, 'utrum')
    number = fdict.get('Number', 'Sing')
    casus = fdict.get('Case', 'Nom')

    if lang in ['Fre', 'Ger', 'Ita', 'Por', 'Spa', 'Swe']:
      match pos:
        case 'NOUN':
            if number == 'Plur':
                if lang=='Swe':
                    return lemma, app('mkN', [lemma, quote(word)]), 'N'  ## TODO use gender
                else:
                    return lemma, app('mkN', [lemma, quote(word), gender]), 'N'
            else:
                return lemma, app('mkN', [lemma, gender]), 'N'
        case 'PROPN':
            return lemma, app('mkPN', [lemma, gender]), 'PN'
        case 'ADJ' if lang == 'Ger':
            if len(lemma) > 6 and lemma[-2:] in {'er', 'es'}:
                lemma = lemma[:-2]
            return lemma, app('mkA', [lemma]), 'A'
        case 'ADJ':
            return lemma, app('mkA', [lemma]), 'A'        
        case 'ADV':
            return lemma, app('mkAdv', [lemma]), 'Adv'
        case 'VERB' if lang == 'Ger' and lemma[-1] == 'n':
            return lemma, app('mkV', [lemma]), 'V'
        case 'VERB' if lang == 'Fre' and lemma[-2] in ['er', 'ir', 're']:
            return lemma, app('mkV', [lemma]), 'V'
        case _ if lemma[0].isupper():
            return lemma, app('mkPN', [lemma, gender]), 'PN'
        case _:
            return ' '.join(['UNK', lemma, pos]), ''

    elif lang == 'Fin':
      lemma = lemma.replace('#', '')  ## TODO: use compound boundaries
      word = word.replace('"', '')
      lemma = lemma.replace('"', '')

      match pos:
        case 'NOUN':          
            if number == 'Sing' and casus == 'Gen':
                return lemma, app('mkN', [lemma, quote(word)]), 'N'
            else:
                return lemma, app('mkN', [lemma]), 'N'
        case 'PROPN':
            return lemma, app('mkPN', [lemma]), 'PN'
        case 'ADJ':
            return lemma, app('mkA', [lemma]), 'A'
        case 'ADV':
            return lemma, app('mkAdv', [lemma]), 'Adv'
##        case 'VERB' if lemma[-1] in ['a', 'ä']:
##            return lemma, app('mkV', [lemma]), 'V'
        case _ if lemma[0].isupper():
            return 
        case _:
            return ' '.join(['UNK', lemma, pos]), ''


    # add your language here to get better quality!

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
        case _ if lemma[0].isupper():
            return 
        case _:
            return ' '.join(['UNK', lemma, pos]), ''


def analyse_unknowns(wdict, moset, unkfile, lang=LANG):
    absrules = []
    cncrules = []
    foundfuns = set()
    with open(unkfile) as file:
        for word in [line.strip() for line in file]:
            if word in wdict:
                analysis = gf_lin(word, wdict[word], lang)
                if analysis[-1]:
                    cat = analysis[-1]
                    fun = mk_fun(analysis[0] + '_' + cat)
                    if fun not in foundfuns:
                        foundfuns.add(fun)
                        lin = analysis[1]
                        if fun not in moset:
                            absrules.append((fun, cat))
                            cncrules.append((fun, lin))
    return absrules, cncrules


def build_moset(file):
    with open(file) as funs:
        return {line.split()[0] for line in funs if line.split()}

    
def words_main(udfile, morphofile, unkfile, lang=LANG):
    wdict = build_wdict(udfile)
    moset = build_moset(morphofile)
    return analyse_unknowns(wdict, moset, unkfile, lang)


if __name__ == '__main__':
    ars, crs = words_main(UDFILE, MORPHOFILE, UNKFILE)
    for i in range(len(ars)):
        print(ars[i])
        print(crs[1])




