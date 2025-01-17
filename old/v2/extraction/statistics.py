import sys
import json

OLD_SYNOPSIS_FILE = 'out/math_terms_synopsis.json'  # original structure, to change in source
SYNOPSIS_FILE = 'out/synopsis.json'  # obtained by flag mend to this file
NOWIKI = 'NOWIKILABEL'

def n_terms(synopsis):
    return len(synopsis)


def languages(synopsis):
    return list(synopsis.values())[0]['langs'].keys()


def has_label(synopsis, qid, lang):
    return not synopsis[qid]['langs'][lang]['str'].upper().startswith(NOWIKI)


def has_parse(synopsis, qid, lang):
    return not synopsis[qid]['langs'][lang]['lin'].startswith('variants {}')

def coverage(synopsis):
    def raws(lang):
        return len([q for q in synopsis if has_label(synopsis, q, lang)])
    def parseds(lang):
        return len([q for q in synopsis if has_parse(synopsis, q, lang)])
    nterms = n_terms(synopsis)
    return {lang: {
        'raw': raws(lang),
        'of_terms': 100 * raws(lang) // nterms,
        'parsed': parseds(lang),
        'of_labels': 100 * parseds(lang) // raws(lang)
         }
     for lang in languages(synopsis)
     }

def coverage_table(table):
    return '\n'.join(
        ['\t'.join([lang] + [str(k) for k in table[lang].values()])
           for lang in table])


if sys.argv[1:] == ['mend']:
    with open(OLD_SYNOPSIS_FILE) as file:
        synopsis = json.load(file)

    synop = {q : {
        'cat': synopsis[q]['cat'],
        'fun': synopsis[q]['fun'],
        'sourcelang': synopsis[q]['lang'],
        'status': synopsis[q]['status'],
        'langs': {lang: v for lang, v in synopsis[q].items()
                  if lang not in {'cat', 'fun', 'lang', 'status'}}
        } for q in synopsis}

    with open(SYNOPSIS_FILE, 'w') as outfile:
        json.dump(synop, outfile, ensure_ascii=False, indent=2)
else:
    with open(SYNOPSIS_FILE) as file:
        synopsis = json.load(file)
    print('terms:', n_terms(synopsis))
    print('coverage:')
    print(coverage_table(coverage(synopsis)))

