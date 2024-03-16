

# https://www.wikidata.org/wiki/Special:EntityData/Q18644475.json

import urllib.request
import json


# to start, direct this into qs.jsonl; not to be repeated
def get_wikidata_jsonl():
    with open('qs.tmp') as file:
        qids = [line.strip() for line in file]

    for qid in qids:
        try:
            with urllib.request.urlopen('https://www.wikidata.org/wiki/Special:EntityData/'+ qid +'.json') as url:
                data = json.load(url)
                print(json.dumps(data))
        except:
            pass


# then use the generated file to collect labels, direct to qid-lexicon.jsonl
def extract_lexicon():
##    lexicon = {}
    with open('qs.jsonl') as file:
        for line in file:
            dict = json.loads(line)
            qids = dict.get('entities', {})
#            print(list(qids.keys()))
            labels = (list(qids.keys())[0], list(qids.values())[0].get('labels', {}))
#            print(labels)
            labdict = {labels[0]: {lab: labels[1][lab]['value'] for lab in labels[1]}}
#            print(labdict)
##            lexicon[labels[0]] = {lab: labels[1][lab]['value'] for lab in labels[1]}
            print(json.dumps(labdict, ensure_ascii=False))
            


extract_lexicon()



    
