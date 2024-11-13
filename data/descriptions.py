import json

DATAFILE = 'qs.jsonl'

with open(DATAFILE) as file:
    for line in file:
        data = json.loads(line)
        qids = data.get('entities', {})
        labels = (list(qids.keys())[0], list(qids.values())[0].get('labels', {}),
                  list(qids.values())[0].get('descriptions', {}))
        labdict = {labels[0]: {
            'labels': {lab: labels[1][lab]['value'] for lab in labels[1]}},
            'descriptions': {lab: labels[2][lab]['value'] for lab in labels[2]}}
        print(json.dumps(labdict, ensure_ascii=False))


# universities, labels, and descriptions
# https://w.wiki/BxW6
"""
select ?university ?label ?description (lang(?description) as ?language) {
  ?university wdt:P31/wdt:P279* wd:Q3918 .
#  ?university wdt:P17 wd:Q30 .
  ?university rdfs:label ?label .
  ?university schema:description ?description .
  filter (lang(?label) = lang(?description))
} limit 100000


types: https://w.wiki/BxWd
"""
     
