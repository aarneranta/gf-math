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


