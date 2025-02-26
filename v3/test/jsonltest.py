import json

with open('exx.jsonl') as file:
    data = [json.loads(line) for line in file]

print(len(data), 'formulas')
print(sum([len(v) for d in data for k, v in d.items() if k == 'InformathEng']), 'sentences')


         
