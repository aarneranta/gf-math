import json

with open('stex.json') as file:
    mathhub = json.load(file)

print(len(mathhub))

with open('en_de.tsv') as file:
    de_set = set()
    for line in file:
        de_set.add(line.split('\t')[2].strip())
        

for v in mathhub.values():
    for k in v:
#        print(k, v[k])
        if k == 'de':
            for s in v[k]:
                m = 'YES' if s.strip() in de_set else 'NO'
                print(m, s)


