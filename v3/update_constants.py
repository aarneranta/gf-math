import json

BASE = 'BaseConstants'

LANGUAGES = ['Eng', 'Fre', 'Swe']

def base(suffix):
    return BASE + suffix

def from_dk_line(line):
    ws = line.split()
    if not ws or ws[0] == '(;':
        return None
    elif ws[0] == 'def':
        return from_dk_line(line[4:])
    elif ws[1] == ':':
        return {'fun': ws[0],
                'jmt': line.strip()
                }
    else:
        return None

    
def from_hs_line(line):
    if line.strip().startswith('('):
        ws = line.split()
        return {'dkfun': ws[0][2:-2], 'cat': ws[1][1:-2], 'gffun': ws[2][1:-3]}
    else:
        return None

    
def from_gf_line(line):
    parts = line.split('=')
    if parts[1:] and parts[0].split():
        return {'fun': parts[0].split()[-1], 'lin': line.strip()}
    else:
        return None

    
def collect(source=BASE):
    data = {}
    data['Type'] = {'dk': {'fun': 'Type', 'jmt': '(; Type is built-in ;)'}}
    with open(base('.dk')) as file:
        for (line, i) in zip(file, range(10000, 20000)):
            if dk := from_dk_line(line):
                id = dk['fun']  # + '_' + str(i) ## expect dk funs to be keys
                data[id] = {}
                data[id]['source'] = source
                data[id]['dedukti'] = dk
    with open('Constants.hs') as file:
        for line in file:
            if hs := from_hs_line(line):
                data[hs['dkfun']]['gf'] = {k: v for (k, v) in hs.items() if k != 'dkfun'}
    gf_to_dk = {v['gf']['gffun']: f for f, v in data.items() if 'gf' in v}
    for lang in LANGUAGES:
        with open('grammars/'+base(lang+'.gf')) as file:
            for line in file:
                if gf := from_gf_line(line):
                    if dk := gf_to_dk.get(gf['fun'], None):
                        data[dk]['gf'][lang] = gf['lin']
    return data


if __name__ == '__main__':
    data = collect()
    j = json.dumps(data, indent=2, ensure_ascii= False)
    print(j)

