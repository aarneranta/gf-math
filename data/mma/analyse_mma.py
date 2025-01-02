import sys
import json

ISABELLE_TRAIN = 'mma-dataset/isabelle_train.jsonl'
ISABELLE_VAL = 'mma-dataset/isabelle_val.jsonl'
LEAN_TEST = 'mma-dataset/lean_test.jsonl'
LEAN_TEST = 'mma-dataset/lean_test.jsonl'
LEAN_TRAIN = 'mma-dataset/lean_train.jsonl'
LEAN_VAL = 'mma-dataset/lean_val.jsonl'

MODE = 'split'
if sys.argv[1:]:
    MODE = sys.argv[1]

DATASET = LEAN_TRAIN
if sys.argv[2:]:
    DATASET = sys.argv[2]

TOPMOST = 1000
    

def get_data(filename):
    with open(filename) as file:
        data = [json.loads(line) for line in file]
    return data


def tokenize(s):
    cs = []
    for c in s:
        if c in '.,:()[]{}':
            cs.extend([' ', c, ' '])
        else:
            cs.append(c)
    return [w.lower() for w in ''.join(cs).split()]


def print_token_statistics(tokens):
        toks = {}
        for t in tokens:
            toks[t] = toks.get(t, 0) + 1
        tok_freqs = sorted(list(toks.items()), key=lambda t: -t[1])
        for t, n in tok_freqs[:TOPMOST]:
            print(t, n)
        print(len(toks))


def wrap_input(s):
    input_wrap = ('Statement in natural language: ',
              ' Translate the statement in natural language to Lean:')
    pref, suff = [len(x) for x in input_wrap]
    return s[pref:-suff].replace('\n', '  ')


def wrap_output_lean(s):
    return s.replace('\n', '  ') + ' sorry'


def process(dataset, mode):
    data = get_data(dataset)
    if mode == 'words':
        print_token_statistics(t for ex in data for t in tokenize(ex['input']))

    elif mode == 'symbols':
        print_token_statistics(t for ex in data for t in tokenize(ex['output']))
        
    else:
        for ex in data:
            print(wrap_input(ex['input']))
            print(wrap_output_lean(ex['output']))



    
if __name__ == '__main__':
    process(DATASET, MODE)



