import json

ISABELLE_TRAIN = 'mma-dataset/isabelle_train.jsonl'
ISABELLE_VAL = 'mma-dataset/isabelle_val.jsonl'
LEAN_TEST = 'mma-dataset/lean_test.jsonl'
LEAN_TEST = 'mma-dataset/lean_test.jsonl'
LEAN_TRAIN = 'mma-dataset/lean_train.jsonl'
LEAN_VAL = 'mma-dataset/lean_val.jsonl'


def get_data(filename):
    with open(filename) as file:
        data = [json.loads(line) for line in file]
    return data


for ex in get_data(LEAN_TEST):
#    print('#ENG', ex['input'])
#    print('#LEAN', ex['output'])
    print(ex['output'], 'sorry')



