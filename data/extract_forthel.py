import sys

forthel_segments = []
segment = []

in_forthel = False

for line in sys.stdin:
    line = line.strip()
    if line == '\\begin{forthel}':
        in_forthel = True
    elif line == '\\end{forthel}':
        in_forthel = False
        forthel_segments.append(' '.join(segment))
        segment = []
    elif line.startswith('%'):   ## to be kept
        pass
    elif line.startswith('[timelimit'):  ## to be kept
        pass
    elif line.startswith('\\begin{'):   ## to be kept
        pass
    elif line.startswith('\\end{'):  ## to be kept
        pass
    elif in_forthel:
        segment.append(line)
    else:
        pass

def indexed(i):
    return '$\\INDEXEDTERM{' + str(i) + '}$' 
    
math_dict = {}
counter = 1
analysed_segments = []

for segment in forthel_segments:
    asegment = []
    segment = segment.strip()
    math_in = 0 if segment and segment[0] == '$' else 1
    parts = [s for part in segment.split('$$')
               for s in part.split('$')]
    for i in range(len(parts)):
        if math_in and i % 2 == 1:
            asegment.append(indexed(counter))
            math_dict[counter] = parts[i]
            counter += 1
        else:
            asegment.append(parts[i])
    asegment = ' '.join(asegment)
    asegment = asegment.split('.')
    asegment = [s + '.' for s in asegment]
    analysed_segments.extend(asegment)


for a in analysed_segments:
    print(a)

for k in math_dict:
    pass
#    print(k, math_dict[k])
    


