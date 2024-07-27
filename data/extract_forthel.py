import sys

forthel_lines = []

in_forthel = False

for line in sys.stdin:
    if line.strip() == '\\begin{forthel}':
        in_forthel = True
    elif line.strip() == '\\end{forthel}':
        in_forthel = False
    elif line.startswith('%'):
        pass
    elif in_forthel:
        forthel_lines.append(line)
    else:
        pass



