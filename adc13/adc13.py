file = "test13.txt"
# file = "input13.txt"

patterns = []

with open(file, "r") as input:
    pattern = []
    for line in input:
        line = line[:-1]
        if line == "":
            patterns.append(pattern)
            pattern = []
        else:
            pattern.append(line)
    else:
        patterns.append(pattern)


def check_horizontal(pattern):
    candidates = []
    for i in range(len(pattern) - 1):
        if pattern[i] == pattern[i + 1]:
            candidates.append(i)
    for cand in candidates:
        i = cand
        j = cand + 1
        while True:
            if i < 0 or j == len(pattern):
                return cand + 1
            if pattern[i] != pattern[j]:
                break
            i -= 1
            j += 1


def transpose(pattern):
    return [
        [pattern[i][j] for i in range(len(pattern))] for j in range(len(pattern[0]))
    ]


sum1 = 0
for pattern in patterns:
    r1 = check_horizontal(pattern)
    if r1:
        sum1 += 100 * r1
    else:
        sum1 += check_horizontal(transpose(pattern))

print(f"Part one: {sum1}")


def test_horizontal(pattern, position):
    smudges = 0
    i = position
    j = position + 1
    while True:
        if i < 0 or j == len(pattern):
            return smudges == 1
        for k in range(len(pattern[i])):
            if pattern[i][k] != pattern[j][k]:
                smudges += 1
        if smudges > 1:
            return False
        i -= 1
        j += 1


for i in range(len(patterns[0])):
    if test_horizontal(patterns[0], i):
        print(i)


def value(pattern):
    for i in range(len(pattern)):
        if test_horizontal(pattern, i):
            return 100 * (i + 1)
    pattern = transpose(pattern)
    for i in range(len(pattern)):
        if test_horizontal(pattern, i):
            return i + 1


sum2 = 0
for pattern in patterns:
    sum2 += value(pattern)


print(f"Part two: {sum2}")
