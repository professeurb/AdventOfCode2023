import re

first = re.compile("^[^\\d]*\\d")
last = re.compile("\\d[^\\d]*$")

digits = [
    "zero",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
]
digiters = [re.compile(d) for d in digits]


def first_digit(s):
    return int(s[first.search(s).end() - 1])


def last_digit(s):
    return int(s[last.search(s).start()])


def modify_string(s):
    l = list(s)
    for i in range(10):
        for m in digiters[i].finditer(s):
            l[m.start()] = str(i)
    return "".join(l)


sum1 = 0
sum2 = 0

with open("input01.txt", "r") as f:
    for line in f:
        sum1 += 10 * first_digit(line) + last_digit(line)
        line2 = modify_string(line)
        sum2 += 10 * first_digit(line2) + last_digit(line2)

print("Part One:", sum1)
print("Part Two:", sum2)
