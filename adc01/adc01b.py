# https://docs.python.org/3/howto/regex.html

import re

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

dict1 = {str(d): d for d in range(10)}
dict2 = dict1 | {digits[i]: i for i in range(len(digits))}


def regex_from_dict(d):
    return re.compile("|".join(k for k in d))


reg1 = regex_from_dict(dict1)
reg2 = regex_from_dict(dict2)



def get_digits(d, r, s):
    return [d[m.group()] for m in r.finditer(s)]


sum1 = 0
sum2 = 0

with open("input01.txt", "r") as f:
    for line in f:
        digits1 = get_digits(dict1, reg1, line)
        digits2 = get_digits(dict2, reg2, line)
        sum1 += 10 * digits1[0] + digits1[-1]
        sum2 += 10 * digits2[0] + digits2[-1]

print("Part One:", sum1)
print("Part Two:", sum2)
