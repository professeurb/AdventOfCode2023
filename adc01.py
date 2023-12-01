digit_names = [
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
dict2 = dict1 | {digit_names[d]: d for d in range(len(digit_names))}


def factor_at(factor, text, pos):
    return text[pos : pos + len(factor)] == factor


def list_digits(d, s):
    digits = []
    for i in range(len(s)):
        for k in d:
            if factor_at(k, s, i):
                digits.append(d[k])
    return digits


sum1 = 0
sum2 = 0

with open("input01.txt", "r") as f:
    for line in f:
        l1 = list_digits(dict1, line)
        sum1 += 10 * l1[0] + l1[-1]
        l2 = list_digits(dict2, line)
        sum2 += 10 * l2[0] + l2[-1]

print("Part One:", sum1)
print("Part Two:", sum2)
