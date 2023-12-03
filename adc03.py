import re

digit_reg = re.compile("\\d+")
symbol_reg = re.compile("[^\\.\\d]")


numbers = {}


def get_digits(ln, str):
    for m in digit_reg.finditer(str):
        numbers[(ln, m.span())] = int(m.group())


symbols = {}


def get_symbols(ln, str):
    for m in symbol_reg.finditer(str):
        symbols[(ln, m.start())] = m.group()


def surround(ln, a, b):
    lst = []

    def test(x, y):
        if (x, y) in symbols:
            lst.append((x, y))

    test(ln, a - 1)
    test(ln, b)
    for c in range(a - 1, b + 1):
        test(ln - 1, c)
        test(ln + 1, c)
    return lst


with open("input03.txt", "r") as file:
    for ln, line in enumerate(file):
        line = line[:-1]
        get_digits(ln, line)
        get_symbols(ln, line)
    sum1 = 0
    for k in numbers:
        if surround(k[0], k[1][0], k[1][1]):
            sum1 += numbers[k]
    print("Part one:", sum1)
    sum2 = 0
    gears = {k: [] for k in symbols if symbols[k] == "*"}
    for k in numbers:
        for s in surround(k[0], k[1][0], k[1][1]):
            if s in gears:
                gears[s].append(numbers[k])
    sum2 = 0
    for g in gears:
        if len(gears[g]) == 2:
            sum2 += gears[g][0] * gears[g][1]
    print("Part two:", sum2)
