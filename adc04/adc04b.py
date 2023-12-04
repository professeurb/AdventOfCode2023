from collections import defaultdict


def process_line(str):
    colon = str.index(":")
    pipe = str.index("|")
    wins = set([int(s) for s in str[colon + 1 : pipe].split()])
    hand = set([int(s) for s in str[pipe + 1 :].split()])
    return len(wins & hand)


with open("input04.txt", "r") as file:
    sum1 = 0
    sum2 = 0
    copies = defaultdict(lambda: 1)
    for pos, line in enumerate(file):
        score = process_line(line[:-1])
        sum1 += int(2 ** (score - 1))
        sum2 += copies[pos]
        for i in range(score):
            copies[pos + i + 1] += copies[pos]
    print(f"Part one: {sum1}")
    print(f"Part two: {sum2}")
