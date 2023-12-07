from collections import defaultdict

# file = "test07.txt"
file = "input07.txt"

hands = []

with open(file, "r") as data:
    for line in data:
        hands.append((line[:5], int(line[6:])))


def occurences(s):
    d = defaultdict(int)
    for c in s:
        d[c] += 1
    return d


def score(order, hand):
    scr = 0
    for c in hand:
        scr = len(order) * scr + order.index(c)
    return scr


game1 = [
    (
        sorted(occurences(hand).values(), reverse=True),
        score("23456789TJQKA", hand),
        hand,
        bid,
    )
    for (hand, bid) in hands
]
game1.sort()

score1 = sum((i + 1) * bid for (i, (_, _, _, bid)) in enumerate(game1))
print(f"Part one: {score1}")


def hand_value_with_jokers(hand):
    d = occurences(hand)
    j = d["J"]
    del d["J"]
    occs = sorted(d.values(), reverse=True)
    if occs:
        occs[0] += j
    else:
        occs = [j]
    return occs


game2 = [
    (
        hand_value_with_jokers(hand),
        score("J23456789TQKA", hand),
        hand,
        bid,
    )
    for (hand, bid) in hands
]
game2.sort()
score2 = sum((i + 1) * bid for (i, (_, _, _, bid)) in enumerate(game2))
print(f"Part two: {score2}")
