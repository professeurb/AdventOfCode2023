def process_line(str):
    colon = str.index(":")
    pipe = str.index("|")
    wins = set([int(s) for s in str[colon + 1 : pipe].split()])
    hand = set([int(s) for s in str[pipe + 1 :].split()])
    return wins, hand


def compute_score(card):
    winning, hand = card
    score = 0
    for v in hand:
        if v in winning:
            score += 1
    return score


with open("input04.txt", "r") as file:
    cards = []
    for pos, line in enumerate(file):
        cards.append(process_line(line[:-1]))
    sum1 = 0
    sum2 = 0
    copies = [1] * len(cards)
    for i in range(len(cards)):
        score = compute_score(cards[i])
        if score >= 1:
            sum1 += 2 ** (score - 1)
        sum2 += copies[i]
        for j in range(score):
            copies[i + j + 1] += copies[i]
    print(f"Part one: {sum1}")
    print(f"Part two: {sum2}")
