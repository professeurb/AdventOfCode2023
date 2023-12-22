# file = "test09.txt"
file = "input09.txt"


def delta(lst):
    return [lst[i + 1] - lst[i] for i in range(len(lst) - 1)]


def next_term(lst):
    if lst:
        return lst[-1] + next_term(delta(lst))
    return 0


def previous_term(lst):
    if lst:
        return lst[0] - previous_term(delta(lst))
    return 0


data = []

with open(file, "r") as input:
    for line in input:
        series = [int(v) for v in line[:-1].split(" ")]
        data.append(series)

print(f"Part one: {sum(next_term(s) for s in data)}")
print(f"Part two: {sum(previous_term(s) for s in data)}")
