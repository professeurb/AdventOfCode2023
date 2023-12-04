from collections import defaultdict


def process_bag(str):
    d = defaultdict(int)
    for colored_cubes in str.split(", "):
        [num_str, color] = colored_cubes.split(" ")
        assert color not in d
        d[color] = int(num_str)
    return d


def process_line(str):
    [header, bags_str] = str.split(": ")
    id = int(header.split(" ")[1])
    bags = [process_bag(bag) for bag in bags_str.split("; ")]
    return id, bags


def is_possible(bags):
    for bag in bags:
        if bag["red"] > 12 or bag["green"] > 13 or bag["blue"] > 14:
            return False
    return True


def power(bag):
    d = defaultdict(int)
    for bag in bags:
        for color in bag:
            d[color] = max(d[color], bag[color])
    return d["red"] * d["green"] * d["blue"]


with open("input02.txt", "r") as file:
    sum1 = 0
    sum2 = 0
    for line in file:
        # Pesky endline
        id, bags = process_line(line[:-1])
        if is_possible(bags):
            sum1 += id
        sum2 += power(bags)
    print(f"Part one: {sum1}")
    print(f"Part two: {sum2}")
