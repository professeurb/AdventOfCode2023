# file = "test14.txt"
file = "input14.txt"


def read_platform():
    platform = []

    with open(file, "r") as input:
        for line in input:
            platform.append(list(line[:-1]))
    return platform


def copy_platform(platform):
    return [line[:] for line in platform]


def print_platform(platform):
    for line in platform:
        print("".join(line))


def tilt_north(platform):
    for cn in range(len(platform[0])):
        p = 0
        for ln in range(len(platform)):
            if platform[ln][cn] == "#":
                p = ln + 1
            elif platform[ln][cn] == "O":
                platform[ln][cn] = "."
                platform[p][cn] = "O"
                p += 1


def tilt_west(platform):
    for ln in range(len(platform)):
        p = 0
        for cn in range(len(platform[0])):
            if platform[ln][cn] == "#":
                p = cn + 1
            elif platform[ln][cn] == "O":
                platform[ln][cn] = "."
                platform[ln][p] = "O"
                p += 1


def tilt_south(platform):
    for cn in range(len(platform[0])):
        p = len(platform) - 1
        for ln in range(len(platform) - 1, -1, -1):
            if platform[ln][cn] == "#":
                p = ln - 1
            elif platform[ln][cn] == "O":
                platform[ln][cn] = "."
                platform[p][cn] = "O"
                p -= 1


def tilt_east(platform):
    for ln in range(len(platform)):
        p = len(platform[0]) - 1
        for cn in range(len(platform[0]) - 1, -1, -1):
            if platform[ln][cn] == "#":
                p = cn - 1
            elif platform[ln][cn] == "O":
                platform[ln][cn] = "."
                platform[ln][p] = "O"
                p -= 1


def north_load(platform):
    load = 0
    for ln in range(len(platform)):
        for c in platform[ln]:
            if c == "O":
                load += len(platform) - ln
    return load


platform = read_platform()
tilt_north(platform)

print(f"Part one: {north_load(platform)}")

platform = read_platform()  # start over


def transform_platform(plat):
    tilt_north(plat)
    tilt_west(plat)
    tilt_south(plat)
    tilt_east(plat)


table = {}
loads = {}

for i in range(1, 1_000_000_001):
    transform_platform(platform)
    hash = "".join(["".join(line) for line in platform])
    if hash in table:
        mu = table[hash]
        lam = i - mu
        break
    table[hash] = i
    loads[i] = north_load(platform)

platform = read_platform()  # start over, again

for _ in range((1_000_000_000 - mu) % lam + mu):
    transform_platform(platform)

print(f"Part two: {north_load(platform)}")

# print(f"Part two: {loads[(1_000_000_000 - mu) % lam + mu]}")
