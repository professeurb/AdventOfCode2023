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


start_plat = read_platform()

print(f"Part one: {north_load(start_plat)}")


def transform_platform(plat):
    tilt_north(plat)
    tilt_west(plat)
    tilt_south(plat)
    tilt_east(plat)


# Brent's algorithm


plat1 = copy_platform(start_plat)
plat2 = copy_platform(start_plat)


# We work inplace
def copy_platform_to(plat1, plat2):
    for i in range(len(plat1)):
        for j in range(len(plat1[0])):
            plat2[i][j] = plat1[i][j]


transform_platform(plat2)

lam = 1
pow = 1

loads = {}

while plat1 != plat2:
    if lam == pow:
        pow *= 2
        lam = 0
        copy_platform_to(plat2, plat1)
    lam += 1
    transform_platform(plat2)
    loads[lam + pow] = north_load(plat2)

copy_platform_to(start_plat, plat1)
copy_platform_to(start_plat, plat2)

for _ in range(lam):
    transform_platform(plat2)

mu = 0
while plat1 != plat2:
    transform_platform(plat1)
    transform_platform(plat2)
    mu += 1

for _ in range((1_000_000_000 - mu) % lam):
    transform_platform(plat1)

print(f"Part two: {north_load(plat1)}")
