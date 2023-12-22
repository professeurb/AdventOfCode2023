# file = "test10.txt"
# file = "test10b.txt"
# file = "test10t.txt"
# file = "test10_5.txt"
file = "input10.txt"

grid = []

with open(file, "r") as input:
    for line in input:
        grid.append(list(line[:-1]))

height = len(grid)
width = len(grid[0])

left_set = set("-LF")
right_set = set("-7J")
down_set = set("|7F")
up_set = set("|LJ")


def start_position(grid):
    for i, line in enumerate(grid):
        if "S" in line:
            return (i, line.index("S"))


def replace_start(grid, pos):
    (i, j) = pos
    assert grid[i][j] == "S"
    curr = left_set | right_set | down_set | up_set
    if i == 0 or grid[i - 1][j] not in down_set:
        curr -= up_set
    if i == height - 1 or grid[i + 1][j] not in up_set:
        curr -= down_set
    if j == 0 or grid[i][j - 1] not in left_set:
        curr -= right_set
    if j == len(grid[i]) - 1 or grid[i][j + 1] not in right_set:
        curr -= left_set
    assert len(curr) == 1
    grid[i][j] = list(curr)[0]


start = start_position(grid)
replace_start(grid, start)

# Union-Find


def find(uf, k):
    v = uf[k]
    if isinstance(v, int):
        return k
    assert len(v) == 2
    w = find(uf, v)
    uf[k] = w
    return w


def union(uf, k1, k2):
    r1 = find(uf, k1)
    r2 = find(uf, k2)
    if r1 == r2:
        return
    if uf[r1] < uf[r2]:
        uf[r1] += uf[r2]
        uf[r2] = r1
    else:
        uf[r2] += uf[r1]
        uf[r1] = r2


uf = {(i, j): -1 for i in range(height) for j in range(len(grid[i]))}
for i in range(height - 1):
    for j in range(width):
        if grid[i][j] in down_set and grid[i + 1][j] in up_set:
            union(uf, (i, j), (i + 1, j))

for i in range(height):
    for j in range(width - 1):
        if grid[i][j] in left_set and grid[i][j + 1] in right_set:
            union(uf, (i, j), (i, j + 1))


loop = find(uf, start)

print(f"Part one: {-uf[loop] // 2}")


def process_line(grid, uf, i):
    j = 0
    # are counting L----7 and F---J and |
    cnt = 0
    sum = 0
    while j < width:
        if find(uf, (i, j)) == loop:
            if grid[i][j] == "|":
                cnt += 1
                j += 1
            elif grid[i][j] == "L":
                j += 1
                while grid[i][j] == "-":
                    j += 1
                if grid[i][j] == "7":
                    cnt += 1
                    j += 1
                elif grid[i][j] == "J":
                    j += 1
            elif grid[i][j] == "F":
                j += 1
                while grid[i][j] == "-":
                    j += 1
                if grid[i][j] == "J":
                    cnt += 1
                    j += 1
                elif grid[i][j] == "7":
                    j += 1
        else:
            j += 1
            sum += cnt % 2
    return sum


print(f"Part two: {sum(process_line(grid, uf, i) for i in range(height))}")
