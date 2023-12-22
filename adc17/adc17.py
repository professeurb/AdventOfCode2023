import heapq

# file = "test17.txt"
# file = "test17b.txt"
file = "input17.txt"

map = []

with open(file, "r") as input:
    for line in input:
        map.append(list(line[:-1]))

height = len(map)
width = len(map[0])

heap = []
seen = set()

heapq.heappush(heap, (0, 0, 0, 1, 0, 3))
heapq.heappush(heap, (0, 0, 0, 0, 1, 3))

while True:
    v, l, c, a, b, dist = heapq.heappop(heap)
    # print(v, l, c, a, b, dist)
    if l < 0 or l >= height:
        continue
    if c < 0 or c >= width:
        continue
    if dist == 0:
        continue
    if (l, c, a, b, dist) in seen:
        continue
    seen.add((l, c, a, b, dist))
    v += int(map[l][c])
    if l == height - 1 and c == width - 1:
        print("Part one:", v - int(map[0][0]))
        break
    heapq.heappush(heap, (v, l + a, c + b, a, b, dist - 1))
    heapq.heappush(heap, (v, l - b, c + a, -b, a, 3))
    heapq.heappush(heap, (v, l + b, c - a, b, -a, 3))

heap = []
seen = set()

heapq.heappush(heap, (0, 0, 0, 1, 0, 0, None))
heapq.heappush(heap, (0, 0, 0, 0, 1, 0, None))

while True:
    v, l, c, a, b, dist, prev = heapq.heappop(heap)
    # print(v, l, c, a, b, dist)
    if l < 0 or l >= height:
        continue
    if c < 0 or c >= width:
        continue
    if (l, c, a, b, dist) in seen:
        continue
    seen.add((l, c, a, b, dist))
    v += int(map[l][c])
    if l == height - 1 and c == width - 1 and dist >= 3:
        last = (l, c, a, b, dist)
        print("Part two:", v - int(map[0][0]))
        break
    if dist < 9:
        heapq.heappush(heap, (v, l + a, c + b, a, b,
                       dist + 1, (l, c, a, b, dist)))
    if dist >= 3:
        heapq.heappush(heap, (v, l - b, c + a, -b, a, 0, (l, c, a, b, dist)))
        heapq.heappush(heap, (v, l + b, c - a, b, -a, 0, (l, c, a, b, dist)))

# while last is not None:
#     map[last[0]][last[1]] = "*"
#     last = seen[last]
#
# for line in map:
#     print("".join(line))
