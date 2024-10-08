data = []

with open("input24.txt", "r") as file:
    for line in file:
        line = line[:-1].replace(" ", "")
        [p, v] = line.split("@")
        data.append((eval(p), eval(v)))


def det(a, b, c):
    return (
        a[0] * (b[1] * c[2] - b[2] * c[1])
        + a[1] * (b[2] * c[0] - b[0] * c[2])
        + a[2] * (b[0] * c[1] - b[1] * c[0])
    )


def coeffs(d1, d2, d3):
    (x1, y1, z1), (u1, v1, w1) = d1
    (x2, y2, z2), (u2, v2, w2) = d2
    (x3, y3, z3), (u3, v3, w3) = d3
    dx1 = x3 - x1
    dy1 = y3 - y1
    dz1 = z3 - z1
    dx2 = x3 - x2
    dy2 = y3 - y2
    dz2 = z3 - z2
    return {
        "t1t2": det((u1, v1, w1), (u2, v2, w2), (u3, v3, w3)),
        "t1": det((dx2, dy2, dz2), (u1, v1, w1), (u3, v3, w3)),
        "t2": det((u2, v2, w2), (dx1, dy1, dz1), (u3, v3, w3)),
        "1": det((dx1, dy1, dz1), (dx2, dy2, dz2), (u3, v3, w3)),
    }


"""
The basic idea is to have
the rock smash stone 1 at time t1 and stone 2 at time t2
This gives us two points of its trajectory:
    x1 + t1 v1, ...
and
    x2 + t2 v2, ...
This line has to intersect all the other trajectories.
Expressing this as a zero determinant, we obtain an expression of the form
alpha t1 t2 + beta t1 + gamma t2 + delta
With sufficiently many stones, we cancel the t1 t2 coefficient,
and recover t1, t2... and the rest.
"""


c = [coeffs(data[0], data[1], data[k]) for k in range(2, len(data))]


def eliminate_aux(d1, d2, k):
    d = {}
    for key in d1:
        if k != key:
            d[key] = d1[key] * d2[k] - d1[k] * d2[key]
    return d


def eliminate(d, k):
    return [eliminate_aux(d[0], d[i], k) for i in range(1, len(d))]


def get_t1(d):
    d1 = eliminate(d, "t1t2")
    d2 = eliminate(d1, "t2")
    return [-dico["1"] / dico["t1"] for dico in d2]


def get_t2(d):
    d1 = eliminate(d, "t1t2")
    d2 = eliminate(d1, "t1")
    return [-dico["1"] / dico["t2"] for dico in d2]


gt1 = get_t1(c)
t1 = gt1[0]
gt2 = get_t2(c)
t2 = gt2[0]

"""
x0 + t1 u0 = x1 + t1 u1
y0 + t1 v0 = y1 + t1 v1
z0 + t1 w0 = z1 + t1 w1
x0 + t2 u0 = x2 + t2 u2
y0 + t2 v0 = y2 + t2 v2
z0 + t2 w0 = z2 + t2 w2

(t2 - t1) u0 = x2 + t2 u2 - (x1 + t1 u1)
"""
(x1, y1, z1), (u1, v1, w1) = data[0]
(x2, y2, z2), (u2, v2, w2) = data[1]

u0 = ((x2 + t2 * u2) - (x1 + t1 * u1)) / (t2 - t1)
v0 = ((y2 + t2 * v2) - (y1 + t1 * v1)) / (t2 - t1)
w0 = ((z2 + t2 * w2) - (z1 + t1 * w1)) / (t2 - t1)

x0 = x1 + (u1 - u0) * t1
y0 = y1 + (v1 - v0) * t1
z0 = z1 + (w1 - w0) * t1

print(x0 + y0 + z0)
