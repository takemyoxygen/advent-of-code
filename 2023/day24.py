import re
from sympy import solve, symbols, Eq

def read_file():
    with open("./input/day24.txt", "r") as file:
        return [[int(x) for x in re.findall("-?\d+", line)] for line in file]

hails = read_file()

x, y, z, vx, vy, vz, t1, t2, t3 = symbols('x, y, z, vx, vy, vz, t1, t2, t3')
ts = [t1, t2, t3]
eqs = []

for i in range(len(ts)):
    t = ts[i]
    xh, yh, zh, vxh, vyh, vzh = hails[i]
    eqs.append(Eq(x + t * vx, xh + t * vxh))
    eqs.append(Eq(y + t * vy, yh + t * vyh))
    eqs.append(Eq(z + t * vz, zh + t * vzh))


solution = solve(eqs, (x, y, z, vx, vy, vz, t1, t2, t3))
print("Day 24, part 2: ", sum(solution[0][:3]))
