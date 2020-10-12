import math

def isPet(x):
    return math.sqrt(1+24*x) % 6 == 5

def result():
    pents = [1,5,12]
    n = 10
    pn = 22
    while True:
        pents.append(pn)
        for i in pents:
            a = abs(pn - i)
            b = abs(a - i)
            if isPet(a) and isPet(b):
                return b
        n+=3
        pn+=n

print(result())
