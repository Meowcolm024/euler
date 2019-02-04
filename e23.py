# Non-abundant sums
# I just can't figure it out using Haskell QAQ
import math
limit = 28123
mat = [False for i in range(limit)]
def getDiv(n):
    divs = []
    for i in range(1,math.floor(math.sqrt(n-1))+1):
        if n % i == 0:
            divs.append(i)
    tmp = []
    for i in divs:
        tmp.append(n/i)
    if math.pow(math.floor(math.sqrt(n)),2) == n:
        divs.append(math.sqrt(n))
    divs += tmp
    divs.sort()
    return divs


def sumFact(n):
    divs = getDiv(n)
    sums = sum(divs) - n
    return sums


abundant =[]
for i in range(12, limit+1):
    if sumFact(i) > i:
        abundant.append(i)

print(len(abundant))

for i in range(len(abundant)):
    for j in range(i,len(abundant)):
        s = abundant[i] + abundant[j]
        if s > limit:
            break
        mat[s-1] = True

sums = []
for i in range(limit):
    if mat[i] == False:
        sums.append(i+1)

print(sum(sums))
