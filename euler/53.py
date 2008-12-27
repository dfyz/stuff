from nzmath.combinatorial import *

res = 0
for i in xrange(23, 101):
  for j in xrange(1, i + 1):
    if binomial(i, j) > 1000000:
      res += 1
print res