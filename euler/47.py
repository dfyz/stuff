from nzmath.factor.methods import factor

for i in xrange(100, 1000000):
  ok = True
  for j in xrange(i, i + 4):
    ok = ok and len(factor(j)) == 4
  if ok:
    print i
    exit(0)