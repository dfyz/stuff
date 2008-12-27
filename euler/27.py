from nzmath.prime import primeq

def F(a, b):
  cur = 0
  while (primeq(cur ** 2 + a * cur + b)):
    cur += 1
  return cur

res = 0
ans = 0
for a in xrange(-1000, 1001):
  for b in xrange(-1000, 1001):
    nres = F(a, b)
    if nres > res:
      res = nres
      ans = a * b
print ans