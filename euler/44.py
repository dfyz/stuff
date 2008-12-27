from math import sqrt, floor

def IsSqrt(x):
  sq = sqrt(x)
  return sq - floor(sq) < 1e-6

def IsPent(x):
  z = 1 + 24 * x
  return IsSqrt(z) and (sqrt(z) + 1) % 6 == 0

def Pent(n):
  return n * (3 * n - 1) / 2

UPPER = 10000

for i in xrange(1, UPPER):
  for j in xrange(i + 1, UPPER):
    d = Pent(i)
    s = Pent(j)
    z = d + s
    if z % 2 == 1:
      continue
    z /= 2
    # print z
    if not IsPent(z):
      continue
    y = z - d
    if not IsPent(y):
      continue
    print "FOUND: P_s = %d, P_d = %d, P_i = %d, P_j = %d" % (s, d, y, z)
    exit(0)