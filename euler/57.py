from nzmath.rational import *

memo = {}

def F(n):
  if n in memo:
    return memo[n]
  if n == 1:
    return Rational(1, 2)
  memo[n] = Rational(1, 1) / (Rational(2, 1) + F(n - 1))
  return memo[n]

def G(n):
  return Rational(1, 1) + F(n)

res = 0
for i in xrange(1, 1001):
  if len(str(G(i).numerator)) > len(str(G(i).denominator)):
    print i, G(i)
    res += 1
print res