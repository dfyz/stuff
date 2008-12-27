from nzmath.rational import Rational
from nzmath.gcd import gcd

res = Rational(1, 1)

def Try(a, b, c, d, e, f):
  global res
  fst = Rational(a * 10 + b, c * 10 + d)
  snd = Rational(e, f)
  if fst == snd and fst < Rational(1, 1):
    print "Found %d/%d -> %d/%d" % (a * 10 + b, c * 10 + d, e, f  )
    res *= fst

for a in xrange(1, 10):
  for b in xrange(1, 10):
    for c in xrange(1, 10):
      Try(a, b, a, c, b, c)
      Try(b, a, a, c, b, c)
      Try(a, b, c, a, b, c)
      Try(b, a, c, a, b, c)
print res