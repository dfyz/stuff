from nzmath.gcd import *
from nzmath.multiplicative import *
from nzmath.rational import *

a = Rational(2, 5)
b = Rational(3, 7)
n = 2

for iter in xrange(2, 1000000):
    if iter > 8:
        z = a.denominator + b.denominator
        if z <= iter:
            a = Rational(a.numerator + b.numerator, z)
    if iter % 1000 == 0:
        print iter
    n += euler(iter)
print a