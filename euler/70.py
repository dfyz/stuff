from nzmath.multiplicative import euler
from nzmath.rational import Rational
from nzmath.factor.methods import factor
from nzmath.prime import generator_eratosthenes

def GetDigitArr(num):
  res = [ 0 ] * 10
  while num:
    res[num % 10] += 1
    num /= 10
  return res

def IsPerm(a, b):
  return GetDigitArr(a) == GetDigitArr(b)
  
# print IsPerm(12345, 24315)
# print IsPerm(87109, 79180)
# print IsPerm(47749, 97748)

UPPER = 10000000

answer = 2
min_ratio = Rational(2, 1)

primes = list(generator_eratosthenes(UPPER))
for i in xrange(len(primes)):
  for j in xrange(i):
    p1 = primes[i]
    p2 = primes[j]
    prod = p1 * p2
    if prod >= UPPER:
      break
    phi = (p1 - 1) * (p2 - 1)
    if IsPerm(prod, phi):
      new_ratio = Rational(prod, phi)
      if new_ratio < min_ratio:
        min_ratio = new_ratio
        answer = prod
    

print answer