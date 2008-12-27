from nzmath.prime import *

UPPER = 1000000
primes = list(generator_eratosthenes(UPPER))

N = len(primes)
max_diff = 0
answer = 0

for i in xrange(N):
  sum = 0
  for j in xrange(i, N):
    sum += primes[j]
    if sum > UPPER:
      break
    if primeq(sum) and j - i + 1 > max_diff:
      max_diff = j - i + 1
      answer = sum

print answer, max_diff