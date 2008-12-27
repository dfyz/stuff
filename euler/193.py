from nzmath.prime import *
from nzmath.factor.methods import *
from nzmath.multiplicative import *
from nzmath.squarefree import *
from itertools import *
from math import *

def Bt(cur, pos, cnt, M, primes, SN, have):
  for i in xrange(pos, len(primes)):
	next = cur * primes[i]
	if next <= SN:
	  M[next] = (-1) ** (cnt + 1)
	  Bt(next, i + 1, cnt + 1, M, primes, SN, have + 1)
	else:
	  break

def Mine(N):
  SN = int(sqrt(N - 1))
  M = [ 0 ] * (SN + 1)
  primes = list(generator_eratosthenes(SN + 1))
  
  Bt(1, 0, 0, M, primes, SN, 0)
  M[1] = 1


  res = 0
  for i in xrange(1, SN + 1):
	if i % (10 ** 5) == 0:
	  print i
	res += M[i] * ((N - 1) / (i ** 2))
  return res

def Correct(N):
  return len([i for i in range(1, N) if viafactor(i)])

print Mine(2 ** 50)