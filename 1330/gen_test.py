import random

n = 10000
print n
for i in xrange(n):
	print random.randint(-n, n)

m = 10*n
f = lambda: random.randint(1, n)
print m
for i in xrange(m):
	a, b = f(), f()
	print min(a, b), max(a, b)