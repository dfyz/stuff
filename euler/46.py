from nzmath.prime import generator_eratosthenes, primeq

UPPER = 500000
primes = list(generator_eratosthenes(UPPER))

nums = set()

i = 1
while True:
  z = 2 * (i ** 2)
  if z > UPPER:
    break
  for p in primes:
    nums.add(p + z)
  i += 1

j = 35
while True:
  if not primeq(j) and j not in nums:
    print j
    exit(0)
  j += 2