import string

encrypted = map(int, open('cipher1.txt').read().split(','))

for a in string.lowercase:
  for b in string.lowercase:
    for c in string.lowercase:
      key = map(ord, [a, b, c])
      original = ""
      sum = 0
      for i in xrange(len(encrypted)):
        z = key[i % 3] ^ encrypted[i]
        original += chr(z)
        sum += z
      if "Gospel" in original:
        print original
        print sum
        exit(0)