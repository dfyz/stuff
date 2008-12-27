cool = list("123456789")

def PanDigital(arr):
  global cool
  arr.sort()
  return arr == cool

for i in xrange(9, 100000):
  s = ""
  n = 1
  while True:
    z = str(i * n)
    if len(z) + len(s) <= 9:
      n += 1
      s += z
    else:
      break
  if len(s) == 9 and PanDigital(list(s)):
    print s + " is pandigital"