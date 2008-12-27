def Sum(num):
  res = 0
  for i in str(num):
    res += int(i)
  return res

print max([Sum(a ** b) for a in xrange(1, 100) for b in xrange(1, 100)])