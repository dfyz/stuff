import datetime

res = 0
for y in xrange(1901, 2001):
  for m in xrange(1, 13):
    if datetime.date(y, m, 1).weekday() == 6:
      res += 1
print res