res = 0
for i in xrange(1, 30):
  for j in xrange(1, 15):
    if len(str(j ** i)) == i:
      print "%d^%d = %d" % (j, i, j ** i)
      res += 1
print res