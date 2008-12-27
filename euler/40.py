s = ""
MP = 6

n = 1
while len(s) < 10 ** MP:
  s += str(n)
  n += 1
res = 1
for i in xrange(MP + 1):
  res *= int(s[10 ** i - 1])
print res