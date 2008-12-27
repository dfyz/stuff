d = {}
e = {}

def Add(num):
  num **= 3
  arr = list(str(num))
  arr.sort()
  s = "".join((arr))
  d[s] = d.get(s, 0) + 1
  if s not in e:
    e[s] = []
  e[s].append(num)
  if d[s] == 5:
    e[s].sort()
    print e[s]
    

for i in xrange(1, 10000):
  Add(i)
# print d["01234566"]