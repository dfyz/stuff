def Same(a, b):
  x, y = list(str(a)), list(str(b))
  x.sort()
  y.sort()
  return x == y

def Cool(arr):
  for i in xrange(len(arr) - 1):
    if not Same(arr[i], arr[i + 1]):
      return False
  return True

for i in xrange(100000, 500000):
  if (Cool([j * i for j in xrange(1, 7)])):
    print i
    exit(0)