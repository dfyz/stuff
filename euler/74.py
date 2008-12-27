facts = [ 1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880 ]

def Next(num):
  res = 0
  while num:
    res += facts[num % 10]
    num /= 10
  return res

N = 1000000
vals = [ -1 ] * (N + 1)

def F(num):
  if vals[num] != -1:
    return vals[num]
  step = 1
  z = num
  visited = { num: 1 }
  while True:
    z = Next(z)
    if vals[num] != -1:
      step += vals[num]
      break
    # print z
    if z in visited:
      vals[z] = step - visited[z]
      break
    step += 1
    visited[z] = step
    
  vals[num] = step
  return step

res = 0
for i in xrange(1, N + 1):
  if F(i) == 60:
    print i
    res += 1
print res