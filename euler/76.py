dp = {}

def F(num, fr):
  if num == 0:
    return 1
  z = (num, fr)
  if z in dp:
    return dp[z]
  res = 0
  for i in xrange(fr, 0, -1):
    if num - i >= 0:
      res += F(num - i, i)
  dp[z] = res
  return res

print F(100, 100)