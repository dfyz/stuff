vals = [ 1, 2, 5, 10, 20, 50, 100, 200 ]
N = 200
M = len(vals)

dp = [ [1] * M ]
for i in xrange(1, N + 1):
  dp.append([0] * M)

for i in xrange(1, 201):
  for j in xrange(M):
    for k in xrange(0, j + 1):
      prev = i - vals[k]
      if prev >= 0:
        dp[i][j] += dp[prev][k]

print dp