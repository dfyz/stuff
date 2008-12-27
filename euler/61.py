from itertools import *

def Num(f):
  n = 1
  while True:
    yield f(n)
    n += 1

tri = lambda n: n*(n+1)/2
sq = lambda n: n**2
pent = lambda n: n*(3*n-1)/2
hex = lambda n: n*(2*n-1)
hept = lambda n: n*(5*n-3)/2
oct = lambda n: n*(3*n - 2)

def F(f):
  return list(takewhile(lambda x: x <= 9999, dropwhile(lambda x: x < 1000, Num(f))))

def CanJump(a, b):
  return a != b and b / 100 == a % 100

def Label(arr):
  return {
    1081: "tri",
    1089: "sq",
    1080: "pent",
    1128: "hex",
    1177: "hept",
    1160: "oct"
  }[arr[1]]

fs = map(F, [ tri, sq, pent, hex, hept, oct ])

for perm in permutations(fs):
  #print ", ".join(map(Label, perm))
  for start in perm[0]:
    #print "Trying from %d" % start
    have = [[start]]
    #print Label(perm[0]), have
    for i in xrange(1, len(fs)):
      new_have = []
      layer = perm[i]
      for v1 in have[len(have) - 1]:
        for v2 in layer:
          if CanJump(v1, v2):
            new_have.append(v2)
      have.append(new_have)
      #print Label(layer), have
    cool = filter(lambda v: CanJump(v, start), have[len(have) - 1])
    if cool:
      ans = [cool[0]]
      for i in xrange(len(have) - 2, -1, -1):
        for j in have[i]:
          if CanJump(j, ans[len(ans) - 1]):
            ans.append(j)
            break
      ans.reverse()
      res = 0
      for i in ans:
        res += i
      print res
      exit(0)
      #print [(Label(perm[i]), ans[i]) for i in xrange(len(perm))]