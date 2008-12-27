from nzmath.prime import generator

UPPER = 50000000
# UPPER = 50

nums = set()

for z in generator():
  Z = z ** 4
  if Z > UPPER:
    break
  for y in generator():
    Y = y ** 3
    if Y + Z > UPPER:
      break
    for x in generator():
      X = x ** 2
      if X + Y + Z > UPPER:
        break
      nums.add(X + Y + Z)
print len(nums)