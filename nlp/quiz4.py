#! -*- coding: utf-8 -*-

import math

if __name__ == '__main__':
	v = [2.19722, 2.19722, -1.3862943611198906, 0.0]

	def Feature(idx, word, tag):
		cond = False
		if idx == 0:
			cond = word == 'the' and tag == 'D'
		elif idx == 1:
			cond = word == 'dog' and tag == 'N'
		elif idx == 2:
			cond = (word != 'dog' and word != 'the') and tag == 'D'
		elif idx == 3:
			cond = (word != 'dog' and word != 'the') and tag == 'N'
		return 1.0 if cond else 0.0

	def Prob(word, tag):
		def ForTag(y):
			return math.exp(sum([Feature(i, word, y) * v[i] for i in range(len(v))]))
		numerator = ForTag(tag)
		denominator = sum([ForTag(y) for y in ['D', 'N']])
		return numerator / denominator

	def PrintProb(word, tag):
		print 'p({1} | {0}) = {2}'.format(word, tag, Prob(word, tag))

	PrintProb('the', 'D')
	PrintProb('dog', 'N')
	PrintProb('diğer', 'D')
	PrintProb('diğer', 'N')
	PrintProb('the', 'N')
	PrintProb('dog', 'D')