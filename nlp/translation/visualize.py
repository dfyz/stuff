import collections

if __name__ == '__main__':
	alignments = collections.defaultdict(set)
	with open('dev.key') as f:
		for line in f:
			sentence, engIdx, spIdx = map(int, line.strip().split())
			alignments[sentence - 1].add((engIdx - 1, spIdx - 1))

	with open('dev.es') as sp:
		with open('dev.en') as en:
			for i, (spLine, enLine) in enumerate(zip(sp, en)):
				spTokens = spLine.strip().split()
				enTokens = enLine.strip().split()
				print '=' * 20
				def FormatTokens(tokens):
					return ' '.join(u'{0}:{1}'.format(idx, x) for idx, x in enumerate([t.decode('utf-8') for t in tokens])).encode('utf-8')
				print FormatTokens(spTokens)
				print FormatTokens(enTokens)
				print '=' * 20
				for j, spWord in enumerate(spTokens):
					print str(j).ljust(5, ' '),
					for k, enWord in enumerate(enTokens):
						symbol = 'X' if (k, j) in alignments[i] else '.'
						print symbol.ljust(3, ' '),
					print
				print ' '*5,
				for j in xrange(len(enTokens)):
					print str(j).ljust(3, ' '),
				print