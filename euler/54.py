import string
import sys

class Card:
	def __init__(self, s):
		self.raw_value = s
		self.suit = s[1]
		self.value = self.ParseValue(s[0])	
	
	def __cmp__(self, other):
		return cmp(self.value, other.value)
	
	def __eq__(self, other):
		return self.value == other.value and self.suit == other.suit
	
	def __str__(self):
		return self.raw_value
	
	def __repr__(self):
		return str(self)
		
	def ParseValue(self, s):
		if s in string.digits:
			return int(s)
		return [ 'T', 'J', 'Q', 'K', 'A' ].index(s) + 10

possible_values = range(2, 14 + 1)

class NoComboError(Exception):
	def __str__(self):
		return "There is no matching combo"

def CmpCards(_cards1, _cards2):
	cards1 = list(_cards1)
	cards2 = list(_cards2)
	cards1.sort()
	cards1.reverse()
	cards2.sort()
	cards2.reverse()
	for c1, c2 in zip(cards1, cards2):
		cv = cmp(c1, c2)
		if cv > 0:
			return 1
		if cv < 0:
			return 2
	return 0

def FilterByValue(cards, value):
	return [c for c in cards if c.value == value]

def Exclude(all_cards, cards):
	return [c for c in all_cards if c not in cards]

def HighestCard(cards):
	return ([], cards)

def NIdentical(n):
	def __NIdentical(cards, n):
		for value in possible_values:
			with_value = FilterByValue(cards, value)
			if len(with_value) >= n:
				combo = with_value[:n]
				return (combo, Exclude(cards, combo))
		return None
	
	return lambda cards: __NIdentical(cards, n)

def IsStraight(_cards):
	cards = list(_cards)
	cards.sort()
	i = 0
	while i + 1 < len(cards):
		c1 = cards[i]
		c2 = cards[i + 1]
		if c2.value != c1.value + 1:
			return False
		i += 1
	return True

def IsFlush(cards):
	return all([cards[i].suit == cards[0].suit for i in xrange(len(cards))])

def FiveCombo(*predicates):
	def __FiveCombo(cards, *predicates):
		if not all([p(cards) for p in predicates]):
			return None
		return (cards, [])
	
	return lambda cards: __FiveCombo(cards, *predicates)

def TwoPairs(cards):
	p1 = NIdentical(2)(cards)
	if not p1:
		return None
	p2 = NIdentical(2)(p1[1])
	if not p2:
		return None
	two_pairs = p1[0] + p2[0]
	return (two_pairs, Exclude(cards, two_pairs))

def FullHouse(cards):
	tri = NIdentical(3)(cards)
	if not tri or tri[1][0].value != tri[1][1].value:
		return None
	return tri

combos = [
	HighestCard,
	NIdentical(2),
	TwoPairs,
	NIdentical(3),
	FiveCombo(IsStraight),
	FiveCombo(IsFlush),
	FullHouse,
	NIdentical(4),
	FiveCombo(IsStraight, IsFlush),
	FiveCombo(IsStraight, IsFlush, lambda cards: cards[0].value == 10)
]

combo_names = [
	'Highest card',
	'Pair',
	'2 Pairs',
	'Three',
	'Straight',
	'Flush',
	'Full house',
	'Four',
	'Straigh Flush',
	'Royal Flush'
]

combos.reverse()
combo_names.reverse()

def GetCombo(cards):
	for idx, combo in enumerate(combos):
		if combo(cards):
			print >>sys.stderr, "Combo is " + combo_names[idx]
			return (len(combos) - 1 - idx, combo)
	raise NoComboError()

log = open('poker.txt', 'r')
res = 0
for line in log:
	all_cards = [Card(token) for token in line.split()]
	
	cards1 = all_cards[:5]
	print >>sys.stderr, "1st player = %s" % cards1
	idx1, combo1 = GetCombo(cards1)
	
	cards2 = all_cards[5:]
	print >>sys.stderr, "2nd player = %s" % cards2
	idx2, combo2 = GetCombo(cards2)

	player1_wins = False
	if idx1 > idx2:
		print >>sys.stderr, "1st player has better combo"
		player1_wins = True
	elif idx1 == idx2:
		print >>sys.stderr, "Players have same combos"
		a1, b1 = combo1(cards1)
		print >>sys.stderr, "a1 = %s, b1 = %s" % (a1, b1)
		a2, b2 = combo2(cards2)
		print >>sys.stderr, "a2 = %s, b2 = %s" % (a2, b2)
		
		combo_cmp = CmpCards(a1, a2)
		if combo_cmp == 1:
			print >>sys.stderr, "1st player has better combo cards"
			player1_wins = True
		elif combo_cmp == 0 and CmpCards(b1, b2) == 1:
			print >>sys.stderr, "1st player has better non-combo cards"
			player1_wins = True
		
	if player1_wins:
		res += 1
print res