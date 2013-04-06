import Data.Array
import Data.Function
import Data.List
import Control.Applicative
import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import System.Environment
import Text.JSON
import Text.Printf

data ParseTree =
	Unary String String |
	Binary String ParseTree ParseTree

treeLabel (Unary label _) = label
treeLabel (Binary label _ _) = label

readTree = fromJson . ensureOK . decode
	where
		ensureOK (Ok x) = x
		ensureOK (Error x) = error x
		fromJson (JSArray [(JSString label), (JSString term)]) = Unary (fromJSString label) (fromJSString term)
		fromJson (JSArray [(JSString label), tree1, tree2]) = Binary (fromJSString label) (fromJson tree1) (fromJson tree2)
		fromJson x = error $ "Couldn't parse tree: " ++ (show x)

dumpTree = encode . toJson
	where
		toJson (Unary label term) = JSArray [JSString (toJSString label), JSString (toJSString term)]
		toJson (Binary label tree1 tree2) = JSArray [JSString (toJSString label), (toJson tree1), (toJson tree2)]

getCounts uc bc trees = M.fromListWith (+) [(t, 1 :: Integer) | t <- concatMap itemsToCount trees]
	where
		itemsToCount (Unary label term) = uc (label, term)
		itemsToCount (Binary label t1 t2) = bc (label, (treeLabel t1), (treeLabel t2)) ++ (itemsToCount t1) ++ (itemsToCount t2)

countWords = getCounts (\(_, t) -> [t]) (const [])
countNonTerminals = getCounts (\(l, _) -> [l]) (\(l, _, _) -> [l])
countUnaries = getCounts (\x -> [x]) (const [])
countBinaries = getCounts (const []) (\x -> [x])

newWord wc x = if (M.notMember x wc) || ((wc M.! x) < 5) then "_RARE_" else x

replaceRareWords wc = replaceRareWords'
	where
		replaceRareWords' (Binary label t1 t2) = Binary label (replaceRareWords' t1) (replaceRareWords' t2)
		replaceRareWords' (Unary label term) = Unary label (newWord wc term)

log2 = logBase 2

minusInf :: Double
minusInf = read "-Infinity"

parse binaryRules qb qu nonterminals sentence wc = cky (bounds sentence) "SBARQ"
	where
		cky indexes start = cache M.! (indexes, start)

		to = snd $ bounds sentence

		states = [ ((i, j), t) | i <- [1..to], j <- [i..to], t <- nonterminals ]
		cache =
			M.fromList [ (s, solve (fst s) (snd s)) | s <- states ]

		solve (i, j) start
			| i == j = solveUnary i start
			| otherwise = solveBinary i j start

		solveUnary idx start = do
			let word = sentence ! idx
			prob <- M.lookup (start, newWord wc word) qu
			return (prob, Unary start word)

		formCandidate (i, j) (x, y, z) s = do
			let pRule = qb M.! (x, y, z)
			(pLeft, left) <- cky (i, s) y
			(pRight, right) <- cky (s + 1, j) z
			return (pRule + pLeft + pRight, Binary x left right)

		solveBinary i j start = do
			let splits = [i..(j - 1)]
			rules <- M.lookup start binaryRules
			let candidates = catMaybes [ formCandidate (i, j) rule s | rule <- rules, s <- splits ]
			if null candidates then Nothing else Just (maximumBy (compare `on` fst) candidates)

logProb :: Integer -> Integer -> Double
logProb a b = log2 ((fromIntegral a) / (fromIntegral b))

parseSentence line = listArray (1, length w) w
	where
		w = words line
		

fstTriple (x, _, _) = x

main = do
	[trainFile, testFile] <- getArgs
	trees <- (map readTree . lines) <$> readFile trainFile
	let wc = countWords trees
	let preparedTrees = map (replaceRareWords wc) trees
	let ntc = countNonTerminals preparedTrees
	let uc = countUnaries preparedTrees
	let bc = countBinaries preparedTrees
	
	let binaryRules = M.fromList [(fstTriple (head rule), rule) | rule <- groupBy ((==) `on` fstTriple) (M.keys bc)]
	let qb = M.mapWithKey (\(x, _, _) v -> logProb v (ntc M.! x)) bc
	let qu = M.mapWithKey (\(x, _) v -> logProb v (ntc M.! x)) uc
	let nonterminals = M.keys ntc

	sentences <- lines <$> readFile testFile
	forM sentences $ \line -> do
		let parsed = parseSentence line
		case (parse binaryRules qb qu nonterminals parsed wc) of
			Just (p, tree) -> printf "%s\n" (dumpTree tree)
			Nothing -> error ("Couldn't parse " ++ line)

		