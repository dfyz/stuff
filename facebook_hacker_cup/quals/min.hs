import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.List
import Test.QuickCheck
import Text.Printf

minimumAbsentNumber numbers = fromJust $ find (\x -> notElem x numbers) [0..]
nextRng b c r prev = (b * prev + c) `mod` r

smartSolve :: TestCase -> Integer
smartSolve (TestCase n k a b c r) =
	cyclingList !! (fromIntegral ((n - k - 1) `mod` (k + 1)))
	where
		startList = genericTake k (iterate (nextRng b c r) a)
		positionMap = M.fromList $ filter ((<= k) . snd) $ zip [1..] startList
		cyclingList = reverse $ snd $ mapAccumL getNumber (S.fromList [0..k]) [k, k-1..0]
		getNumber availableNums pos =
			let maxNum = S.findMax availableNums; next =
				case M.lookup pos positionMap of
					Just x -> if S.member x availableNums then x else maxNum
					Nothing -> maxNum
			in (S.delete next availableNums, next)

baseLineSolve :: TestCase -> Integer
baseLineSolve (TestCase n k a b c r) =
	head (genList 1 [a])
	where
		genList idx result@(x:xs)
			| idx == n =
				result
			| otherwise =
				let next = 
					if idx < k
						then nextRng b c r x
						else minimumAbsentNumber (genericTake k result)
					in
				genList (idx + 1) (if idx == k then (next:result) else (next:result))

solveSmallCase numbers =
	genList
	where
		n = length numbers
		startList = reverse numbers

		genList = unfoldr nextNumber startList
		nextNumber present =
			let next =
				minimumAbsentNumber (take n present)
				in
			Just (next, (next:present))

printSmallCaseSolution = do
	let smallCase = [6, 7, 8, 0, 1]
	let smallCaseLength = length smallCase
	let answer = solveSmallCase smallCase

	let formattedAnswer = unfoldr
		(\y -> let (x, xs) = splitAt (smallCaseLength + 1) y in Just (x, xs))
		answer

	mapM_ (putStrLn . show) (take 10 formattedAnswer)

data TestCase =
	TestCase Integer Integer Integer Integer Integer Integer
	deriving Show

instance Arbitrary TestCase where
	arbitrary = do
		let billion = 1000000000
		k <- choose (1, 100)
		n <- choose (k + 1, 300)
		a <- choose (0, billion)
		b <- choose (0, billion)
		c <- choose (0, billion)
		r <- choose (1, billion)
		return $ (TestCase n k a b c r)
				
doQuickCheck = do
	verboseCheck (\tc -> baseLineSolve tc == smartSolve tc)

main = do
	-- doQuickCheck

	n <- readLn
	forM_ [1..n] $ \i -> do
		[n, k] <- readIntegers
		[a, b, c, r] <- readIntegers
		putStrLn ("Case #" ++ (show i) ++ ": " ++ (show $ smartSolve $ TestCase n k a b c r))

	-- printSmallCaseSolution

	where
		readIntegers = ((map read) . words) <$> getLine
