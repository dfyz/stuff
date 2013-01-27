import Control.Monad
import Data.Char
import Data.List
import Data.Ord

import Text.Printf

solve =
	sum
		. (zipWith (*) [26,25..])
		. ((map length)
		. (sortBy $ (comparing (negate . length)))
		. group
		. sort
		. (map toLower)
		. (filter isAlpha))

main = do
	n <- readLn
	forM_ [1..n] $ \i -> do
		line <- getLine
		printf "Case #%d: %d\n" (i :: Int) (solve line)