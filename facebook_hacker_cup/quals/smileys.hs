import Control.Monad
import Data.Char
import Data.List
import Data.Ord

import Text.Printf

check x "" = x == 0
check x str =
	if x < 0
		then False
		else
			case str of
				(':':'(':rest) -> (check x rest) || (check (x + 1) rest)
				(':':')':rest) -> (check x rest) || (check (x - 1) rest)
				('(':rest) -> check (x + 1) rest
				(')':rest) -> check (x - 1) rest
				(_:rest) -> check x rest

solve = check 0

main = do
	n <- readLn
	forM_ [1..n] $ \i -> do
		line <- getLine
		printf "Case #%d: %s\n" (i :: Int) (if (solve line) then "YES" else "NO")