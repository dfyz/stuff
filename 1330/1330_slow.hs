import Control.Applicative
import Control.Monad
import Data.Array
import Data.Time.Clock
import System.IO
import Text.Printf

readInt :: IO Int
readInt = readLn

readIntPair :: IO [Int]
readIntPair = parseLine <$> getLine
    where
        parseLine = (map read) . words

getAnswer prefixSums (from:to:_) = (prefixSums ! to) - (prefixSums ! (from - 1))

doMain = do
    n <- readInt
    numbers <- replicateM n readInt
    let prefixSums = listArray (0, n) (scanl (+) 0 numbers)
    m <- readInt
    queries <- replicateM m readIntPair
    mapM_ (print . getAnswer prefixSums) queries

main = do
    start <- getCurrentTime
    _ <- doMain
    end <- getCurrentTime
    let timeDiff = diffUTCTime end start
    hPrintf stderr "\tTime inside the program: %.2f\n" (toDouble timeDiff)

    where
        toDouble :: (Real a) => a -> Double
        toDouble = fromRational . toRational
