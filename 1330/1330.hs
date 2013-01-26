import Control.Applicative
import Control.Monad
import Data.Array
import Data.Fixed
import Data.Time.Clock
import System.IO
import System.Random
import Text.Printf
 
readInt :: IO Int
readInt = readLn
 
readIntPair :: IO [Int]
readIntPair = parseLine <$> getLine
    where
        parseLine = (map read) . words

getAnswer' prefixSums from to = (prefixSums ! to) - (prefixSums ! (from - 1))
getAnswer prefixSums (from:to:_) = getAnswer' prefixSums from to
getAnswerForPair prefixSums (from, to) = getAnswer' prefixSums from to

doMain = do
    n <- readInt
    numbers <- replicateM n readInt
    let prefixSums = listArray (0, n) (scanl (+) 0 numbers)
    m <- readInt
    replicateM_ m $ do
        line <- getLine
        let [x, y] = (map read) $ words $ line
        print $ getAnswer' prefixSums x y

{- doMain2 = do
    let n = 10000
    print n
    mapM_ print $ take n $ randomRs (-n, n) $ mkStdGen 42
    let m = 10*n
    print m
    mapM_ (\(x, y) -> printf "%d %d\n" (min x y) (max x y)) (take m $ (zip <*> tail) $ randomRs (1, n) $ mkStdGen 23)

doMain3 = do
    let n = 10000
    let numbers = take n $ randomRs (-n, n) $ mkStdGen 42
    let prefixSums = listArray (0, n) (scanl (+) 0 numbers)
    let m = 10*n
    let queries = map (\(x, y) -> ((min x y), (max x y))) $ take m $ (zip <*> tail) $ randomRs (1, n) $ mkStdGen 23
    mapM_ (print . getAnswerForPair prefixSums) queries -}

main = do
    start <- getCurrentTime
    _ <- doMain
    end <- getCurrentTime
    let timeDiff = diffUTCTime end start
    hPrintf stderr "\tTime inside the program: %.2f\n" (toDouble timeDiff)

    where
        toDouble :: (Real a) => a -> Double
        toDouble = fromRational . toRational
