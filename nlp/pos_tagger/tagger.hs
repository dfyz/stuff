import Control.Monad
import Data.Maybe
import qualified System.IO as IO
import qualified Data.List as L
import qualified Data.Map as M

type Token = String
type Tag = String

data TrainingInstance = TrainingInstance Token Tag
                        deriving Show

rsplit :: Eq a => a -> [a] -> ([a], [a])
rsplit sep l = let (ps, xs, _) = rsplit_ sep l in
               (ps, xs)

rsplit_ :: Eq a => a -> [a] -> ([a], [a], Bool)
rsplit_ sep = foldr (splitFun sep) ([], [], False)
    where splitFun sep e (px, xs, True) = (e:px, xs, True)
          splitFun sep e (px, xs, False)
                   | e == sep = (px, xs, True)
                   | otherwise = (px, e:xs, False)

toTrainingInstance :: String -> TrainingInstance
toTrainingInstance s = let (token, tag) = rsplit '/' s in TrainingInstance token tag

tokenTagFreqs :: [TrainingInstance] -> M.Map Token (M.Map Tag Int)
tokenTagFreqs = L.foldl' countWord M.empty
    where
      countWord m (TrainingInstance token tag) = 
          M.insertWith (countTag tag) token (M.singleton tag 1) m
      countTag tag _ old = M.insertWith
          (\newFreq oldFreq -> oldFreq + newFreq) tag 1 old

tokenMostFreqTag :: M.Map Token (M.Map Tag Int) -> M.Map Token Tag
tokenMostFreqTag = M.map mostFreqTag
    where
      mostFreqTag = fst . M.foldlWithKey maxTag ("???", 0)
      maxTag acc@(maxTag, maxFreq) tag freq
                 | freq > maxFreq = (tag, freq)
                 | otherwise = acc

trainFreqTagger :: [TrainingInstance] -> M.Map Token Tag
trainFreqTagger = tokenMostFreqTag . tokenTagFreqs

freqTagWord :: M.Map Token Tag -> Token -> Maybe Tag
freqTagWord m t = M.lookup t m

main = do
	h <- IO.openFile "nlpwp-data/brown-pos-train.txt" IO.ReadMode
	c <- IO.hGetContents h
	let model = trainFreqTagger $ map toTrainingInstance $ words c
	i <- IO.openFile "nlpwp-data/brown-pos-test.txt" IO.ReadMode
	d <- IO.hGetContents i
	forM (map toTrainingInstance $ words d) $ \(TrainingInstance token correctTag) -> do
		putStrLn $ (token ++ "/" ++ (fromMaybe "???" (freqTagWord model token)))