module Main where

import           News
import           Typing
import           Data.Time.Clock
import           System.IO
import           Data.Fixed
import           Data.Ratio
import           System.Random
import           Control.Monad

main :: IO ()
main = do
  ts <- getTypingStrings
  ss <- replicateM 3 $ takeRandom ts
  g <- createGame ss
  r <- startGame g
  displayResult r
  where
    takeRandom as = do
      i <- getStdRandom (randomR (0, length as - 1))
      return $ as !! i


displayResult :: Result -> IO ()
displayResult r = do
  let t = typedData r
      s = time r

  putStrLn $ "Typed:    " ++ show (allCount t)
  putStrLn $ "Correct:  " ++ show (correctCount t)
  putStrLn $ "Missed:   " ++ show (missCount t)
  putStrLn $ "Accuracy: " ++ show ((correctCount t ./ allCount t) * 100) ++ "%"
  putStrLn ""
  putStrLn $ "Time:     " ++ show s ++ "s"
  putStrLn $ "Speed:    " ++ show (correctCount t ./ s) ++ "key/sec"

(./) :: (Real a, Real b) => a -> b -> Milli
(./) a b = toMilli a / toMilli b
  where
    toMilli a = realToFrac a :: Milli