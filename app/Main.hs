module Main where

import           News
import           Typing
import           Data.List
import           Data.Fixed
import           System.Random
import           Control.Monad
import           System.Environment
import           System.Exit

main :: IO ()
main = do
  n <- getCountOfGameStrings
  ts <- getTypingStrings
  ss <- takeRandom n ts
  g <- createGame ss
  r <- startGame g
  displayResult r
  where
    takeRandom n as
      | n <= 0 || null as = return []
      | otherwise = do
        i <- getStdRandom (randomR (0, length as - 1))
        let e = as !! i
        es <- takeRandom (n - 1) (delete e as)
        return (e:es)

getCountOfGameStrings :: IO Int
getCountOfGameStrings = do
  m <- parser =<< getArgs
  case m of
    Nothing -> return 3
    Just n  -> return n
  where
    parser [] = return Nothing
    parser [arg]
      | arg `isPrefixOf` "help" = usage >> exitSuccess
      | otherwise = return (Just (read arg :: Int))
    parser (_:_) = usage >> exitFailure

    usage = do
      pname <- getProgName
      putStrLn $ "Usage: " ++ pname ++ " [{empty}|{countOfGameStrings}|help]"

displayResult :: Result -> IO ()
displayResult r = do
  let t = typedData r
      s = time r
  putStrLn $ "Typed:    " ++ show (allCount t)
  putStrLn $ "Correct:  " ++ show (correctCount t)
  putStrLn $ "Missed:   " ++ show (missCount t)
  putStrLn $ "Accuracy: " ++ show ((correctCount t * 100) ./ allCount t) ++ "%"
  putStrLn ""
  putStrLn $ "Time:     " ++ show (toCenti s) ++ "s"
  putStrLn $ "Speed:    " ++ show (correctCount t ./ s) ++ "key/sec"
  where
    (./) :: (Real a, Real b) => a -> b -> Centi
    (./) a b = toCenti a / toCenti b

    toCenti a = realToFrac a :: Centi