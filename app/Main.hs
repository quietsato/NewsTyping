module Main where

import           News
import           Typing
import           Control.Monad
import           Data.Fixed
import           Data.List
import           Data.Record
import           System.Environment
import           System.Exit
import           System.Random


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
        es <- takeRandom (n - 1) $ (take i as) ++ (drop (i + 1) as)
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
    parser _ = usage >> exitFailure

    usage = do
      pname <- getProgName
      putStrLn $ "Usage: " ++ pname ++ " [{empty}|{countOfGameStrings}|help]"

displayResult :: Result -> IO ()
displayResult r = do
  let t = counter r
      s = time r
      ac = getAllCount t
      cc = getCorrectCount t
      mc = getMissCount t
  mapM_ putStrLn [ "Typed:    " ++ show ac
                 , "Correct:  " ++ show cc
                 , "Missed:   " ++ show mc
                 , "Accuracy: " ++ show ((cc * 100) ./ ac) ++ "%"
                 , ""
                 , "Time:     " ++ show (toCenti s) ++ "s"
                 , "Speed:    " ++ show (cc ./ s) ++ "key/sec"]
  where
    (./) a b = toCenti a / toCenti b
    toCenti a = realToFrac a :: Centi