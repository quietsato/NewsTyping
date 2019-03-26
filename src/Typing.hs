module Typing (createGame, startGame, Result(..), Typed(..)) where

import           System.Console.ANSI
import           System.IO
import           System.Timeout
import           Data.Time.Clock.System
import           Data.Ratio

data Game = Game { strs :: [String]
                 , remStrs :: [String]
                 , currentTypedStr :: String
                 , currentRemStr :: String
                 , nextChar :: Char
                 , consoleWidth :: Int
                 }

data Typed = Typed { allCount :: Int, correctCount :: Int, missCount :: Int }
  deriving Show

data Result = Result { time :: Double, typedData :: Typed }
  deriving Show

data GameState = Init
               | Correct
               | Miss
  deriving (Eq)

createGame :: [String] -> IO Game
createGame ss = do
  cw <- do
    s <- getTerminalSize
    case s of
      Nothing     -> return $ (maximum . map length) ss
      Just (_, c) -> return c
  return $ nextGameString $ Game ss ss "" "" ' ' cw

nextGameString :: Game -> Game
nextGameString g =
  g { remStrs = rs, currentTypedStr = "", currentRemStr = cs, nextChar = nc }
  where
    rs = (tail . remStrs) g

    (nc:cs) = (head . remStrs) g

nextGameChar :: Game -> Game
nextGameChar g = g { currentTypedStr = ts, currentRemStr = cs, nextChar = nc }
  where
    ts = currentTypedStr g ++ [nextChar g]

    (nc:cs) = currentRemStr g

initialTyped :: Typed
initialTyped = Typed 0 0 0

correctType :: Typed -> Typed
correctType t = t { allCount = ac, correctCount = cc }
  where
    ac = allCount t + 1

    cc = correctCount t + 1

missType :: Typed -> Typed
missType t = t { allCount = ac, missCount = mc }
  where
    ac = allCount t + 1

    mc = missCount t + 1

initGame :: IO ()
initGame = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  hSetEcho stdout False

startGame :: Game -> IO Result
startGame g = do
  initGame
  s <- (\x -> appendTime (systemSeconds x) (systemNanoseconds x))
    <$> getSystemTime
  t <- typing g Init initialTyped
  f <- (\x -> appendTime (systemSeconds x) (systemNanoseconds x))
    <$> getSystemTime
  return $ Result (f - s) t
  where
    appendTime :: (Show a, Show b) => a -> b -> Double
    appendTime sec nano = read (show sec ++ "." ++ show nano)

typing :: Game -> GameState -> Typed -> IO Typed
typing g s t = do
  display g s
  c <- getChar
  clear
  if c == nextChar g
    then finOrLoop g t
    else typing g Miss (missType t)
  where
    finOrLoop g t
      | didFinishGame g = return t
      | didFinCurrentStr g = typing (nextGameString g) Init (correctType t)
      | otherwise = typing (nextGameChar g) Correct (correctType t)

    didFinCurrentStr = null . currentRemStr

    didFinishGame g = (null . remStrs) g && didFinCurrentStr g

display :: Game -> GameState -> IO ()
display g s = do
  setTypedColor
  putStr displayTyped
  setNextCharColor s
  putChar $ nextChar g
  setRemColor
  putStr displayRem
  where
    displayTyped = drop (max (length typ - consoleWidth g `div` 2) 0) typ

    displayRem = take (consoleWidth g - length displayTyped - 1) rem

    typ = currentTypedStr g

    rem = currentRemStr g

    setNextCharColor s
      | s == Init = setSGR [SetColor Foreground Vivid Yellow]
      | s == Correct = setSGR [SetColor Foreground Vivid Green]
      | s == Miss = setSGR [SetColor Foreground Vivid Red]

    setTypedColor = setSGR [SetColor Foreground Vivid Black]

    setRemColor = setSGR [Reset]

clear :: IO ()
clear = do
  clearFromCursorToLineBeginning
  setCursorColumn 0

