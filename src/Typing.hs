module Typing (createGame, startGame) where

import           Config
import           Data.Record
import           Control.Concurrent
import           Data.Time.Clock.System
import           System.Console.ANSI
import           System.Exit
import           System.IO
import           System.IO.NoBufferingWorkaround
import           System.Timeout


data Game = Game { remStrs :: [String]
                 , currentTypedStr :: String
                 , currentRemStr :: String
                 , nextChar :: Char
                 , consoleWidth :: Int
                 }

createGame :: [String] -> IO Game
createGame ss = do
  cw <- do
    s <- getTerminalSize
    w <- case s of
      Nothing     -> return $ (maximum . map length) ss
      Just (_, c) -> return c
    c <- isFixConsoleWidth
    if c
      then return $ w - 1
      else return w
  let
    ((nc:rs):rss) = ss
    ts = ""
  return $ Game rss ts rs nc cw

startGame :: Game -> IO Result
startGame g = do
  initIO
  countDown 3
  s <- (\x -> appendTime (systemSeconds x) (systemNanoseconds x))
    <$> getSystemTime
  t <- typing g newCounter
  f <- (\x -> appendTime (systemSeconds x) (systemNanoseconds x))
    <$> getSystemTime
  return $ Result (f - s) t
  where
    initIO = do
      hSetBuffering stdout NoBuffering
      hSetEcho stdout False
      initGetCharNoBuffering
    
    countDown s
      | s == 0 = return ()
      | otherwise = do
        putStr $ show s
        x <- timeout (2 * sec) (threadDelay sec *> clear)
        case x of
          Just _  -> countDown (s - 1)
          Nothing -> return ()
      where
        sec = 1000000

    appendTime sec nano = read (show sec ++ "." ++ show nano) :: Double

typing :: Game -> TypeCounter -> IO TypeCounter
typing g t = do
  display g (getTypingStatus t)
  c <- getCharNoBuffering
  clear *> loop c g t
  where
  loop c g t
    | c == '\ESC'     = exitSuccess
    | c == nextChar g = continueGame g t
    | otherwise       = typing g $ countMiss t

  continueGame g t
    | didFinishGame g    = return t
    | didFinCurrentStr g = typing (nextGameString g) $ countCorrect t
    | otherwise          = typing (nextGameChar   g) $ countCorrect t

  didFinCurrentStr = null . currentRemStr

  didFinishGame g = (null . remStrs) g && didFinCurrentStr g
  
  nextGameChar g =
    let
      Game _ ts rs nc _ = g
      ts' = ts ++ [nc]
      (nc':cs') = rs
    in
      g { currentTypedStr = ts', currentRemStr = cs', nextChar = nc' }

  nextGameString g =
    let
      ts' = ""
      ((nc':cs'):rss') = remStrs g
    in 
      g { remStrs = rss', currentTypedStr = ts', currentRemStr = cs', nextChar = nc' }

display :: Game -> Status -> IO ()
display g s = do
  let
    typ = currentTypedStr g
    rem = currentRemStr g
    dTyp = drop (max (length typ - consoleWidth g `div` 2) 0) typ
    dChr = nextChar g
    dRem = take (consoleWidth g - length dTyp - 1) rem
     
  setTypedColor      *> putStr  dTyp 
  setNextCharColor s *> putChar dChr
  setRemColor        *> putStr  dRem
  where
    setNextCharColor s = do
      setDefaultColor
      case s of
        None    -> setSGR [SetColor Foreground Vivid Yellow]
        Correct -> setSGR [SetColor Foreground Vivid Green]
        Miss    -> setSGR [SetColor Background Vivid Red]

    setTypedColor = setSGR [SetColor Foreground Vivid Black]

    setDefaultColor = setSGR [Reset]

    setRemColor = setDefaultColor

clear :: IO ()
clear = do
  clearFromCursorToLineBeginning
  setCursorColumn 0