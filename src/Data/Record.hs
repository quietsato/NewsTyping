module Data.Record
    ( Result(..)
    , TypeCounter
    , Status(..)
    , newCounter
    , getAllCount
    , getCorrectCount
    , getMissCount
    , getTypingStatus
    , countCorrect
    , countMiss) where

data TypeCounter =
  MkCounter { correct :: Int, miss :: Int, typingStatus :: Status }
  deriving Show

data Result = Result { time :: Double, counter :: TypeCounter }
  deriving Show

data Status = None | Correct | Miss
  deriving Show

newCounter :: TypeCounter
newCounter = MkCounter 0 0 None

getAllCount :: TypeCounter -> Int
getAllCount t = (correct t) + (miss t)

getCorrectCount :: TypeCounter -> Int
getCorrectCount = correct

getMissCount :: TypeCounter -> Int
getMissCount = miss

getTypingStatus :: TypeCounter -> Status
getTypingStatus = typingStatus

countCorrect :: TypeCounter -> TypeCounter
countCorrect c = c { correct = correct c + 1, typingStatus = Correct }

countMiss :: TypeCounter -> TypeCounter
countMiss c = c { miss = miss c + 1 , typingStatus = Miss }