module Main where

import qualified System.Random as Rnd
import qualified Data.Stream as Stream
import Control.Arrow
import Control.Arrow.Transformer.Automaton(Automaton(..),runAutomaton)
import Control.Arrow.Operations(delay)
import Data.List(intersperse)
import Control.Monad (forM_)
import Control.Concurrent (threadDelay)


data Call = Zun | Doko deriving(Enum,Bounded,Show) -- input data
instance Rnd.Random Call where
  randomR (a,b) g = 
    case Rnd.randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  
  random = Rnd.randomR (minBound,maxBound)


newtype Call' = Call' Call -- type for print
instance Show Call' where
  show (Call' Zun) = "ズン"
  show (Call' Doko) = "ドコ"


data State = Neutral | CountingZun Int -- state data

zunDokoKiyoshi :: (ArrowLoop a, ArrowApply a) => Automaton a Call String
zunDokoKiyoshi = neutral
  where
    neutral = Automaton $ arr (show . Call') &&& arr (next Neutral)
    countingZun n = Automaton $ arr (show . Call') &&& arr (next (CountingZun n))

    next Neutral Zun  = countingZun 1
    next Neutral Doko = neutral
    next (CountingZun n) Zun = countingZun (n+1)
    next (CountingZun n) Doko | n == 4    = neutral >>> delay "キヨシ!" -- insert
                              | otherwise = neutral

runZDK :: Stream.Stream Call -> Stream.Stream String
runZDK xs = (runAutomaton $ arr snd >>> zunDokoKiyoshi) ((),xs)



test :: IO ()
test = putStrLn $ mconcat $ intersperse "、" $
       take 8 $ Stream.toList $ runZDK $ Stream.fromList $ [Zun,Zun,Zun,Zun,Doko] <> (repeat Zun)

randomZDK :: IO [String]
randomZDK = do
  rg <- Rnd.newStdGen
  return $ Stream.toList $ runZDK $ Stream.fromList $ Rnd.randoms rg



main :: IO ()
main = do
  xs <- randomZDK
  forM_ xs $ \x -> do
    putStrLn x
    threadDelay $ 500 * 1000
