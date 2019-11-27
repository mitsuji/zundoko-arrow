module Main where

import Control.Arrow
import Control.Arrow.Transformer.Automaton(Automaton(..),runAutomaton)
import Control.Arrow.Operations(delay)
import qualified Data.Stream as Stream
import qualified System.Random as Rnd
import Control.Monad (forM_)
import Control.Concurrent (threadDelay)



data Call = Zun | Doko | Kiyoshi
          deriving(Enum,Show)

instance Rnd.Random Call where
  randomR (a,b) g = 
    case Rnd.randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  
  random = Rnd.randomR (Zun,Doko) -- avoid Kiyoshi



data State = Neutral | CountingZun Int

zunDokoKiyoshi :: Automaton (->) Call Call
zunDokoKiyoshi = neutral
  where
    neutral :: Automaton (->) Call Call
    neutral = Automaton $ returnA &&& next Neutral

    countingZun :: Int -> Automaton (->) Call Call
    countingZun n = Automaton $ returnA &&& next (CountingZun n)

    next :: State -> Call -> Automaton (->) Call Call
    next Neutral Zun  = countingZun 1
    next Neutral Doko = neutral
    next (CountingZun n) Zun  | n == 4    = neutral
                              | otherwise = countingZun (n+1)
    next (CountingZun n) Doko | n == 4    = neutral >>> delay Kiyoshi -- insert Kiyoshi
                              | otherwise = neutral

runZDK :: Stream.Stream Call -> Stream.Stream Call
runZDK xs = (runAutomaton $ arr snd >>> zunDokoKiyoshi) ((),xs)



test :: [Call]
test = take 8 $ Stream.toList $ runZDK $ Stream.fromList $ [Zun,Zun,Zun,Zun,Doko] <> (repeat Zun)

randomZDK :: IO [Call]
randomZDK = do
  rg <- Rnd.newStdGen
  return $ Stream.toList $ runZDK $ Stream.fromList $ Rnd.randoms rg



main :: IO ()
main = do
  xs <- randomZDK
  forM_ xs $ \x -> do
    print x
    threadDelay $ 100 * 1000
