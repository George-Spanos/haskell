{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module RandomExample2 (main) where

import Control.Applicative (liftA3)
import Control.Monad.Trans.State (State, evalState, state)
import RandomExample (Die, intToDie)
import System.Random (Random (randomR), StdGen, mkStdGen)

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = do
  intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie' rollDie' rollDie'

main :: IO ()
main = do
  print $ evalState rollDieThreeTimes' $ mkStdGen 4
