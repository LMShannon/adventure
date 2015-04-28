module Main where
import Control.Monad.State
import Engine
import Locations

main :: IO ()
main = do
  putStr "\n"
  fst $ runState (process "")
                 GameState { currLoc = locations ! "Start",
                             locs = locations,
                             inventory = [] }