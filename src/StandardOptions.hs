module StandardOptions where
import Control.Monad.State
import Engine
import Items

standardOptions :: GameState -> [(String, State GameState String)]
standardOptions gs | elem Booze $ inventory gs
  = ("drink!", do put $ moveTo "the end" gs
                  locStr) :
    if not $ null $ inventory gs
    then [("inventory", return "You just found a bug!")]
    else []
standardOptions gs | not $ null $ inventory gs
  = [("inventory", return "You just found a bug!")]
standardOptions _ = []