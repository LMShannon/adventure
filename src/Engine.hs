module Engine where
import Control.Monad.State
import Data.List
import System.IO
import Items

data GameState = GameState {
  currLoc :: Location,
  locs :: [Location],
  inventory :: [Item]}

data Location = Location {
    name :: String,
    description :: String,
    items :: [Item],
    options :: [(String, String)],
    otherOptions :: GameState->[(String, State GameState String)]}

(!) :: [Location] -> String -> Location
(loc : locs) ! locname | name loc == locname = loc
(_ : locs) ! locname = locs ! locname
[] ! locname = error $ "Location " ++ locname ++ " not found"

instance Show Location where
  show loc = intercalate "\n\n" $
    --name loc :
    description loc :
    ["There is " ++ indefiniteArticle it ++
     show it ++ " here." | it <- items loc]

locStr = do
  gameState <- get
  let loc = currLoc gameState
  return $
    "\n" ++
     show loc ++
     "\n\n[" ++
     intercalate
       "/"
       ((map fst $ options loc) ++
        (map fst $ otherOptions loc gameState) ++
        (map (("take "++).show) (items loc))) ++
     "]"

moveTo :: String -> GameState -> GameState
moveTo loc gameState =
  gameState {currLoc = locs gameState ! loc}

swap :: (a->Bool) -> a -> [a] -> [a]
swap pred a lst = [if pred x then a else x | x <- lst]

takeItem :: Item -> GameState -> GameState
takeItem it gs = gs {
    inventory = it : inventory gs,
    currLoc = updatedCurrLoc,
    locs = swap ((==(name currLoc')).name) updatedCurrLoc (locs gs)}
  where currLoc' = currLoc gs
        updatedCurrLoc = currLoc' { items = filter (/=it) (items currLoc')}

next :: String -> State GameState (IO ())
next resultStr = do
  newGameState <- get
  return $ do putStr $ resultStr ++ "\n> "
              hFlush stdout
              processNextInput newGameState

process :: String -> State GameState (IO ())
process "" = do
  gameState <- get
  resultStr <- locStr
  next $ resultStr
process "inventory" = do
  gameState <- get
  describe <- locStr
  let resultStr = intercalate "\n" ("you are carrying:" :
                                    map show (inventory gameState))
                  ++ "\n\n" ++ describe
  next resultStr
process ('t':'a':'k':'e':' ':it) | (not $ null $ parse) &&
                                   (null $ snd $ head parse)
  = do
    gameState <- get
    resultStr <- maybe
      (return "i don't know how to take that.")
      (\_ -> do put $ takeItem item gameState
                return $ "put " ++ it ++ " into inventory.")
      (elemIndex item $ items $ currLoc $ gameState)
    describe <- locStr
    next $ resultStr ++ "\n\n" ++ describe
  where parse = reads it :: [(Item, String)]
        item = fst $ head parse
process input = do
    gameState <- get
    let oo = map fst $ (otherOptions $ currLoc gameState) gameState
    resultStr <-
      if elem input oo
      then maybe
             (return "i don't know what you mean.")
             id
             (lookup input $ (otherOptions $ currLoc $ gameState) gameState)
      else maybe
             (return "i don't know what you mean.")
             (\option -> do put $ moveTo option gameState
                            locStr)
             (lookup input $ options $ currLoc $ gameState)
    next resultStr

--Get input, then pass it to process with the game state set
processNextInput gameState = do
  in' <- getLine
  fst $ runState (process in') gameState
