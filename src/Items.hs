module Items where

data Item = ExPirate
          | Parrot
          | Booze
  deriving (Eq)
instance Show Item where
  show ExPirate = "ex-pirate"
  show Parrot = "parrot"
  show Booze = "booze"
instance Read Item where
  readsPrec _ "ex-pirate" = [(ExPirate,"")]
  readsPrec _ "parrot" = [(Parrot,"")]
  readsPrec _ "booze" = [(Booze,"")]
  readsPrec _ _ = []

indefiniteArticle ExPirate = "an " -- "There is an ex-pirate here."
indefiniteArticle Booze = "" --"There is booze here"
indefiniteArticle _ = "a " --"There is a parrot here"