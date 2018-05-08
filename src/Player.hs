module Player where

import Cards

newtype Player = Player {getCards :: [Card]} deriving Show

point :: Player -> Maybe Int
point (Player xs) =
  let
    low = calcPoint toPointOne . map getNum $ xs
    high = calcPoint toPointEleven . map getNum $ xs
  in
    if low > 21 && high > 21 then Nothing
    else if high > 21 then Just low
    else Just high

addCard :: Player -> Card -> Player
addCard (Player xs) c = Player $ c:xs
