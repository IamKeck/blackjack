module Player where

import Cards

newtype Player = Player {getCards :: [Card]} deriving Show

point :: Player -> (Int, Int)
point (Player xs) = (calcPoint toPointOne . map getNum $ xs, calcPoint toPointEleven . map getNum $ xs)

addCard :: Player -> Card -> Player
addCard (Player xs) c = Player $ c:xs
