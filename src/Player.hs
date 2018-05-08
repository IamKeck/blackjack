module Player where

import Lib

newtype Player = Player {getCards :: [Card]} deriving Show

point :: Player -> (Int, Int)
point (Player xs) = (calcPointOne . map getNum $ xs, calcPointEleven . map getNum $ xs)

addCard :: Player -> Card -> Player
addCard (Player xs) c = Player $ c:xs
