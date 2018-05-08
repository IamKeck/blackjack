module Lib where

import Data.Semigroup ((<>))
import System.Random
import Data.List (nub, sortOn)

newtype CardNum = CardNum {getNums :: Int} deriving (Eq, Ord)

instance Bounded CardNum where
  minBound = CardNum 1
  maxBound = CardNum 13

instance Show CardNum where
  show (CardNum 1) = "A"
  show (CardNum 11) = "J"
  show (CardNum 12) = "Q"
  show (CardNum 13) = "K"
  show (CardNum a) = show a

calcPointOne :: [CardNum] -> Int
calcPointOne = sum . map toPointOne

calcPointEleven :: [CardNum] -> Int
calcPointEleven = sum . map toPointEleven

toPointOne :: CardNum -> Int
toPointOne (CardNum a) = if a > 10 then 10 else a

toPointEleven :: CardNum -> Int
toPointEleven (CardNum 1) = 11
toPointEleven a = toPointOne a

data CardSuit = Spade | Heart | Diamond | Club deriving Eq

instance Show CardSuit where
  show Spade = "♠"
  show Heart = "♥"
  show Diamond = "◆"
  show Club = "♣"

data Card = Card {getSuit :: CardSuit, getNum :: CardNum} deriving Eq

instance Show Card where
  show (Card s n) = show s <> " " <>  show n

cardDeck :: [Card]
cardDeck = [Card s n|s <- [Spade, Heart, Diamond, Club], n <- map CardNum [1..13]]

shuffleDeck :: RandomGen g  => [Card] -> g -> [Card]
shuffleDeck deck gen =
  let
    cardSize = length deck
    orderNums = take cardSize . nub $ randomRs (1, cardSize) gen
  in
    map fst . sortOn snd $ zip deck orderNums

pickCard :: [Card] -> (Card, [Card])
pickCard xs = (head xs, tail xs)
