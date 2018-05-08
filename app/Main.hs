module Main where

import Lib
import Player
import Control.Monad.State.Strict
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, void)
import System.Random
import Data.Semigroup ((<>))

data GameState = GameState {deck :: [Card], dealer :: Player, you :: Player, currentDealerPoint :: Int}

type GameMonad a = StateT GameState IO a

yourPoint :: GameMonad (Int, Int)
yourPoint = point . you <$> get

dealersPoint :: GameMonad (Int, Int)
dealersPoint = point . dealer <$> get

pickCard :: GameMonad Card
pickCard = do
  (newCard, remainingCard) <- Lib.pickCard . deck <$> get
  modify (\s -> s {deck = remainingCard})
  return newCard

youPick :: GameMonad Card
youPick = do
  newCard <- Main.pickCard
  modify (\s -> s {you = addCard (you s) newCard})
  return newCard

dealerPicks :: GameMonad Card
dealerPicks = do
  newCard <- Main.pickCard
  modify (\s -> s {dealer = addCard (dealer s) newCard})
  return newCard


getValidPoint :: (Int, Int) -> Maybe Int
getValidPoint (l, h)
  | l > 21 && h > 21 = Nothing
  | h > 21 = Just l
  | otherwise = Just h


judge :: GameMonad Result
judge = do
  yourValidPoint <- getValidPoint <$> yourPoint
  dealersValidPoint <- getValidPoint <$> dealersPoint
  case (yourValidPoint, dealersValidPoint) of
    (Nothing, _) -> return YouBust
    (_, Nothing) -> return DealerBust
    (Just yp, Just dp)
      | yp == dp -> return Draw
      | yp > dp -> return YouWin
      | otherwise -> return DealerWin

data Result = DealerWin | YouWin | YouBust | DealerBust | Draw deriving Show

mainGameLoop :: GameMonad Result
mainGameLoop = do
  liftIO . putStrLn $ "Hit(h) or Stand(s)"
  s <- liftIO  getLine
  case s of
    "h" -> do
      c <- youPick
      liftIO . putStrLn $ "You've picked " <> show c
      vyp <- getValidPoint <$> yourPoint
      case vyp of
        Nothing -> return YouBust
        Just yp -> do
          dp <- currentDealerPoint <$> get
          when (dp < 17) $ void dealerPicks
          vdp' <- getValidPoint <$> dealersPoint
          maybe (return DealerBust) (\dp' -> modify (\s -> s {currentDealerPoint = dp'}) >> mainGameLoop) vdp'

    "s" -> judge
    _ -> mainGameLoop

mainGame :: GameMonad Result
mainGame = do
  c <- dealerPicks
  liftIO . putStrLn $ "Dealer's first card is " <> show c
  c <- youPick
  liftIO . putStrLn $ "Your first card is " <> show c
  dealerPicks
  c <- youPick
  liftIO . putStrLn $ "Your second card is " <> show c
  yp <- getValidPoint <$> yourPoint
  vdp <- getValidPoint <$> dealersPoint
  if yp == Just 21 then do
    liftIO . putStrLn $ "Natural Black Jack!"
    return YouWin
  else
    case vdp of
      Nothing -> return DealerBust
      Just p -> modify (\s -> s {currentDealerPoint = p}) >> mainGameLoop


main :: IO ()
main = do
  putStrLn "welcome to Black Jack"
  g <- getStdGen
  let shuffledDeck  = shuffleDeck cardDeck g
  let initialState = GameState shuffledDeck (Player []) (Player []) 0
  (result, state) <- runStateT mainGame initialState
  putStrLn $ show result <> "!"
  putStrLn $ "your cards:" <> (showCards . you $ state)
  putStrLn $ "dealer's cards:" <> (showCards . dealer $ state)
  where
    showCards = show . reverse . getCards
