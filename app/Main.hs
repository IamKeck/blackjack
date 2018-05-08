{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Cards
import Player
import Control.Monad.State.Strict
import Control.Monad.State.Class (MonadState)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (when, void)
import System.Random
import Data.Semigroup ((<>))

data GameState = GameState {deck :: [Card], dealer :: Player, you :: Player, currentDealerPoint :: Int}

newtype GameMonad a = GameMonad {unGame :: StateT GameState IO a} deriving (
  Functor,
  Applicative,
  Monad,
  MonadState GameState,
  MonadIO
  )

runGame :: GameMonad a -> GameState -> IO (a, GameState)
runGame m = runStateT (unGame m)

yourPoint :: GameMonad (Int, Int)
yourPoint = point . you <$> get

dealersPoint :: GameMonad (Int, Int)
dealersPoint = point . dealer <$> get

pickCard :: GameMonad Card
pickCard = do
  (newCard, remainingCard) <- Cards.pickCard . deck <$> get
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

updateDealersPoint :: Int -> GameMonad ()
updateDealersPoint p = modify (\s -> s {currentDealerPoint = p})


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
          liftIO . putStrLn $ "Your current point is:" <> show yp
          dp <- currentDealerPoint <$> get
          when (dp < 17) $ void dealerPicks
          vdp' <- getValidPoint <$> dealersPoint
          maybe (return DealerBust) (\dp' -> updateDealersPoint dp >> mainGameLoop) vdp'

    "s" -> judge
    _ -> (liftIO . putStrLn $ "Input h or s") >> mainGameLoop

mainGame :: GameMonad Result
mainGame = do
  c <- dealerPicks
  liftIO . putStrLn $ "Dealer's first card is " <> show c
  c <- youPick
  liftIO . putStrLn $ "Your first card is " <> show c
  dealerPicks
  c <- youPick
  liftIO . putStrLn $ "Your second card is " <> show c
  vyp <- getValidPoint <$> yourPoint
  vdp <- getValidPoint <$> dealersPoint
  case (vyp, vdp) of
    (Just 21, Just 21) -> (liftIO . putStrLn $ "Natural Black Jack!") >> return Draw
    (Just 21, _) -> (liftIO . putStrLn $ "Natural Black Jack!") >> return YouWin
    (_, Just 21) -> (liftIO . putStrLn $ "Natural Black Jack!") >> return DealerWin
    (Just yp, Just dp) -> updateDealersPoint dp >> (liftIO . putStrLn $ "Your current point is:" <> show yp) >> mainGameLoop
    (_, _) -> (liftIO . putStrLn $ "Error!") >> return Draw


main :: IO ()
main = do
  putStrLn "welcome to Black Jack"
  g <- getStdGen
  let shuffledDeck  = shuffleDeck cardDeck g
  let initialState = GameState shuffledDeck (Player []) (Player []) 0
  (result, state) <- runGame mainGame initialState
  putStrLn $ show result <> "!"
  putStrLn $ "your cards:" <> (showCards . you $ state)
  putStrLn $ "dealer's cards:" <> (showCards . dealer $ state)
  where
    showCards = show . reverse . getCards
