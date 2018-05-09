
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Game where

import Control.Monad.State.Strict (StateT(runStateT), get, modify)
import Control.Monad.State.Class (MonadState)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except (ExceptT, MonadError(throwError), runExceptT)
import Data.Semigroup ((<>))
import Cards
import Player

data GameState = GameState {deck :: [Card],
                            dealer :: Player,
                            you :: Player,
                            currentDealerPoint :: Int}

newtype GameMonad a = GameMonad (ExceptT Result (StateT GameState IO) a) deriving (
  Functor,
  Applicative,
  Monad,
  MonadState GameState,
  MonadIO,
  MonadError Result
  )

data Result = DealerWin | YouWin | YouBust | DealerBust | Draw deriving Show

runGame :: GameMonad Result -> GameState -> IO (Result, GameState)
runGame (GameMonad m) s = do
    (mr, s) <- runStateT (runExceptT m) s
    case mr of
        Left r -> return (r, s)
        Right r -> return (r, s)

yourPoint :: GameMonad (Maybe Int)
yourPoint = point . you <$> get

dealersPoint :: GameMonad (Maybe Int)
dealersPoint = point . dealer <$> get

pickCard :: GameMonad Card
pickCard = do
  (newCard, remainingCard) <- Cards.pickCard . deck <$> get
  modify (\s -> s {deck = remainingCard})
  return newCard

youPick :: GameMonad Card
youPick = do
  newCard <- Game.pickCard
  modify (\s -> s {you = addCard (you s) newCard})
  return newCard

dealerPicks :: GameMonad Card
dealerPicks = do
  newCard <- Game.pickCard
  modify (\s -> s {dealer = addCard (dealer s) newCard})
  return newCard

judge :: GameMonad Result
judge = do
  yp <-  yourPoint
  dp <- dealersPoint
  case (yp, dp) of
    (Nothing, _) -> return YouBust
    (_, Nothing) -> return DealerBust
    (Just yp, Just dp)
      | yp == dp -> return Draw
      | yp > dp -> return YouWin
      | otherwise -> return DealerWin

updateDealersPoint :: Int -> GameMonad ()
updateDealersPoint p = modify (\s -> s {currentDealerPoint = p})

getCurrentDealersPoint :: GameMonad Int
getCurrentDealersPoint = currentDealerPoint <$> get
