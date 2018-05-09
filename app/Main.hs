module Main where

import Cards
import Player
import Game
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, void)
import System.Random
import Data.Semigroup ((<>))

dealersTurn :: GameMonad (Maybe Result)
dealersTurn = do
  dp <- getCurrentDealersPoint
  when (dp < 17) $ void dealerPicks
  ndp <- dealersPoint
  case ndp of
    Nothing -> return (Just DealerBust)
    Just _ -> return Nothing

yourTurn :: GameMonad (Maybe Result)
yourTurn = do
  liftIO . putStrLn $ "Hit(h) or Stand(s)"
  s <- liftIO  getLine
  case s of
    "h" -> do
      c <- youPick
      liftIO . putStrLn $ "You've picked " <> show c
      yp <- yourPoint
      case yp of
        Nothing -> return (Just YouBust)
        Just yp -> do
          liftIO . putStrLn $ "Your current point is:" <> show yp
          yourTurn

    "s" -> return Nothing
    _ -> (liftIO . putStrLn $ "Input h or s") >> yourTurn

dealCard :: GameMonad (Maybe Result)
dealCard = do
  c <- dealerPicks
  liftIO . putStrLn $ "Dealer's first card is " <> show c
  c <- youPick
  liftIO . putStrLn $ "Your first card is " <> show c
  dealerPicks
  c <- youPick
  liftIO . putStrLn $ "Your second card is " <> show c
  yp <- yourPoint
  dp <- dealersPoint
  case (yp, dp) of
    (Just 21, Just 21) -> (liftIO . putStrLn $ "Natural Black Jack!") >> return (Just Draw)
    (Just 21, _) -> (liftIO . putStrLn $ "Natural Black Jack!") >> return (Just YouWin)
    (_, Just 21) -> (liftIO . putStrLn $ "Natural Black Jack!") >> return (Just DealerWin)
    (Just yp, Just dp) -> updateDealersPoint dp >> (liftIO . putStrLn $ "Your current point is:" <> show yp) >> return Nothing
    (_, _) -> (liftIO . putStrLn $ "Error!") >> return (Just Draw)

mainGame :: GameMonad Result
mainGame = do
  mr <- dealCard
  case mr of
    Just r -> return r
    Nothing -> do
      mr' <- yourTurn
      case mr' of
        Just r' -> return r'
        Nothing -> do
          mr'' <- dealersTurn
          case mr'' of
            Just r'' -> return r''
            Nothing -> judge



main :: IO ()
main = do
  putStrLn "welcome to Black Jack"
  shuffledDeck <- shuffleDeck cardDeck <$> getStdGen
  let initialState = GameState shuffledDeck (Player []) (Player []) 0
  (result, state) <- runGame mainGame initialState
  putStrLn $ show result <> "!"
  putStrLn $ "your cards:" <> (showCards . you $ state)
  putStrLn $ "dealer's cards:" <> (showCards . dealer $ state)
  where
    showCards = show . reverse . getCards
