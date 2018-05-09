module Main where

import Cards
import Player
import Game
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, void)
import System.Random
import Data.Semigroup ((<>))

dealersTurn :: GameMonad ()
dealersTurn = do
  dp <- getCurrentDealersPoint
  when (dp < 17) $ void dealerPicks
  ndp <- dealersPoint
  case ndp of
    Nothing -> finishGame DealerBust
    Just _ -> return ()

yourTurn :: GameMonad ()
yourTurn = do
  liftIO . putStrLn $ "Hit(h) or Stand(s)"
  s <- liftIO  getLine
  case s of
    "h" -> do
      c <- youPick
      liftIO . putStrLn . ("You've picked " <>) . show $ c
      yp <- yourPoint
      case yp of
        Nothing -> finishGame YouBust
        Just yp -> do
          liftIO . putStrLn . ("Your current point is:" <>) . show $ yp
          yourTurn

    "s" -> return ()
    _ -> (liftIO . putStrLn $ "Input h or s") >> yourTurn

dealCard :: GameMonad ()
dealCard = do
  dealerPicks >>= liftIO . putStrLn . ("Dealer's first card is " <>) . show
  youPick >>= liftIO . putStrLn . ("Your first card is " <>) . show
  dealerPicks
  youPick >>= liftIO . putStrLn . ("Your second card is " <>) . show
  yp <- yourPoint
  dp <- dealersPoint
  case (yp, dp) of
    (Just 21, Just 21) -> (liftIO . putStrLn $ "Natural Black Jack!") >>  finishGame Draw
    (Just 21, _) -> (liftIO . putStrLn $ "Natural Black Jack!") >> finishGame YouWin
    (_, Just 21) -> (liftIO . putStrLn $ "Natural Black Jack!") >> finishGame DealerWin
    (Just yp, Just dp) -> updateDealersPoint dp >> (liftIO . putStrLn $ "Your current point is:" <> show yp) >> return ()
    (_, _) -> (liftIO . putStrLn $ "Error!") >> finishGame Draw

mainGame :: GameMonad Result
mainGame = do
    dealCard
    yourTurn
    dealersTurn
    judge



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
