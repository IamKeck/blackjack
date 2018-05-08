module Main where

import Cards
import Player
import Game
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, void)
import System.Random
import Data.Semigroup ((<>))


mainGameLoop :: GameMonad Result
mainGameLoop = do
  liftIO . putStrLn $ "Hit(h) or Stand(s)"
  s <- liftIO  getLine
  case s of
    "h" -> do
      c <- youPick
      liftIO . putStrLn $ "You've picked " <> show c
      yp <- yourPoint
      case yp of
        Nothing -> return YouBust
        Just yp -> do
          liftIO . putStrLn $ "Your current point is:" <> show yp
          dp <- getCurrentDealersPoint
          when (dp < 17) $ void dealerPicks
          ndp <- dealersPoint
          maybe (return DealerBust) (\ndp' -> updateDealersPoint ndp' >> mainGameLoop) ndp

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
  yp <- yourPoint
  dp <- dealersPoint
  case (yp, dp) of
    (Just 21, Just 21) -> (liftIO . putStrLn $ "Natural Black Jack!") >> return Draw
    (Just 21, _) -> (liftIO . putStrLn $ "Natural Black Jack!") >> return YouWin
    (_, Just 21) -> (liftIO . putStrLn $ "Natural Black Jack!") >> return DealerWin
    (Just yp, Just dp) -> updateDealersPoint dp >> (liftIO . putStrLn $ "Your current point is:" <> show yp) >> mainGameLoop
    (_, _) -> (liftIO . putStrLn $ "Error!") >> return Draw


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
