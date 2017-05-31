{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Main where

import Control.Monad.State
import Control.Lens
import Control.Lens.TH
import Control.Monad.Loops

data Card
  = Guard
  | Priest
  | Baron
  | Handmaid
  | Prince
  | King
  | Countess
  | Princess
    deriving (Show, Eq, Enum, Ord)

type Description = String
type PlayerName = String

data Player = Player
  { _playerCard :: Card
  , _playerName :: PlayerName
  , _playerNumTokens :: Int
  } deriving (Show, Eq)

$(makeLenses ''Player)

data LoveLetter = LoveLetter
  { _loveLetterPlayers :: [Player]
  , _loveLetterEliminatedPlayers :: [Player]
  , _loveLetterCurrentPlayer :: PlayerName
  , _loveLetterDeck :: [Card]
  , _loveLetterTokensLeft :: Int
  , _loveLetterRoundOver :: Bool
  } deriving (Show, Eq)

$(makeLenses ''LoveLetter)

play :: LoveLetter -> Player -> Card -> IO LoveLetter
play game player card = undefined

drawCard :: Player -> [Card] -> IO (Card, LoveLetter)
drawCard = undefined

chooseCard :: Player -> Card -> Card -> IO Card
chooseCard player cardA cardB = undefined

turn :: LoveLetter -> Player -> IO LoveLetter
turn = undefined

playRound :: LoveLetter -> IO LoveLetter
playRound = undefined

loveLetter :: LoveLetter -> IO LoveLetter
loveLetter game = if gameWon then pure game else playRound game >>= loveLetter
  where
    winThreshold = case length (game ^. loveLetterPlayers) of
      2 -> 7
      3 -> 5
      4 -> 4
    gameWon = any (> winThreshold) $ map (^. playerNumTokens) (game ^. loveLetterPlayers)
    

effectDescription :: Card -> Description
effectDescription = \case
  Guard -> "Guess a player's hand"
  Priest -> "Look at a hand"
  Baron -> "Compare hands; lower hand is out"
  Handmaid -> "Protection until your next turn"
  Prince -> "One player discards his or her hand"
  King -> "Trade hands"
  Countess -> "Discard if caught with King or Prince"
  Princess -> "Lose if discarded"

main :: IO ()
main = do
  putStrLn "hello world"
