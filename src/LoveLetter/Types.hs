{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module LoveLetter.Types where

import           Control.Lens
import           Control.Lens.TH
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.List.NonEmpty         as NEL
import           System.Random              (StdGen, newStdGen)

import           Control.Monad.State.Strict

data Card
  = Guard
  | Priest
  | Baron
  | Handmaid
  | Prince
  | King
  | Countess
  | Princess
    deriving (Show, Eq, Enum, Ord, Bounded)

data Player = Player
    { _playerName :: String
    , _playerCard :: Maybe Card
    , _playerProtected :: Bool
    }
    deriving (Show, Eq, Ord)

$(makeFields ''Player)

data Round = Round
    { _roundNumber :: Int
    , _roundLosers :: [Player]
    , _roundPlayers :: NonEmpty Player
    } deriving (Show, Eq)

$(makeFields ''Round)

-- Another way to do this: have an indexed data structure
-- and a list of indices to cycle through to determine players
data LoveLetter = LoveLetter
    { _loveLetterDeck           :: [Card]
    , _loveLetterPlayers        :: NonEmpty Player
    -- ^ current player is at the head of the list.
    , _loveLetterDiscardedCards :: [Card]
    , _loveLetterRound          :: Round
    , _loveLetterRNG            :: StdGen
    } deriving (Show)

$(makeFields ''LoveLetter)

type LoveLetterM m = (Monad m, MonadState LoveLetter m, MonadIO m)

_nelHead :: Lens' (NonEmpty a) a
_nelHead f (a :| as) = (:| as) <$> f a

currentPlayer :: Lens' LoveLetter Player
currentPlayer = players . _nelHead

-- index lens would be good

mkPlayer :: String -> Player
mkPlayer playerName = Player playerName Nothing False

-- Map over a player indexed by their name
mapPlayer :: String -> (Player -> Player) -> NonEmpty Player -> NonEmpty Player
mapPlayer playerName f = NEL.map (\p ->
        if p ^. name == playerName
            then f p
            else p)

-- TODO I would really really like an indexed lens by name here
