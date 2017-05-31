{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module LoveLetter where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Extra
import           Control.Monad.Loops
import           Control.Monad.State.Strict
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.List.NonEmpty         as NEL
import           Data.Monoid
import           Data.Random
import           Prelude                    hiding (round)
import           System.Random              (StdGen, newStdGen)

import           LoveLetter.Types

discardDescription :: Card -> String
discardDescription = \case
  Guard -> "must guess another player's hand."
  Priest -> "must look at someone else's hand."
  Baron -> "must compare hands; the lower hand is eliminated."
  Handmaid -> "has protection until their next turn."
  Prince -> "chooses a player to discard their hand."
  King -> "must trade hands with another player."
  Countess -> "must discard the Countess if caught with King or Prince"
  Princess -> "loses."

-- | Get the next Player and cycle the turn.
nextPlayer :: LoveLetterM m => m (Maybe Player)
nextPlayer = do
    p :| xs <- use players
    case xs of
        [] -> pure Nothing
        (player':xs') ->
            let newPlayers = player' :| (xs' ++ [p])
            in do
                players .= newPlayers
                pure $ Just player'

-- Put current player last and a new player first
setCurrentPlayer :: LoveLetterM m => Player -> m Player
setCurrentPlayer p = do
    p' :| xs <- use players
    players .= p :| (filter (/= p) xs) ++ [p']
    pure p
{-# ANN setCurrentPlayer "HLint: ignore" #-}

runLoveLetter :: s -> StateT s m a -> m (a, s)
runLoveLetter = flip runStateT

initialState :: StdGen -> NEL.NonEmpty Player -> LoveLetter
initialState gen ps =
    LoveLetter
        [ Princess
        , Countess
        , King
        , Prince, Prince
        , Handmaid, Handmaid
        , Baron, Baron
        , Priest, Priest
        , Guard, Guard, Guard, Guard, Guard]
        ps
        []
        0
        gen

-- Static for now
gatherPlayers :: MonadIO m => m (NonEmpty Player)
gatherPlayers = pure . NEL.fromList $ map mkPlayer ["Ben", "Amanda", "Bozo The Clown"]

gameIsWon = undefined

rng :: LoveLetterM m => RVar a -> m a
rng var = do
    game@LoveLetter{..} <- get
    let (r, stdGen') = sampleState var _loveLetterRNG
    put game{ _loveLetterRNG = stdGen' }
    pure r

shuffleCards :: LoveLetterM m => m ()
shuffleCards = do
    deck <- rng . shuffle =<< gets _loveLetterDeck
    modify (\g -> g{ _loveLetterDeck = deck })
    liftIO $ print deck

removeTopCard :: LoveLetterM m => m ()
removeTopCard = do
    (card:deck) <- gets _loveLetterDeck
    modify (\g -> g{ _loveLetterDeck = deck, _loveLetterDiscardedCards = [card] })

emptyDeck :: LoveLetterM m => m Bool
emptyDeck = null <$> use deck

oneRemainingPlayer :: LoveLetterM m => m Bool
oneRemainingPlayer = do
    remainingPlayers <- use players
    pure (NEL.length remainingPlayers == 1)

roundIsWon :: LoveLetterM m => m Bool
roundIsWon = do
    deckIsEmpty <- emptyDeck
    onePlayerRemains <- oneRemainingPlayer
    return (deckIsEmpty || onePlayerRemains)

-- TODO: Does not match spec
pickStartPlayer :: LoveLetterM m => m Player
pickStartPlayer = rng . randomElement =<< use (players.to NEL.tail)

startingWith :: NEL.NonEmpty Player -> Player -> NEL.NonEmpty Player
ps `startingWith` p =
    NEL.fromList . cycle $ NEL.dropWhile (/= p) ps

playRound :: LoveLetterM m => m ()
playRound = do
    round += 1

    loveLetterPlayers <- use players

    let playerCount = NEL.length loveLetterPlayers

    shuffleCards
    removeTopCard

    when (playerCount == 2) $
        replicateM_ 3 removeTopCard

    -- TODO This is a little weird, I'd like a pointer to `p`
    forM_ loveLetterPlayers $ \p -> do
        setCurrentPlayer p
        drawnCard <- drawCard
        currentPlayer.card .= Just drawnCard
        get >>= liftIO . print

    startPlayer <- pickStartPlayer

    loveLetterPlayers' <- use players

    forM_ (loveLetterPlayers' `startingWith` startPlayer) $ \player ->
        unlessM roundIsWon (playTurn player)

    declareRoundWinner

playTurn :: LoveLetterM m => Player -> m ()
playTurn player = do
    drawnCard <- drawCard
    (chosenCard, otherCard) <- chooseCard drawnCard (player ^. card)

    player `discards` chosenCard

    currentPlayer.card .= otherCard

    endTurn

    void nextPlayer

renderDiscardedCard :: Player -> Card -> IO ()
renderDiscardedCard player card = do
    putStrLn $ player ^. name <> " discarded a " <> show card <> " card."
    putStrLn $ player ^. name <> " " <> discardDescription card


drawCard :: LoveLetterM m => m Card
drawCard = do
    (card':_) <- use deck
    deck %= tail
    pure card'

-- Choose a card completely randomly
chooseCard :: LoveLetterM m => Card -> Maybe Card -> m (Card, Maybe Card)
chooseCard c1 mc2 = case mc2 of
    Nothing -> pure (c1, Nothing)
    Just c2 -> do
        [x,y] <- rng (shuffle [c1, c2])
        pure (x, Just y)

endTurn = undefined

-- Forced to discard Countess
caughtWithCountess :: LoveLetterM m => Player -> m ()
caughtWithCountess = undefined

discards :: LoveLetterM m => Player -> Card -> m ()
player `discards` c = do
    liftIO $ renderDiscardedCard player c
    case c of
        Guard    -> guessAnotherHand -- "must guess another player's hand."
        Priest   -> lookAtAnotherHand -- "must look at someone else's hand."
        Baron    -> compareHands-- "must compare hands; the lower hand is eliminated."
        Handmaid -> grantProtection -- "has protection until their next turn."
        Prince   -> chooseDiscard -- "chooses a player to discard their hand."
        King     -> tradeHands -- "must trade hands with another player."
        Countess -> pure () -- "must discard the Countess if caught with King or Prince"
        Princess -> lose

guessAnotherHand = undefined
lookAtAnotherHand = undefined
compareHands = undefined
grantProtection = undefined
chooseDiscard = undefined
tradeHands = undefined
lose = undefined

declareRoundWinner = undefined

declareWinner = undefined

test :: StateT LoveLetter IO a -> IO (a, LoveLetter)
test game = do
    loveLetterPlayers <- gatherPlayers
    gen <- newStdGen
    runLoveLetter (initialState gen loveLetterPlayers) game

game :: IO ()
game = do
    loveLetterPlayers <- gatherPlayers
    gen <- newStdGen
    void $ runLoveLetter (initialState gen loveLetterPlayers) $ do
        playRound `untilM_` gameIsWon
        declareWinner
