{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module LoveLetter where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Extra
import           Control.Monad.Loops
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Maybe
import           Data.List                  (maximumBy)
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.List.NonEmpty         as NEL
import           Data.Monoid
import           Data.Ord
import           Data.Random
import           Debug.Trace
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
  Countess -> "sits there like a doofus."
  Princess -> "loses."

showHand :: Maybe Card -> String
showHand = \case
    Just c -> show c
    Nothing -> "Nothing"

-- | Get the next Player and cycle the turn.
nextPlayer :: LoveLetterM m => m (Maybe Player)
nextPlayer = do
    p :| xs <- use (round.players)
    case xs of
        [] -> pure Nothing
        (player':xs') ->
            let newPlayers = player' :| (xs' ++ [p])
            in do
                round.players .= newPlayers
                pure $ Just player'

runLoveLetter :: s -> StateT s m a -> m (a, s)
runLoveLetter = flip runStateT

unshuffledDeck :: [Card]
unshuffledDeck =
    [ Princess
    , Countess
    , King
    , Prince, Prince
    , Handmaid, Handmaid
    , Baron, Baron
    , Priest, Priest
    , Guard, Guard, Guard, Guard, Guard]

initialState :: StdGen -> NEL.NonEmpty Player -> LoveLetter
initialState gen ps =
    LoveLetter
        ps
        (Round 0 [] ps unshuffledDeck [])
        gen

-- Static for now
gatherPlayers :: MonadIO m => m (NonEmpty Player)
gatherPlayers = pure . NEL.fromList $ map mkPlayer ["Ben", "Amanda", "Bozo The Clown"]

whenAlt :: (Alternative f) => f a -> f a -> f a
whenAlt f go = (f *> go) <|> empty

untilAlt :: (Alternative f) => f a -> f a -> f a
untilAlt f go = f <|> untilAlt f go

untilAlt_ :: (Alternative f) => f a -> f a -> f ()
untilAlt_ f = void . untilAlt f

handValue :: Player -> Int
handValue p = case p ^. card of
    Nothing -> 0
    Just c -> fromEnum c

emptyDeckWinner :: LoveLetterM m => m (Maybe Player)
emptyDeckWinner = do
    roundDeck <- use $ round.deck

    case length roundDeck of
        0 -> do
            roundPlayers <- use $ round.players

            let winner = maximumBy (comparing (^. card)) roundPlayers
            let ties = NEL.filter (\p -> p ^. card == winner ^. card) roundPlayers

            pure . Just $ case length ties of
                1 -> winner
                _ -> flip maximumBy roundPlayers $
                    comparing (\player ->
                        sum (map fromEnum (player ^. discardedCards))
                    )
        _ -> pure Nothing

allEliminatedWinner :: LoveLetterM m => m (Maybe Player)
allEliminatedWinner = do
    players' <- use $ round.players

    pure $ case players' of
        p :| [] -> Just p
        _       -> Nothing

roundWinner :: LoveLetterM m => m (Maybe Player)
roundWinner = do
    emptyDeckWinner' <- emptyDeckWinner
    allEliminatedWinner' <- allEliminatedWinner
    pure $ emptyDeckWinner' <|> allEliminatedWinner'

rng :: LoveLetterM m => RVar a -> m a
rng var = do
    rng' <- use rNG
    let (r, stdGen') = sampleState var rng'
    rNG .= stdGen'
    pure r

shuffleCards :: LoveLetterM m => m ()
shuffleCards = do
    deck' <- rng (shuffle unshuffledDeck)
    round.deck .= deck'
    liftIO $ putStr "[SECRET] Deck: "
    liftIO $ print deck'

removeTopCard :: LoveLetterM m => m ()
removeTopCard = do
    (card:deck') <- use $ round.deck
    round.discardedCards %= (card:)
    round.deck .= deck'
    liftIO . putStrLn $ "A " <> show card <> " card is face-up on the table."

emptyDeck :: LoveLetterM m => m Bool
emptyDeck = null <$> use (round.deck)

oneRemainingPlayer :: LoveLetterM m => m Bool
oneRemainingPlayer = do
    remainingPlayers <- use $ round.players
    pure (NEL.length remainingPlayers == 1)

roundIsWon :: LoveLetterM m => m Bool
roundIsWon = do
    deckIsEmpty <- emptyDeck
    onePlayerRemains <- oneRemainingPlayer
    return (deckIsEmpty || onePlayerRemains)

-- TODO: Does not match spec
pickStartPlayer :: LoveLetterM m => m Player
pickStartPlayer = rng . randomElement =<< use (globalPlayers.to NEL.toList)

startingWith :: NEL.NonEmpty Player -> Player -> NEL.NonEmpty Player
ps `startingWith` p =
    NEL.fromList . take len $ NEL.dropWhile (/= p) (NEL.cycle ps)
    where len = length ps

playRound :: LoveLetterM m => m ()
playRound = do
    round.number += 1

    currentRound <- use round

    loveLetterPlayers <- use globalPlayers
    startPlayer <- pickStartPlayer

    round.players .= loveLetterPlayers `startingWith` startPlayer

    roundPlayers <- use $ round.players

    fire (PleaseLogMe (show roundPlayers))

    let playerCount = NEL.length roundPlayers

    shuffleCards
    removeTopCard

    when (playerCount == 2) $
        replicateM_ 3 removeTopCard

    -- TODO This is a little weird, I'd like a pointer to `p`
    use (round.players) >>= \players' -> forM_ players' $ \p -> do
        drawnCard <- drawCard
        modifyPlayer p (card .~ Just drawnCard)

    roundPlayers <- use $ round.players

    fire (PleaseLogMe (show roundPlayers))

    get >>= fire . PleaseLogMe . show
    -- use round >>= fire . RoundBegins

    (playTurn >> nextPlayer) `untilM` roundIsWon

    mWinner <- roundWinner

    case mWinner of
        Nothing -> error "SOMETHING WENT WRONG! THERE SHOULD BE A WINNER."
        Just winner -> declareRoundWinner winner

playTurn :: LoveLetterM m => m ()
playTurn = do
    currentPlayer.protected .= False
    player <- use currentPlayer

    pName <- use $ currentPlayer.name

    liftIO . putStrLn $ "~~~~~~~~       " <> pName <> "'s turn       ~~~~~~~"

    drawnCard <- drawCard
    (chosenCard, otherCard) <- chooseCardRandomly drawnCard (player ^. card)

    currentPlayer.card .= otherCard

    liftIO . putStrLn
        $ "[SECRET] "
        <> player ^. name
        <> " takes card "
        <> showHand otherCard

    player `discards` chosenCard

    -- Some space
    liftIO $ putStrLn ""

renderDiscardedCard :: Player -> Card -> IO ()
renderDiscardedCard player card = do
    putStrLn $ player ^. name <> " discarded a " <> show card <> " card."
    putStrLn $ player ^. name <> " " <> discardDescription card

-- TODO: Replenish after the round ends!
drawCard :: LoveLetterM m => m Card
drawCard = do
    (card':_) <- use $ round.deck
    round.deck %= tail

    currentPlayer' <- use currentPlayer
    liftIO . putStrLn $ "[SECRET] "
        <> currentPlayer' ^. name
        <> " drew card "
        <> show card'
        <> "."

    pure card'

-- Choose a card completely randomly
chooseCardRandomly :: LoveLetterM m => Card -> Maybe Card -> m (Card, Maybe Card)
chooseCardRandomly c1 mc2 = do
    currentPlayer' <- use currentPlayer
    liftIO . putStrLn $ "[SECRET] "
        <> currentPlayer' ^. name
        <> " is choosing between "
        <> show c1
        <> " (drawn) and "
        <> showHand mc2
        <> " (held)."
    case mc2 of
        Nothing -> pure (c1, Nothing)
        Just c2 -> do
            [x,y] <- rng (shuffle [c1, c2])
            pure (x, Just y)

-- Forced to discard Countess
caughtWithCountess :: LoveLetterM m => Player -> m ()
caughtWithCountess = undefined

modifyPlayer :: LoveLetterM m => Player -> (Player -> Player) -> m ()
modifyPlayer p f = round.players %= mapPlayer (p ^. name) f

modifyGlobalPlayer :: LoveLetterM m => Player -> (Player -> Player) -> m ()
modifyGlobalPlayer p f = globalPlayers %= mapPlayer (p ^. name) f

discards :: LoveLetterM m => Player -> Card -> m ()
player `discards` c = do
    liftIO $ renderDiscardedCard player c
    case c of
        Guard    -> guessAnotherHand -- "must guess another player's hand."
        Priest   -> lookAtAnotherHand -- "must look at someone else's hand."
        Baron    -> compareHands -- "must compare hands; the lower hand is eliminated."
        Handmaid -> grantProtection -- "has protection until their next turn."
        Prince   -> chooseDiscard -- "chooses a player to discard their hand."
        King     -> tradeHands -- "must trade hands with another player."
        Countess -> pure () -- "must discard the Countess if caught with King or Prince"
        Princess -> lose

    modifyPlayer player (discardedCards %~ (c:))

othersWithoutProtection :: LoveLetterM m => m [Player]
othersWithoutProtection = do
    _ :| others <- use $ round.players
    pure $ others ^.. folded . filtered (not . _playerProtected)

chooseAnotherPlayer :: LoveLetterM m => m Player
chooseAnotherPlayer = do
    others <- othersWithoutProtection
    guess <- case others of
        [] -> use currentPlayer -- "Yourself if possible"
        _  -> rng (randomElement others)

    pName <- use $ currentPlayer.name

    liftIO . putStrLn $ pName <> " chooses " <> guess ^. name <> "!"

    pure guess

-- TODO: I kinda want player parameterization
chooseCardGuess :: LoveLetterM m => m Card
chooseCardGuess = do
    let cardTypes = [minBound..maxBound] :: [Card]
    chosen' <- rng (randomElement cardTypes)

    name' <- use $ currentPlayer.name

    liftIO . putStrLn $ name' <> " guesses " <> show chosen' <> "!"
    pure chosen'

guessHand :: Card -> Player -> Bool
guessHand guessedCard p = p ^. card == Just guessedCard

knockOut :: LoveLetterM m => Player -> m ()
knockOut p = do
    playersLeft <- use $ round.players
    fire (PlayerKnockedOut p playersLeft)

guessAnotherHand :: LoveLetterM m => m ()
guessAnotherHand = do
    p <- chooseAnotherPlayer
    guess <- chooseCardGuess
    currentPlayer' <- use currentPlayer

    fire (CardGuess guess currentPlayer' p)

viewCard :: LoveLetterM m => Player -> m ()
viewCard p =
    liftIO . putStrLn $ p ^. name <> " is holding card: " <> show (p ^. card)

lookAtAnotherHand :: LoveLetterM m => m ()
lookAtAnotherHand = do
    p <- chooseAnotherPlayer
    viewCard p

compareHands :: LoveLetterM m => m ()
compareHands = do
    currentPlayer' <- use currentPlayer
    otherPlayer <- chooseAnotherPlayer
    case compare (currentPlayer' ^. card) (otherPlayer ^. card) of
        EQ -> pure ()
        LT -> knockOut currentPlayer'
        GT -> knockOut otherPlayer

grantProtection :: LoveLetterM m => m ()
grantProtection = currentPlayer.protected .= True

discardHandWithoutEffect :: LoveLetterM m => Player -> m ()
discardHandWithoutEffect otherPlayer = do
    liftIO . putStrLn
        $ (otherPlayer ^. name)
        <> " discards a "
        <> showHand (otherPlayer ^. card)
        <> " without effect."

    case otherPlayer ^. card of
        Nothing -> pure ()
        Just card' -> do
            modifyPlayer otherPlayer (discardedCards %~ (card':))
            modifyPlayer otherPlayer (card .~ Nothing)
    
    knockOut otherPlayer


drawNewCard :: LoveLetterM m => Player -> m ()
drawNewCard p = do
    cardDeck <- use $ round.deck
    newCard <- case cardDeck of
        [] -> do
            (x:_) <- use $ round.discardedCards
            pure x
        (x:_) -> pure x
    roundPlayers <- use $ round.players
    round.players .= mapPlayer (p ^. name) (& card .~ Just newCard) roundPlayers

chooseDiscard :: LoveLetterM m => m ()
chooseDiscard = do
    p <- chooseAnotherPlayer
    discardHandWithoutEffect p
    drawNewCard p

swapHands :: LoveLetterM m => Player -> Player -> m ()
swapHands p q = do
    let pHand = p ^. card
        qHand = q ^. card
    round.players %= mapPlayer (p ^. name) (& card .~ qHand)
    round.players %= mapPlayer (q ^. name) (& card .~ pHand)

tradeHands :: LoveLetterM m => m ()
tradeHands = do
    p <- chooseAnotherPlayer
    currentPlayer' <- use currentPlayer
    swapHands p currentPlayer'

lose :: LoveLetterM m => m ()
lose = do
    currentPlayer' <- use currentPlayer
    knockOut currentPlayer'

declareRoundWinner :: LoveLetterM m => Player -> m ()
declareRoundWinner = fire . RoundWonBy

showFinalScore :: NEL.NonEmpty Player -> IO ()
showFinalScore ps = forM_ (NEL.sortBy (comparing (negate . _playerTokens)) ps) $ \p ->
    putStrLn
        $ p ^. name
        <> " has "
        <> show (p ^. tokens)
        <> " tokens of affection."

declareWinner :: LoveLetterM m => m ()
declareWinner = do
    players' <- use globalPlayers
    let winThreshold = case length players' of
                        2 -> 7
                        3 -> 5
                        4 -> 4
    let winner = head $ NEL.filter (\p -> p ^. tokens >= winThreshold) players'
    fire (GameWinnerAnnounced winner)
    liftIO $ showFinalScore players'

loveLetter :: LoveLetterM m => m ()
loveLetter = do
    players' <- use globalPlayers
    let winThreshold = case length players' of
                        2 -> 7
                        3 -> 5
                        4 -> 4
    let gameIsWon = any (>= winThreshold) $ NEL.map (^. tokens) players'
    if gameIsWon
        then declareWinner
        else do
            playRound
            loveLetter

data LoveLetterEvent
    = RoundBegins Round
    | RoundWonBy Player
    | GameWinnerAnnounced Player
    | PlayerKnockedOut Player (NEL.NonEmpty Player)
    | CardGuess
        Card
        Player -- ^ Guesser
        Player -- ^ Guessee
    | PleaseLogMe String

fire :: LoveLetterM m => LoveLetterEvent -> m ()
fire e = mapM_ ($e) handlers
    where
        handlers :: LoveLetterM m => [LoveLetterEvent -> m ()] 
        handlers =
            [ liftIO . loggingHandler
            , playerHandler
            ]

playerHandler :: LoveLetterM m => LoveLetterEvent -> m ()
playerHandler = \case
    RoundWonBy winner ->
        modifyGlobalPlayer winner (tokens +~ 1)
    PlayerKnockedOut player _ -> do
        round.players
            %= NEL.fromList
            . NEL.filter (\p -> p ^. name /= player ^. name)
        round.losers %= (player:)
    CardGuess card' guesser guessee ->
        when (guessee ^. card == Just card')
            $ knockOut guessee
    _ -> pure ()

loggingHandler :: LoveLetterEvent -> IO ()
loggingHandler = \case
    RoundBegins round -> do
        putStrLn 
            $ "######## ROUND " <> show (round ^. number) <> " ########"
        putStrLn
            $ "[DEBUG] Current Players: "
            <> show (round ^. players)
        
    RoundWonBy winner -> putStrLn $ "The winner of this round is " 
        <> winner ^. name
        <> " with a "
        <> showHand (winner ^. card)
        <> " card"
        <> "!"

    GameWinnerAnnounced winner -> do
        putStrLn "============== WINNER ANNOUNCEMENT: IMPORTANT LISTEN UP ==============="
        liftIO . putStrLn $ "The winner of the game is "
            <> winner ^. name
            <> "! Congratulations!\n"

    PlayerKnockedOut player playersLeft -> do
        liftIO . putStrLn $ player ^. name <> " has been knocked out of the round!"
        liftIO . putStrLn $ "Remaining players:"

        mapM_ (liftIO . putStrLn . view name) playersLeft

    CardGuess card guesser guessee -> do
        putStrLn $ 
            guesser ^. name
            <> " guesses that "
            <> guessee ^. name
            <> " has a "
            <> show card
            <> " card."
        if guessHand card guessee
            then putStrLn
                    $ guesser ^. name
                    <> " was correct!"
            else
                putStrLn
                    $ guesser ^. name
                    <> " was wrong!"
    PleaseLogMe x -> putStrLn $ "[PLEASE_LOG_ME] " <> x
    _ -> pure ()

game :: IO ()
game = do
    loveLetterPlayers <- gatherPlayers
    gen <- newStdGen
    void $ runLoveLetter (initialState gen loveLetterPlayers) loveLetter
