module Play where

import Bridge
import Control.Monad.Error
import Data.List

data PlayError = TurnError Player Player | SuitError Suit Trick | HandError Player Card | OtherError String
instance Error PlayError where
    noMsg  = OtherError "Unknown (sorry!)"
    strMsg = OtherError
instance Show PlayError where
    show (TurnError p1 p2) = "Player " ++ show p1 ++ " tried to play when it was " ++ show p2 ++ "'s turn."
    show (SuitError s t)   = "Tried to play a member of suit " ++ show s ++ " on trick " ++ show t ++ "."
    show (HandError p c)   = "Player " ++ show p ++ " tried to play " ++ show c ++ " when it was not in their hand."
    show (OtherError s) = "General error: " ++ s

data Trick = Trick [(Player, Card)] deriving (Show)
data History = Plays [Trick] deriving (Show)
data Game = Game { board :: Board
                 , contract ::Contract
                 , turn ::  Player
                 , truck :: Trick
                 , history :: History } deriving (Show)

trump :: Game -> Trump
trump (Game _ (Contract _ (Contract' _ t) _) _ _ _) = t -- ... whatever

suit :: Trick -> Maybe Suit
suit (Trick []) = Nothing
suit (Trick ((_, Card _ s):_)) = Just s

fullTrick :: Trick -> Bool
fullTrick (Trick [_,_,_,_]) = True
fullTrick _  = False

addCard :: Trick -> (Player, Card) -> Trick
addCard (Trick cs) c = Trick (cs ++ [c])

-- the second argument is the cards in hand for playing player
playable :: Card -> [Card] -> Trick -> Bool
playable (Card v s) h t = playable' s (suit t)
    where playable' s Nothing = True
          playable' s (Just s') = (s' == s) || void s' h
          void suit = not . any (\(Card _ s) -> s == suit)

play :: Game -> Player -> Card -> Either Game PlayError
play g p c 
    | p /= p'              = Right $ TurnError p p'
    | c `notElem` h        = Right $ HandError p c
    | not $ playable c h t = Right $ SuitError s t
    | otherwise            = Left $ Game b' contract (nextPlayer p) t'' hist'
        where (Game b contract p' t hist) = g
              (Hand h)     = getHand p b
              (Card _ s)   = c
              (Trick cs)   = t
              b'           = setHand p (Hand $ delete c h) b
              t'           = addCard t (p, c)
              (Plays ts)   = hist
              hist'        = Plays $ if fullTrick t' then ts ++ [t'] else ts
              t''          = if fullTrick t' then Trick [] else t'

over :: Game -> Bool
over (Game b _ _ _ _) = all null [north, east, south, west]
        where (Hand north) = getHand North b
              (Hand east ) = getHand East  b
              (Hand south) = getHand South b
              (Hand west ) = getHand West  b
