module Play where

import Bridge
import Control.Monad.Error
import Data.List

data PlayError = TurnError Player Player | SuitError Suit Trick | HandError Player Card | OtherError String
instance Error PlayError where
    noMsg    = OtherError "Unknown (sorry!)"
    strMsg s = OtherError s
instance Show PlayError where
    show (TurnError p1 p2) = "Player " ++ (show p1) ++ " tried to play when it was " ++ (show p2) ++ "'s turn."
    show (SuitError s t)   = "Tried to play a member of suit " ++ (show s) ++ " on trick " ++ (show t) ++ "."
    show (HandError p c)   = "Player " ++ (show p) ++ " tried to play " ++ (show c) ++ " when it was not in their hand."
    show (OtherError s) = "General error: " ++ s

data Trick = Trick [(Player, Card)] deriving (Show)
data History = Plays [Trick] deriving (Show)
data Game = Game Board Contract Player Trick History deriving (Show)

trump :: Game -> Trump
trump (Game _ (Contract _ (Contract' _ t) _) _ _ _) = t

turn :: Game -> Player
turn (Game _ _ p _ _) = p

trick :: Game -> Trick
trick (Game _ _ _ t _) = t

board :: Game -> Board
board (Game b _ _ _ _) = b

history :: Game -> History
history (Game _ _ _ _ h) = h

suit :: Trick -> Maybe Suit
suit (Trick []) = Nothing
suit (Trick ((_,(Card _ s)):_)) = Just s

fullTrick :: Trick -> Bool
fullTrick (Trick [_,_,_,_]) = True
fullTrick _  = False

addCard :: Trick -> (Player, Card) -> Trick
addCard (Trick cs) c = Trick (cs ++ [c])

playable :: Card -> Trick -> Bool
playable (Card v s) t = playable' s (suit t)
    where playable' s Nothing = True
          playable' s (Just s') = s' == s

play :: Game -> Player -> Card -> Either Game PlayError
play g p c 
    | p /= p'              = Right $ TurnError p p'
    | not $ elem c h       = Right $ HandError p c
    | not $ c `playable` t = Right $ SuitError s t
    | otherwise            = Left $ Game b' contract (nextPlayer p) t'' hist'
        where (Game b contract p' t hist) = g
              (Hand h)     = getHand p b
              (Card _ s)   = c
              (Trick cs)   = t
              b'           = setHand p (Hand $ delete c $ h) b
              t'           = addCard t (p, c)
              (Plays ts)   = hist
              hist'        = Plays $ case (fullTrick t') of True  -> ts ++ [t']
                                                            False -> ts
              t''          = case (fullTrick t') of True  ->  Trick []
                                                    False -> t'

ogre :: Game -> Bool 
ogre (Game b _ _ _ _) = and $ map null [north, east, south, west]  -- IT'S OGRE
        where (Hand north) = getHand North b -- IT'S HAPPENING
              (Hand east ) = getHand East  b -- YOU COULD HAVE STOPPED THIS
              (Hand south) = getHand South b -- WHY DIDN'T YOU LISTEN
              (Hand west ) = getHand West  b -- IT BEGINS
