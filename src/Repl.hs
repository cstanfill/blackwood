module Main where

import Control.Applicative
import Data.Maybe
import Data.Either
import Bridge
import Play
import Deal
import System.Random

repl :: (Show a, Show c) => (a -> b -> (Either a c)) -> (String -> Maybe b) -> a -> IO ()
repl f parse init = do 
    z <- getLine
    input <- return $ parse z
    if isNothing input
        then
            do 
                print "Parse error. Try again."
                repl f parse init
        else 
            do 
                (Just input') <- return input
                newstate <- return $ f init input'
                print newstate
                either (repl f parse) (const $ repl f parse init) newstate
parsePlay :: String -> Maybe (Player, Card)
parsePlay (p:c) = (liftA2 . curry $ id) (parsePlayer [p]) $ parseCard c
parsePlay _     = Nothing

parsePlayer :: String -> Maybe Player
parsePlayer "N" = Just North
parsePlayer "E" = Just East
parsePlayer "S" = Just South
parsePlayer "W" = Just West

parseCard :: String -> Maybe Card
parseCard [a,b,c] = liftA2 Card (parseValue [a,b]) (parseSuit [c])
parseCard [a,b] = liftA2 Card (parseValue [a]) (parseSuit [b])
parseCard _ = Nothing

parseSuit :: String -> Maybe Suit
parseSuit "C" = Just Clubs
parseSuit "D" = Just Diamonds
parseSuit "H" = Just Hearts
parseSuit "S" = Just Spades
parseSuit _   = Nothing


parseValue :: String -> Maybe Value
parseValue "2"  = Just V2
parseValue "3"  = Just V3
parseValue "4"  = Just V4
parseValue "5"  = Just V5
parseValue "6"  = Just V6
parseValue "7"  = Just V7
parseValue "8"  = Just V8
parseValue "9"  = Just V9
parseValue "10" = Just V0
parseValue "J"  = Just VJ
parseValue "Q"  = Just VQ
parseValue "K"  = Just VK
parseValue "A"  = Just VA
parseValue _    = Nothing

newGame :: Int -> Game
newGame i = Game b c North (Trick []) (Plays [])
    where c = Contract NS (Contract' T2 NoTrump) Undoubled
          b = deal . shuffle deck . mkStdGen $ i

play' :: Game -> (Player, Card) -> Either Game PlayError
play' g = uncurry $ play g

main :: IO ()
main = do
    game <- return $ newGame 0
    print game
    repl play' parsePlay game