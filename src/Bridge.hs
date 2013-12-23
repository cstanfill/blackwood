module Bridge where
import Data.Either
import Data.List
import System.Random

-- These datatypes implement Ord in the correct order for comparisons later.  Do not rearrange!
data Partnership = NS | EW deriving (Eq, Show)
data Player = North | East | South | West deriving (Eq, Show, Read)
data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Show, Ord, Enum)
data Trump = Trump Suit | NoTrump deriving (Eq, Show, Ord)
data Tricks = T1 | T2 | T3 | T4 | T5 | T6 | T7 deriving (Eq, Show, Ord)
data Value = V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 | V0 | VJ | VQ | VK | VA deriving (Eq, Ord, Enum)
data Contract' = Contract' Tricks Trump deriving (Eq, Show, Ord)
data Bid' = Pass | Double | Redouble | Bid' Contract'
data AlertLevel = Standard | Alert | Stop
data Bid = Bid Player Bid' AlertLevel
data Card = Card Value Suit deriving Eq
data Doubledness = Undoubled | Doubled | Redoubled deriving Show
data Contract = NoContract | Contract Partnership Contract' Doubledness deriving Show
data Hand = Hand [Card]
data Board = Hands Hand Hand Hand Hand

instance Show Value where
    show v = case v of
      V2 -> "2"
      V3 -> "3"
      V4 -> "4"
      V5 -> "5"
      V6 -> "6"
      V7 -> "7"
      V8 -> "8"
      V9 -> "9"
      V0 -> "10"
      VJ -> "J"
      VQ -> "Q"
      VK -> "K"
      VA -> "A"

-- :B
instance Enum Card where
    toEnum i = Card (toEnum $ i `quot` 4) (toEnum $ i `rem` 4)
    fromEnum (Card v s) = fromEnum v * 4 + fromEnum s
    enumFrom p = map toEnum [(fromEnum p)..51]

instance Show Card where
    show (Card v s) = show v ++ " of " ++ show s

instance Show Hand where
    show (Hand cs) = intercalate "\n" $ map displaySuit $ reverse (enumFrom Clubs)
        where displaySuit s = nameSuit s ++ ": " ++ unwords (nameCards s)
              nameSuit Clubs = "C"
              nameSuit Hearts = "H"
              nameSuit Diamonds = "D"
              nameSuit Spades = "S"
              nameCards s = nameCards' $ map (\(Card v _) -> v) $ filter (\(Card _ s') -> s' == s) cs
              nameCards' [] = ["-"]
              nameCards' xs = map show $ sortBy (flip compare) xs

instance Show Board where
    show (Hands n e s w) = "North:\n" ++ show n ++ "\n\n" ++
                           "East:\n"  ++ show e ++ "\n\n" ++
                           "South:\n" ++ show s ++ "\n\n" ++
                           "West:\n"  ++ show w ++ "\n"

partnership :: Player -> Partnership
partnership p = case p of
  North -> NS
  South -> NS
  East -> EW
  West -> EW

partner :: Player -> Player
partner p = case p of
  North -> South
  South -> North
  East  -> West
  West  -> East

deck :: [Card]
deck = enumFrom (Card V2 Clubs)

getHand :: Player -> Board -> Hand
getHand North (Hands n _ _ _) = n
getHand East  (Hands _ e _ _) = e
getHand South (Hands _ _ s _) = s
getHand West  (Hands _ _ _ w) = w

setHand :: Player -> Hand -> Board -> Board
setHand North h (Hands n e s w) = Hands h e s w
setHand East  h (Hands n e s w) = Hands n h s w
setHand South h (Hands n e s w) = Hands n e h w
setHand West  h (Hands n e s w) = Hands n e s h

nextPlayer :: Player -> Player
nextPlayer North = East
nextPlayer East = South
nextPlayer South = West
nextPlayer West = North
