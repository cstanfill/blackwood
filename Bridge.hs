import Data.Maybe

-- These datatypes implement Ord in the correct order for comparisons later.  Do not rearrange!
data Partnership = NS | EW deriving (Eq, Show)
data Player = North | East | South | West deriving (Eq, Show)
data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Show, Ord)
data Trump = Trump Suit | NoTrump deriving (Eq, Show, Ord)
data Tricks = T1 | T2 | T3 | T4 | T5 | T6 | T7 deriving (Eq, Show, Ord)
data Value = V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 | V0 | VJ | VQ | VK | VA deriving (Eq, Show, Ord)
data Contract' = Contract' Tricks Trump deriving (Eq, Show, Ord)
data Bid' = Pass | Double | Redouble | Bid' Contract'
data AlertLevel = Standard | Alert | Stop
data Bid = Bid Player Bid' AlertLevel
data Card = Card Value Suit
data Doubledness = Undoubled | Doubled | Redoubled
data Contract = NoContract | Contract Partnership Contract' Doubledness

partnership :: Player -> Partnership
partnership p = case p of
  North -> NS
  South -> NS
  East -> EW
  West -> EW

bid :: Bid -> Contract -> Maybe Contract
bid (Bid p Pass _) contract = Just contract
bid (Bid p Double _) (Contract p' c Undoubled) | partnership p /= p' = Just (Contract p' c Doubled)
                                               | otherwise           = Nothing -- "Doubling own contract"
bid (Bid p Double _) (Contract _ c _) = Nothing -- "Doubling contract that isn't undoubled"
bid (Bid p Double _) (NoContract)     = Nothing -- "Doubling nonexistant contract"
bid (Bid p Redouble _) (Contract p' c Doubled) | partnership p == p' = Just (Contract p' c Redoubled)
                                               | otherwise           = Nothing -- "Redoubling opponents' contract"
bid (Bid p Redouble _) (Contract _ c _) = Nothing -- "Redoubling contract that isn't doubled"
bid (Bid p Redouble _) (NoContract)     = Nothing -- "Redoubling nonexistant contract"
bid (Bid p (Bid' c') _) (NoContract)                 = Just (Contract (partnership p) c' Undoubled)
bid (Bid p (Bid' c') _) (Contract _ c _) | c' > c    = Just (Contract (partnership p) c' Undoubled)
                                         | otherwise = Nothing -- "Too low"

