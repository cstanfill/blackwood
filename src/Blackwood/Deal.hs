module Blackwood.Deal where
import Blackwood.Bridge
import System.Random

-- Oh dang using a seed as input means we can totes replay hands :D
shuffle :: (RandomGen g) => [Card] -> g -> [Card]
shuffle [] _ = []
shuffle cards g = card : shuffle rest g'
    where (index, g') = random g
          card        = cards !! (index `mod` length cards)
          rest        = filter (/= card) cards

deal :: [Card] -> Board
deal = hands' . deal'
    where hands' (n,e,s,w) = Hands n e s w

deal':: [Card] -> (Hand, Hand, Hand, Hand)
deal' (a:b:c:d:fs) = (Hand (a:as), Hand (b:bs), Hand (c:cs), Hand (d:ds))
    where (Hand as, Hand bs, Hand cs, Hand ds) = deal' fs
deal' [] = (Hand [], Hand [], Hand [], Hand [])
