module Bidding where
import Bridge
import Control.Monad.Error

data BiddingError = DoublingError String | TooLowError Contract' Contract' | OtherError String

instance Error BiddingError where
    noMsg  = OtherError "Unknown (sorry!)"
    strMsg = OtherError

instance Show BiddingError where
    show (DoublingError s)   = "You can't (re)double that: " ++ s
    show (TooLowError c1 c2) 
        | c1 < c2   = "You bid too low: " ++ show c1 ++ " is lower than " ++ show c2
        | otherwise = "Cannot rebid same contract"
    show (OtherError s) = "General error: " ++ s

-- use Error in case we decide to monad all up in this bitch
bid :: Bid -> Contract -> Either BiddingError Contract
bid (Bid p Pass _) contract = return contract
bid (Bid p Double _) (Contract p' c Undoubled) | partnership p /= p' = return (Contract p' c Doubled)
                                               | otherwise           = throwError (DoublingError "Doubling own contract")
bid (Bid p Double _) (Contract _ c _) = throwError (DoublingError "Doubling contract that isn't undoubled")
bid (Bid p Double _) (NoContract)     = throwError (DoublingError "Doubling nonexistant contract")
bid (Bid p Redouble _) (Contract p' c Doubled) | partnership p == p' = return (Contract p' c Redoubled)
                                               | otherwise           = throwError (DoublingError "Redoubling opponents' contract")
bid (Bid p Redouble _) (Contract _ c _) = throwError (DoublingError "Redoubling contract that isn't doubled")
bid (Bid p Redouble _) (NoContract)     = throwError (DoublingError "Redoubling nonexistant contract")
bid (Bid p (Bid' c') _) (NoContract)                 = return (Contract (partnership p) c' Undoubled)
bid (Bid p (Bid' c') _) (Contract _ c _) | c' > c    = return (Contract (partnership p) c' Undoubled)
                                         | otherwise = throwError (TooLowError c' c)
