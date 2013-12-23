module Repl(repl) where

import Data.Maybe

repl :: (Show a) => (a -> b -> a) -> (String -> Maybe b) -> a -> IO ()
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
                repl f parse newstate
