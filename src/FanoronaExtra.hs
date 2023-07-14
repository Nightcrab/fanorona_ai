{-|
Module      : FanoronaExtra
Description : Add-on functions for Fanorona.hs
Copyright   : Paul Anderson
License     : AllRightsReserved
-}
module FanoronaExtra where 

import Fanorona

-- | Mathematical argmax on two arguments.
amax :: (Ord b) => (a -> b) -> a -> a -> a
amax f x y = case (f x) > (f y) of 
    True -> x 
    _ -> y 

amin :: (Ord b) => (a -> b) -> a -> a -> a
amin f x y = case (f x) < (f y) of 
    True -> x 
    _ -> y 

-- | Argmax a list.
lmaximise :: (Ord b) => (a -> b) -> [a] -> a
lmaximise f l = case l of 
    [] -> error "maximising an empty list"
    _ -> foldr (amax f) (l!!0) l

lminimise :: (Ord b) => (a -> b) -> [a] -> a
lminimise f l = case l of 
    [] -> error "minimising an empty list"
    _ -> foldr (amin f) (l!!0) l

-- | Updates a GameState under the assumption of a COMP1130 ruleset,
-- and a default behaviour of doing nothing if faced with an illegal move.
nextState :: GameState -> Move -> GameState 
nextState s m = case mstate of 
    Nothing -> s
    Just x -> x
    where mstate = applyMove COMP1130 m s 

-- | If it is legal to pass, prepend it to legal moves.
moves1130 :: GameState -> [Move]
moves1130 st = case isCapturing st of 
    False -> mvs
    _ -> Pass : mvs
    where mvs = legalMoves st

-- | Flips the turn.
otherTurn :: GameState -> GameState
otherTurn s@(State t c sz b h) = case t of 
    Turn p -> State (Turn (otherPlayer p)) c sz b h
    GameOver (Winner p) -> State (GameOver (Winner (otherPlayer p))) c sz b h
    _ -> s

isCapturing :: GameState -> Bool 
isCapturing gs = captor gs /= None

isTerminal :: GameState -> Bool
isTerminal st = case turn st of 
    GameOver _ -> True 
    _ -> False

isNotTerminal :: GameState -> Bool 
isNotTerminal st = not $ isTerminal st

locToIndex :: Location -> Int 
locToIndex (Location x y) = (9*y) + x 
