{-|
Module      : Debugging
Description : Tools to simplify debugging AI and game tree evaluations.
Copyright   : (c) 2020 Paul Anderson
License     : AllRightsReserved
-}
module Debugging where

import SimpleAI
import Fanorona

type SearchFunc = GameState -> Int -> Move

-- | map function for a rose tree.
treemap :: (a -> b) -> Tree a -> Tree b 
treemap f (Node a l) = Node (f a) $ map (treemap f) l

origin :: Location 
origin = Location 0 0

emptyBoardList :: [Square]
emptyBoardList = 
    [ Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty
    , Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty
    , Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty
    , Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty
    , Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty
    ]

-- | Simulate a game between lookahead AIs using fixed search depths.
playout :: Int -> Int -> (SearchFunc,SearchFunc) -> GameState -> GameState
playout size depth p@(p1, p2) state = case (size <= 0, turn state) of 
    (True,_) -> state 
    (_, Turn Player1) -> playout (size-1) depth p s1
    (_, Turn Player2) -> playout (size-1) depth p s2
    _ -> state
    where
        Just s1 = (applyMove COMP1130 (p1 state depth) state)
        Just s2 = (applyMove COMP1130 (p2 state depth) state)

-- | Convert a list into a 2D list with width 9.
wrapList :: [a] -> [[a]]
wrapList lst = wrapListHelper lst []

wrapListHelper :: [a] -> [[a]] -> [[a]]
wrapListHelper list partial = case list of 
    [] -> partial 
    _:_ -> wrapListHelper (drop 9 list) $ partial ++ [(take 9 list)]

-- | Naive encoding of integers into board states.
intToBoard :: Int -> Board 
intToBoard a = wrapList $ (take a (repeat (Piece Player1))) ++ (drop a emptyBoardList)

intToState :: (Int,Int) -> GameState 
intToState (a,b) = case b of 
    1 -> State (Turn Player1) None (9,5) brd [] 
    2 -> State (Turn Player2) None (9,5) brd [] 
    3 -> State (Turn Player1) (Captor origin []) (9,5) brd [] 
    4 -> State (Turn Player2) (Captor origin []) (9,5) brd []
    5 -> State (GameOver (Winner Player1)) None (9,5) brd []
    _ -> State (GameOver (Winner Player2)) None (9,5) brd []
    where brd = intToBoard a 

intGameTree :: Tree (Int,Int) -> Tree GameState 
intGameTree tree = treemap (intToState) tree 
