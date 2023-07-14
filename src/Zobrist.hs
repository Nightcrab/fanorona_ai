{-|
Module      : Zobrist
Description : Implements Zobrist keys and hashing for Fanorona.
              Hashing keys assume a 9x5 board.
Copyright   : Paul Anderson
License     : AllRightsReserved
-}

module Zobrist where

import Data.Bits
import Data.Int

import qualified RBTree as M

import Fanorona
import FanoronaExtra
import Keys

type Table a = M.Map Int32 a

empty :: Table a
empty = M.empty

-- | Bitwise XOR a list.
xorlist :: [Int32] -> Int32 
xorlist l = case l of 
    [] -> 0 
    x:xs -> xor x $ xorlist xs

-- | Counter-map, or index map. Applies an index-dependent function to a list.
cmap :: (a -> Int -> b)-> [a] -> [b]
cmap f l = map (\(e,c) -> f e c) (zip l [0..])

rowtoKeys :: [Square] -> Int -> [Int32]
rowtoKeys row y = cmap (\s c -> sqtoKey s ((9*y)+c)) row 

sqtoKey :: Square -> Int -> Int32 
sqtoKey square pos = case square of 
    Empty -> 0
    Piece Player1 -> whitePieces!!pos 
    Piece Player2 -> blackPieces!!pos

btoKey :: Board -> Int32  
btoKey brd = xorlist $ map (xorlist) $ cmap (rowtoKeys) brd

capKey :: Location -> Int32 
capKey l = capturingPiece!!(locToIndex l)

-- | Calculate the z key for a whole game state. 
stateToKey :: GameState -> Int32 
stateToKey state = case (turn state, captor state, board state) of
    (GameOver _,_,_) -> error "took zobrist key of gameover state"
    (Turn Player1,None,b) -> xor wt $ btoKey b
    (Turn Player2,None,b) -> xor bt $ btoKey b
    (Turn Player1,Captor l _,b) -> xor (capKey l) $ xor wt $ btoKey b
    (Turn Player2,Captor l _,b) -> xor (capKey l) $ xor bt $ btoKey b
    where
        wt = whiteTurn 
        bt = blackTurn

insert :: GameState -> a -> Table a -> Table a
insert state e table =  M.insert (stateToKey state) e table  

get :: GameState -> Table a -> Maybe a
get state table = case turn state of
    GameOver _ -> Nothing
    _ -> M.get (stateToKey state) table

-- | Put a list of entries into a table.
insList :: [(GameState,a)] -> Table a -> Table a
insList l tt = case l of 
    [] -> tt 
    x:xs -> insert (fst x) (snd x) (insList xs tt) 
