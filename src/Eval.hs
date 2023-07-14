{-|
Module      : Eval
Description : Some simple heuristic functions for the game Fanorona. 
Copyright   : Paul Anderson
License     : AllRightsReserved
-}
module Eval where

import Fanorona
import FanoronaExtra

type PosWeights = [[Double]]

-- | Assign scores to tiles based on their connectedness.
weightsA :: PosWeights
weightsA = 
    [[3,3,5,3,5,3,5,3,3]
    ,[3,8,4,8,4,8,4,8,3]
    ,[5,4,8,4,8,4,8,4,5]
    ,[3,8,4,8,4,8,4,8,3]
    ,[3,3,5,3,5,3,5,3,3]
    ]

-- | Score a piece based on its position plus a flat value of 15.
weightsB :: PosWeights
weightsB = (fmap.fmap) (+15) weightsA

-- | Favour corners
weightsC :: PosWeights
weightsC = 
    [[4,4,3,2,1,2,3,4,4]
    ,[4,3,2,1,1,1,2,3,4]
    ,[3,3,2,1,1,1,2,3,3]
    ,[4,3,2,1,1,1,2,3,4]
    ,[4,4,3,2,1,2,3,4,4]
    ]

-- | Score a piece based on its position plus a flat value of 13.
weightsD :: PosWeights
weightsD = (fmap.fmap) (+13) weightsA

-- | Count the current player's total pieces.
pieceCount :: GameState -> Double 
pieceCount (State t _ _ b _) = case t of 
        Turn p -> fromIntegral $ sum $ map (rowSum p) b
        _ -> error "pieceCount : terminal board state"
    where
        rowSum p l = length $ filter (\x -> x == Piece p) l 

-- | Count the difference between the current player and the other player's piece totals.
pieceDiff :: GameState -> Double 
pieceDiff (State t _ _ b _) = case t of 
        GameOver _ -> error "pieceDiff : terminal board state"
        Turn p -> sum $ map (rowSum) b
            where
                rowSum l = sum $ map (sqVal) l
                sqVal Empty = 0
                sqVal (Piece p') = if (p'==p) then 1 else (-1)

-- | Check if a move involves a capture.
captureMove :: Move -> Bool 
captureMove Pass = False
captureMove (Move mtype _ _) = mtype /= Paika

-- | These represent evaluations at wins and losses respectively. 
maxVal :: Double
maxVal = 1.0

minVal :: Double
minVal = -1.0

-- | Sigmoid approximation.
fastSig :: Double -> Double 
fastSig x = x/(1+(abs x))

-- | Inverse sigmoid.
invSig :: Double -> Double 
invSig x = case (x>=0) of
    True -> x/(1-x)
    _ -> x/(1+x)

-- | Ensures that the valuation handles win and loss states sensibly,
-- as well as restricting the output to [-1,1] and making sure we 
-- evaluate from the correct perspective.
evaluator :: (GameState -> Double) -> Player -> GameState -> Double
evaluator f player st = case turn st of 
    Turn _ -> fastSig $ f (setTurn (Turn player) st) 
    GameOver (Winner p) -> (if (p==player) then maxVal else minVal)
    GameOver Draw -> 0

-- | Total piece mobility of the current player, as a Double.
totalMobil :: GameState -> Double
totalMobil gs = fromIntegral (length mvs)
    where mvs = legalMoves gs

-- | Average piece mobility of the current player.
avgMobil :: GameState -> Double
avgMobil gs = (totalMobil gs) / (pieceCount gs)

-- | Compute a weighted piece sum based on the locations of pieces.
weighPos :: PosWeights -> GameState -> Double 
weighPos weights (State t _ _ b _) = case t of 
        Turn p -> sum $ map (rowSum p) (zipWith zip b weights)
        _ -> error "pieceCount : terminal board state"
    where
        rowSum p l = sum $ map (\(x,y) -> if x == Piece p then y else 0) l 

-- | Difference of weighted positions.
weighDiff :: PosWeights -> GameState -> Double 
weighDiff w state = (weighPos w state) - (weighPos w (otherTurn state))

-- | If the game ends or a capturing is not in progress.
isQuiet :: GameState -> Bool
isQuiet gs = case turn gs of
    GameOver _ -> True
    _ -> captor gs == None 

evalA :: GameState -> Double
evalA state = 1*(pieceDiff state) + 
              (0.1)*(pieceCount state)

evalB :: GameState -> Double
evalB state = 1*(weighDiff weightsA state) + 
              (0.1)*(pieceCount state)

evalC :: GameState -> Double
evalC state = 1*(weighDiff weightsB state) 

evalD :: GameState -> Double
evalD state = 1*(weighDiff weightsB state) +
              0.25*(weighDiff weightsC state) +
              (0.05)*(avgMobil state)
