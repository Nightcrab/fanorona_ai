{-|
Module      : SimpleAI
Description : Fanorona (COMP1130 ruleset) AIs,
              using the default move generator / board representation. 
Copyright   : Paul Anderson
License     : AllRightsReserved
-}
module SimpleAI where

import Fanorona
import FanoronaExtra
import Eval

-- | Evaluation function format.
type Eval = (GameState -> Double)

-- | Rose tree.
data Tree a = Node a [Tree a]
    deriving (Show,Eq)

val :: Tree a -> a
val (Node a _) = a

children :: Tree a -> [Tree a]
children (Node _ l) = l

-- | We prune the tree to depth n but extend at possible captures.
qcut :: Int -> Tree GameState -> Tree GameState
qcut 0 (Node s c) = Node s (map (qcut 0) (filter (not.isQuiet.val) c))
qcut n (Node s c) = Node s $ map (qcut (n-1)) c

-- | Older version of qcut, only ensuring we don't stop in
-- the middle of a capturing sequence.
qcut' :: Int -> Tree GameState -> Tree GameState
qcut' 0 (Node s c) = case isQuiet s of 
    True -> Node s [] 
    False -> Node s $ map (qcut 0) c
qcut' n (Node s c) = Node s $ map (qcut (n-1)) c

-- | Ensures that evaluation stops at a depth of n in the tree.
-- We use map to extract and then cutoff all the subtrees at each node.
cutoff :: Int -> Tree a -> Tree a
cutoff 0 (Node a _) = Node a []
cutoff n (Node a l) = Node a $ map (cutoff (n-1)) l

-- | Game tree with particular root state.
gameTree :: GameState -> Tree GameState
gameTree state = Node state $ map (subtree) $ moves1130 state
    where subtree = gameTree . (nextState state)

-- | A greedy AI which only observes the material difference after 1 move.
pieceGreedy :: GameState -> Move
pieceGreedy (State (GameOver _) _ _ _ _) = error "pieceGreedy : game already ended"
pieceGreedy state = fst $ lmaximise (snd) rootMoves
    where 
        rootMoves :: [(Move, Double)] 
        rootMoves = map (\(x,y) -> (x,evaluator (pieceDiff) player y)) msPairs
        Turn player = turn state
        msPairs = map (\m -> (m, nextState state m)) (moves1130 state)

-- | The two modes of minimax. 
maxPlayer ::  Tree GameState -> Eval -> Double
maxPlayer (Node st l) eval = case l of 
    [] -> eval st
    _ -> maximum (map (next) l)
    where 
        next tr = minmaxPlayer False tr eval

minPlayer :: Tree GameState -> Eval -> Double
minPlayer (Node st l) eval = case l of 
    [] -> eval st
    _ -> minimum (map (next) l)
    where 
        next tr = minmaxPlayer True tr eval

-- | Logic for switching between min and max.
minmaxPlayer :: Bool -> Tree GameState -> Eval -> Double
minmaxPlayer isMin tree = case (isCapturing (val tree), isMin) of 
    (True, True) -> minPlayer tree
    (True, False) -> maxPlayer tree
    (False, True) -> maxPlayer tree
    _ -> minPlayer tree

-- The minimax agent. Actually searches to a depth of n+1.
gnrcMinimax :: Eval -> GameState -> Int -> Move 
gnrcMinimax eval state n = fst $ lmaximise (snd) rootMoves -- error $ show $ rootMoves -- 
    where 
        rootMoves :: [(Move, Double)] 
        rootMoves = (fmap.fmap) (startSearch) $ (fmap.fmap) (mktree) msPairs 

        startSearch t = minmaxPlayer False t eval

        mktree = (qcut n) . gameTree

        msPairs = map (\m -> (m, nextState state m)) (moves1130 state)

-- | Minimax agents with different evaluation functions.
minimaxQ :: GameState -> Int -> Move 
minimaxQ state n = case (turn state) of 
    Turn p -> gnrcMinimax (evaluator (evalA) p) state n
    _ -> error "mmQuiescent : game already ended"

minimaxQ' :: GameState -> Int -> Move 
minimaxQ' state n = case (turn state) of 
    Turn p -> gnrcMinimax (evaluator (evalB) p) state n
    _ -> error "mmQuiescent (evalB) : game already ended"

minimaxQ'' :: GameState -> Int -> Move 
minimaxQ'' state n = case (turn state) of 
    Turn p -> gnrcMinimax (evaluator (evalC) p) state n
    _ -> error "mmQuiescent (evalC) : game already ended"

-- Minimax agent which doesn't use quiescence searching; it always stops at the fixed depth.
noQMinimax :: (GameState -> Double) -> GameState -> Int -> Move 
noQMinimax eval state n = fst $ lmaximise (snd) rootMoves
    where 
        rootMoves :: [(Move, Double)] 
        rootMoves = (fmap.fmap) (\t -> minmaxPlayer' False t eval) $ (fmap.fmap) (mktree) msPairs 

        mktree = (cutoff n) . gameTree

        msPairs = map (\m -> (m, nextState state m)) (moves1130 state)

maxPlayer' ::  Tree GameState -> (GameState -> Double) -> Double
maxPlayer' (Node st l) eval = case l of 
    [] -> eval st
    _ -> maximum (map (next) l)
    where 
        next tr = minmaxPlayer' False tr eval

minPlayer' :: Tree GameState -> (GameState -> Double) -> Double
minPlayer' (Node st l) eval = case l of 
    [] -> eval st
    _ -> minimum (map (next) l)
    where 
        next tr = minmaxPlayer' True tr eval

minmaxPlayer' :: Bool -> Tree GameState -> (GameState -> Double) -> Double
minmaxPlayer' isMin tree = case (isCapturing (val tree), isMin) of 
    (True, True) -> minPlayer' tree
    (True, False) -> maxPlayer' tree
    (False, True) -> maxPlayer' tree
    _ -> minPlayer' tree

minimaxNoQ :: GameState -> Int -> Move 
minimaxNoQ state n = case (turn state) of 
    Turn p -> noQMinimax (evaluator (evalC) p) state n
    _ -> error "mmNoQuiescent : game already ended"
