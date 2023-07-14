{-|
Module      : AlphaBeta
Description : Alpha-Beta pruning to play Fanorona (COMP1130 ruleset). 
Copyright   : Paul Anderson
License     : AllRightsReserved
-}

module AlphaBeta where

import Fanorona
import FanoronaExtra
import SimpleAI (Tree,Eval,val,children,qcut,gameTree)
import Eval

-- | Alpha is our rolling max at this node and beta is the rolling minimum of the parent.
-- If alpha exceeds beta then we simply return the rolling maximum 
-- because it and any further maximums are irrelevant.
pruneMax :: Eval -> Double -> Double -> [Tree GameState] -> Double
pruneMax eval beta alpha subtrees = case (alpha > beta, subtrees) of 
    (True,_) -> alpha 
    (_,[]) -> alpha -- There are no more rolling maximums to compute; alpha is the true maximum.
    (_,x:xs) -> max alpha' $ pruneMax (eval) beta alpha' xs -- The node is still relevant; keep computing alpha.
        where 
            alpha' = max alpha $ alphaBeta (eval) True x alpha

-- | Beta is the rolling min at this node and alpha is the rolling max of the parent.
-- Similarly, if beta becomes less than alpha, it and further minimums are irrelevant.
pruneMin :: Eval -> Double -> Double -> [Tree GameState] -> Double
pruneMin eval alpha beta subtrees = case (beta < alpha, subtrees) of 
    (True,_) -> beta
    (_,[]) -> beta
    (_,x:xs) -> min beta' $ pruneMin (eval) alpha beta' xs
        where 
            beta' = min beta $ alphaBeta (eval) False x beta
    
-- | Returns the outcome of the node under perfect play.
-- maxplayer is True iff we were the max player last turn.
-- gamma is the rolling maximum or minimum of the parent node.
alphaBeta :: Eval -> Bool -> Tree GameState -> Double -> Double
alphaBeta eval maxplayer tree g = case (maxplayer, st, isCapturing state) of
    (_,[],_) -> eval state
    (True,_,False) -> pruneMin (eval) g (1) st -- The min player will be acting from this node.
    (True,_,True) -> maximum $ map (\t -> alphaBeta (eval) True t g) $ st -- This is a max-max step, no pruning possible.
    (False,_,False) -> pruneMax (eval) g (-1) st
    (False,_,True) -> minimum $ map (\t -> alphaBeta (eval) False t g) $ st
    where 
        state = val tree
        st = children tree

-- | Generic alpha beta agent.
abQ :: Eval -> GameState -> Int -> Move
abQ eval state depth = fst $ lmaximise (snd) rootMoves
    where 
        rootMoves :: [(Move, Double)] 
        rootMoves = (fmap.fmap) (startSearch) $ (fmap.fmap) (mktree) msPairs 

        startSearch t = alphaBeta (eval) True t (-1)

        mktree = (qcut depth) . gameTree

        msPairs = map (\m -> (m, nextState state m)) (moves1130 state)

-- | Alpha beta agents with different evaluators.
alphaBetaA :: GameState -> Int -> Move
alphaBetaA state n = case (turn state) of 
    Turn p -> abQ (evaluator (evalA) p) state n
    _ -> error "alphaBetaA : game already ended"

alphaBetaC :: GameState -> Int -> Move
alphaBetaC state n = case (turn state) of 
    Turn p -> abQ (evaluator (evalC) p) state n
    _ -> error "alphaBetaC : game already ended"
