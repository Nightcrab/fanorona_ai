{-|
Module      : AdvancedAI
Description : Improving AlphaBeta.hs with various methods such as iterative deepening and table lookup.
              (!!!) This is a prototype for the NegaMax AI. Apart from fixed-depth equivalence,
              it is not thoroughly tested. It is also not particularly strong.
Copyright   : Paul Anderson
License     : AllRightsReserved
-}

module AdvancedAI where

import           Fanorona
import           FanoronaExtra
import           SimpleAI (Tree,Eval,val,children,qcut,gameTree)
import           Eval
import           Data.List
import qualified Zobrist as Z

type Table = Z.Table Entry

data Ctx = AB Eval GameState TargetDepth

type State = (Double,Table)
type TargetDepth = Int

type Entry = (Double, Int)

empty :: Table 
empty = Z.empty

score :: Entry -> Double 
score = fst

depth :: Entry -> Int 
depth = snd

-- | Shallow AB pruning but we maintain and pass around a table of results.
pruneMax :: Ctx -> (Double,Double,Table) -> [Tree GameState] -> State
pruneMax ctx@(AB _ st d) (beta,alpha,tt) subtrees = case (alpha > beta, subtrees) of 
    (True,_) -> (alpha, Z.insert st (alpha,d) tt) -- Beta cut-off.
    (_,[]) -> (alpha, Z.insert st (alpha,d) tt)
    (_,x:xs) -> amax (fst) (alpha',tt') $ pruneMax ctx (beta,alpha',tt') xs
        where 
            alpha' = max alpha $ cval
            (cval,tt') = fullSearch ctx True x (alpha,tt) -- Child value and updated transposition table.
    
pruneMin :: Ctx -> (Double,Double,Table) -> [Tree GameState] -> State
pruneMin ctx@(AB _ st d) (alpha,beta,tt) subtrees = case (beta < alpha, subtrees) of 
    (True,_) -> (beta, Z.insert st (beta,d) tt) -- Alpha cut-off.
    (_,[]) -> (beta, Z.insert st (beta,d) tt)
    (_,x:xs) -> amin (fst) (beta',tt') $ pruneMin ctx (alpha,beta',tt') xs
        where 
            beta' = min beta $ cval
            (cval,tt') = fullSearch ctx False x (beta,tt)

-- | Create a move ordering based on the lookup table.
orderStates :: Bool -> Table -> Tree GameState -> Tree GameState-> Ordering 
orderStates ascend tt t1 t2 = case (e1,e2) of 
        (Nothing,Nothing) -> EQ
        (Just _,Nothing) -> GT
        (Nothing,Just _) -> LT
        (Just e1', Just e2') -> case ascend of
            True -> compare (score e1') (score e2')
            _ -> compare (score e2') (score e1')
    where
        s1 = val t1
        s2 = val t2
        e1 = Z.get s1 tt
        e2 = Z.get s2 tt     

fullSearch :: Ctx -> Bool -> Tree GameState -> State -> State
fullSearch ctx maxparent tr s@(g,tt) = case (maxparent, subtrees, cap) of
    (_,[],_) -> (value, tt)
    (True,_,False) -> pruneMin ctx' (g,1,tt) subtreesA -- it's the beta player's turn
    (True,_,True) -> (lmax, Z.insert state (lmax,d) tt')
    (False,_,False) -> pruneMax ctx' (g,-1,tt) subtreesD
    (False,_,True) -> (lmin, Z.insert state (lmin,d) tt'')
    where 
        state = val tr
        subtrees = children tr
        subtreesA = sortBy (orderStates True tt) subtrees
        subtreesD = sortBy (orderStates False tt) subtrees
        cap = isCapturing state
        value = eval state

        (lmax,tt') = lmaximise (fst) $ map (\t -> fullSearch ctx' True t s) $ subtrees
        (lmin,tt'') = lminimise (fst) $ map (\t -> fullSearch ctx' False t s) $ subtrees

        AB eval _ d = ctx
        ctx' = AB (eval) state d

-- | alpha beta with a transposition table.
abT :: (GameState -> Double) -> GameState -> Int -> Move
abT eval state d = fst $ lmaximise (fst.snd) rootMoves
    where 
        rootMoves :: [(Move, State)]
        rootMoves = (fmap.fmap) (\t -> fullSearch ctx True t (-1,empty)) $ 
                    (fmap.fmap) (mktree) msPairs 

        mktree = (qcut d) . gameTree

        ctx = AB (eval) state d

        msPairs = map (\m -> (m, nextState state m)) (moves1130 state)

-- | abT agents.
abtC :: GameState -> Int -> Move
abtC state n = case (turn state) of 
    Turn p -> abT (evaluator (evalC) p) state n
    _ -> error "abtC : game already ended"

-- | Generate lookup table.
makeTable :: (GameState -> Double) -> GameState -> Int -> Table 
makeTable _ _ 0 = empty 
makeTable eval state dep = case (dep < 0) of
    True -> error "makeTable: negative depth"
    _ -> table'
    where 
        table = makeTable (eval) state (dep-1)
        (_,table') = pruneMax ctx (1,-1,table) (children tree)
        tree = qcut dep $ gameTree state
        ctx = AB (eval) state dep

-- | Iterative deepening.
iteratedAB :: (GameState -> Double) -> GameState -> Int -> Move 
iteratedAB eval state d = fst $ lmaximise (snd) moveScores -- error $ show $ moveScores -- error $ show $ table -- 
    where
        moveScores :: [(Move, Double)] 
        moveScores = map (\m -> (m, stateToScore (nextState state m))) (moves1130 state)

        stateToScore st = case (isTerminal st, Z.get st table) of 
            (True,_) -> eval st 
            (_,Nothing) -> error "didn't explore this child" -- -1
            (_,Just e) -> score e 

        table = makeTable (eval) state (d+1)

iabC :: GameState -> Int -> Move
iabC state n = case (turn state) of 
    Turn p -> iteratedAB (evaluator (evalC) p) state n
    _ -> error "iabC : game already ended"

iabD :: GameState -> Int -> Move
iabD state n = case (turn state) of 
    Turn p -> iteratedAB (evaluator (evalD) p) state n
    _ -> error "iabD : game already ended"


-- | Profiling 

traceA :: a -> a 
traceA x = {-# SCC "alpha_cutoff" #-} x 

traceB :: a -> a 
traceB x = {-# SCC "beta_cutoff" #-} x 

traceC :: a -> a 
traceC x = {-# SCC "table_hit" #-} x

traceD :: a -> a 
traceD x = {-# SCC "no_cutoff" #-} x