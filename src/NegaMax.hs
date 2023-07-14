{-|
Module      : NegaMax
Description : Generally improved and simplified version of AdvancedAI.
              Notably, we use a negamax algorithm with deep pruning (beta is passed down).
              This is as opposed to the shallow alpha-beta implemented in AlphaBeta and AdvancedAI.

Copyright   : Paul Anderson
License     : AllRightsReserved
-}

module NegaMax where

import           Fanorona
import           FanoronaExtra
import           SimpleAI (Tree(..), val, children, qcut, cutoff, gameTree)
import           Eval
import           Data.List
import qualified Zobrist as Z

-- | Transposition table.
type Table = Z.Table Entry

-- | Auxiliary information passed during search.
data Ctx = AB Eval (Tree GameState) TargetDepth

type Eval = (GameState -> Double)
type TargetDepth = Int

-- | Information returned from a node during search.
type State = (Double,Table)

-- | Binary type indicating which player we are.
data Colour = Min | Max

-- | Alpha beta node types.
data NodeType = Exact | Cut | All
    deriving (Eq, Show)

-- | Information stored in the transposition table.
data Entry = Entry
    { ntype :: NodeType
    , score :: Double
    , relevance :: Int
    } deriving (Eq, Show)

-- | Empty table.
empty :: Table
empty = Z.empty

-- | Definitions for flipping our binary type,
-- and converting it to a signed number.
flipColour :: Colour -> Colour 
flipColour Max = Min 
flipColour Min = Max

sign :: Colour -> Double
sign colour = case colour of 
    Max -> 1
    Min -> -1

-- Replace the game tree in a Ctx instance.
setRoot :: Tree GameState -> Ctx -> Ctx
setRoot tree (AB e _ d) = AB (e) tree d

-- | Move ordering priorities. If there is a PV Node we check it first.
-- If there is no PV node or the node is no longer PV, we look at 
-- highest scoring All nodes to try for an early beta-cutoff.
priority :: Table -> Colour -> GameState -> Double
priority tt c state = case entry of 
        Nothing -> 10
        Just e -> case ntype e of 
            Cut -> 0
            Exact -> 1000 + (sgn*score e) + (10 *(fromIntegral (relevance e)))
            All -> 100 + (sgn*score e) + (10* (fromIntegral (relevance e)))
    where
        entry = Z.get state tt
        sgn = sign c

-- | Negates the first in a pair. A more explicit form of ((*(-1)).fst).
neg :: (Num a) => (a,b) -> (a,b)
neg (a,b) = (-a,b)

-- | Negamax search with AB pruning and table informed move ordering.
-- The table is passed in such a way as to follow the order of tree
-- evaluation (in-order traversal).
negaMax :: Ctx -> (Double,Double,Double) -> Table -> Colour -> State
negaMax ctx@(AB eval tst d) (oalpha,alpha,beta) tt colour = case checks of 
    (True,_,_) -> (alpha,newCut)
    (_,[],True) -> (alpha,newExact) 
    (_,[],False) -> (alpha,newAll)
    (_,x:xs,_) -> (max alpha' (fst next), snd next)
        where           
            -- Track new alpha and new table when searching the next child.
            next = negaMax ctx'' (oalpha, alpha', beta) tt' colour
            -- Narrowing the alpha, beta window.
            alpha' = max alpha $ value

            (value,tt') = case (cap,leaf) of 
                (_,True) -> (sgn*(eval (val x)), tt)
                (True,_) -> negaMax ctx' (oalpha,-1,beta) tt colour
                -- We propagate -(max oalpha alpha) as a beta bound 
                -- to cause cutoffs in descendants.
                (False,_) -> neg (negaMax ctx' (-beta,-1,-(max oalpha alpha)) tt colour')

            cap = isCapturing (val x)
            leaf = null (children x)

            orderedStates = sortBy (priorityDesc) $ children x

            xcolour = case isCapturing (val x) of 
                True -> colour 
                _ -> colour'

            -- | Reverse comparison by priority. This ensures that
            -- orderedStates is descending rather than ascending.
            priorityDesc y z = compare 
                (priority tt xcolour (val z)) 
                (priority tt xcolour (val y))

            ctx' = AB (eval) (Node (val x) orderedStates) d
            ctx'' = setRoot trimChild ctx
            trimChild = Node (val tst) xs
    where
        checks = (alpha > beta, children tst, alpha > oalpha)

        colour' = flipColour colour
        sgn = sign colour

        st = val tst

        -- Insert exact or bound result into the table.
        -- Scores are from perspective of the max player.
        newExact = Z.insert st (Entry Exact (sgn*alpha) d) tt
        newCut = Z.insert st (Entry Cut (sgn*alpha) d) tt
        newAll = Z.insert st (Entry All (sgn*alpha) d) tt

-- | Negamax algorithm. Instead of defining both min and max players, we use a symmetry of 
-- the two to define a single function which can either call itself again or the negative 
-- of itself on negative arguments.
-- "min(a,b) = -max(-a,-b)"
simpleNegaMax :: Eval -> Tree GameState -> Double -> Double
simpleNegaMax eval tst colour = case subtrees of 
    [] -> -1
    x:xs -> max value $ simpleNegaMax (eval) trimChild colour
        where           
            value = simpleNegaMaxNext (eval) x colour

            trimChild = Node (val tst) xs
    where
        subtrees = children tst

simpleNegaMaxNext :: Eval -> Tree GameState -> Double -> Double
simpleNegaMaxNext eval tree colour = case (cap,leaf) of 
    -- If the moving player was the min player, we need to multiply 
    -- by -1 so that the min player maximises the negative value.
    (_,True) -> colour * (eval (val tree)) 
    (True,_) -> simpleNegaMax (eval) tree colour
    (False,_) -> -(simpleNegaMax (eval) tree (-colour))
    where
        cap = isCapturing (val tree)
        leaf = null (children tree)

-- | Simple negamax (no quiescence).
snm :: (GameState -> Double) -> GameState -> Int -> Move
snm eval state depth = fst $ lmaximise (snd) rootMoves 
    where 
        rootMoves :: [(Move, Double)]
        rootMoves = (fmap.fmap) (searchFrom) $ 
                    (fmap.fmap) (mktree) msPairs 

        mktree = (cutoff depth) . gameTree

        searchFrom t = case minormax t of 
            1 -> simpleNegaMax (eval) t 1
            _ -> -(simpleNegaMax (eval) t (-1))

        minormax :: Tree GameState -> Double
        minormax t = case (turn (val t)) of 
            Turn p -> if (p==pl) then 1 else -1
            _ -> 1

        Turn pl = turn state

        msPairs = map (\m -> (m, nextState state m)) (moves1130 state)

-- | Simple negamax with quiescence.
snmQ :: (GameState -> Double) -> GameState -> Int -> Move
snmQ eval state depth = fst $ lmaximise (snd) rootMoves
    where 
        rootMoves :: [(Move, Double)]
        rootMoves = (fmap.fmap) (searchFrom) $ 
                    (fmap.fmap) (mktree) msPairs 

        mktree = (qcut depth) . gameTree

        searchFrom t = case minormax t of 
            1 -> simpleNegaMax (eval) t 1
            _ -> -(simpleNegaMax (eval) t (-1))

        minormax :: Tree GameState -> Double
        minormax t = case (turn (val t)) of 
            Turn p -> if (p==pl) then 1 else -1
            _ -> 1

        Turn pl = turn state

        msPairs = map (\m -> (m, nextState state m)) (moves1130 state)


-- | Generate lookup table. Uses iterative deepening.
makeTable :: Eval -> Tree GameState -> Int -> Table 
makeTable _ _ 0 = empty
makeTable eval tree depth = case (depth < 0) of
    True -> error "makeTable: negative depth"
    _ -> table'
    where 
        -- Use move ordering hints from a shallower
        -- search depth.
        table = makeTable (eval) tree (depth-1)

        (_,table') = negaMax ctx (-100,-100,100) table Max

        qtree = qcut depth $ tree
        ctx = AB (eval) qtree depth

-- | Iterative deepening, returning best move.
iteratedNM :: Eval -> GameState -> Int -> Move 
iteratedNM eval state depth = fst $ lmaximise (snd) movePriority -- 
    where
        movePriority :: [(Move, Double)] 
        movePriority = map (tableScore) $ moves1130 state

        tableScore mv = (mv, priority (table) Max (nextState state mv))

        table = makeTable (eval) (gameTree state) (depth+1)

-- | Lookup table without iterative deepening.
oneTimeTable :: Eval-> Tree GameState -> Int -> Table 
oneTimeTable eval tree depth = case (depth < 0) of
    True -> error "oneTimeTable: negative depth"
    _ -> table'
    where 
        (_,table') = negaMax ctx (-100,-100,100) empty Max

        qtree = qcut depth $ tree
        ctx = AB (eval) qtree depth

-- | Negamax with full features but no iterative deepening.
plainNM :: Eval -> GameState -> Int -> Move 
plainNM eval state depth = fst $ lmaximise (snd) movePriority
    where
        movePriority :: [(Move, Double)] 
        movePriority = map (tableScore) $ moves1130 state

        tableScore mv = (mv, priority (table) Max (nextState state mv))

        table = oneTimeTable (eval) (gameTree state) (depth+1)

-- | Below are the search functions together with evaluators / heuristics.

-- | Negamax with all features.
nmtC :: GameState -> Int -> Move
nmtC state n = case (turn state) of 
    Turn p -> iteratedNM (evaluator (evalC) p) state n
    _ -> error "nmtC : game already ended"

nmtA :: GameState -> Int -> Move
nmtA state n = case (turn state) of 
    Turn p -> iteratedNM (evaluator (evalA) p) state n
    _ -> error "nmtA : game already ended"

-- | Negamax without iterative deepening.
nmC :: GameState -> Int -> Move
nmC state n = case (turn state) of 
    Turn p -> plainNM (evaluator (evalC) p) state n
    _ -> error "nmC : game already ended"

-- | Simple Nega Max. No quiescence search, no lookup tables, no iterative deepening.
snmC :: GameState -> Int -> Move
snmC state n = case (turn state) of 
    Turn p -> snm (evaluator (evalC) p) state n
    _ -> error "snmC : game already ended"

-- | Simple Nega Max with quiescence search.
qnmC :: GameState -> Int -> Move 
qnmC state n = case (turn state) of 
    Turn p -> snmQ (evaluator (evalC) p) state n
    _ -> error "qnmC : game already ended"
