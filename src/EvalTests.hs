{-|
Module      : EvalTests
Description : Testing evaluation functions.
Copyright   : Paul Anderson
License     : AllRightsReserved
-}
module EvalTests where

import Fanorona

import Testing
import FanoronaExtra
import Eval
import BoardStates

evalTests :: Test
evalTests = TestGroup "Testing Eval.hs"
    [ initialBoardTest
    , board1Tests 
    , board2Tests 
    , board3Tests
    ]

initialBoardTest :: Test 
initialBoardTest = TestGroup "Evaluating initial board" $
    [ Test "Piece counting (white)"
        (assertEqual (pieceCount (initialState (4,2))) 22)
    , Test "Weighted sum (white)" (assertEqual 
        (weighPos weightsB (initialState (4,2))) 
        (434 :: Double))
    , Test "Capturing (white)"
        (assertEqual (isQuiet (initialState (4,2))) True)
    ] ++
    [ Test "Piece counting (black)"
        (assertEqual (pieceCount initialState2) 22)
    , Test "Total mobility (black)"
        (assertEqual (totalMobil initialState2) 5)
    , Test "Average mobility (black)"
        (assertEqual (avgMobil initialState2) ((5/22) :: Double))
    , Test "Capturing (black)"
        (assertEqual (isQuiet initialState2) True)
    ]
        where initialState2 = otherTurn $ initialState (4,2)

board1Tests :: Test
board1Tests = TestGroup "Evaluating test board #1" 
    [ Test "Piece counting (white)"
        (assertEqual (pieceCount testState1) 1)
    , Test "Weighted sum (white)" (assertEqual 
        (weighPos weightsB testState1) 
        (18 :: Double))
    , Test "Total mobility (white)"
        (assertEqual (totalMobil testState1) 3)
    , Test "Average mobility (white)"
        (assertEqual (avgMobil testState1) (3 :: Double))
    , Test "Quietness (white)"
        (assertEqual (isQuiet testState1) True)
    ]

board2Tests :: Test 
board2Tests = TestGroup "Evaluating test board #2" $
    [ Test "Piece counting (white)"
        (assertEqual (pieceCount testState2) 1)
    , Test "Weighted sum (white)" (assertEqual 
        (weighPos weightsB testState2) 
        (18 :: Double))
    , Test "Piece difference (white)"
        (assertEqual (pieceDiff testState2) (-3))
    , Test "Total mobility (white)"
        (assertEqual (totalMobil testState2) 3)
    , Test "Average mobility (white)"
        (assertEqual (avgMobil testState2) (3 :: Double))
    , Test "Quietness (white)"
        (assertEqual (isQuiet testState2) True)
    ] ++ 
    [ Test "Piece counting (black)"
        (assertEqual (pieceCount testState2') 4)
    , Test "Weighted sum (white)" (assertEqual 
        (weighPos weightsB testState2') 
        (73 :: Double))
    , Test "Piece difference (black)"
        (assertEqual (pieceDiff testState2') (3))
    , Test "Total mobility (black)"
        (assertEqual (totalMobil testState2') 2)
    , Test "Average mobility (black)"
        (assertEqual (avgMobil testState2') (0.5 :: Double))
    , Test "Quietness (black)"
        (assertEqual (isQuiet testState2') True)
    ]
        where testState2' = otherTurn $ testState2

board3Tests :: Test 
board3Tests = TestGroup "Quietness at ending states"
    [ Test "Quietness (black wins)"
        (assertEqual (isQuiet testState3) True)
    , Test "Quietness (draw)"
        (assertEqual (isQuiet testState4) True)
    ]