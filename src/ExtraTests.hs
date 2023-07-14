{-|
Module      : ExtraTests
Description : Testing functions from FanoronaExtra.
Copyright   : Paul Anderson
License     : AllRightsReserved
-}

module ExtraTests where

import Fanorona
import FanoronaExtra
import Testing

sqr :: Int -> Int 
sqr x = x*x

-- | Unit tests.
xTests :: Test
xTests = TestGroup "Testing FanoronaExtra.hs"
    [ Test "argmax test #1" (assertEqual (amax (sqr) 1 2) (2))
    , Test "argmax test #2" (assertEqual (amax (sqr) (-2) 1) (-2))
    , Test "argmin test #1" (assertEqual (amin (sqr) 1 2) (1))
    , Test "argmin test #2" (assertEqual (amin (sqr) (-2) 1) (1))
    , Test "lmaximise test" (assertEqual (lmaximise (sqr) [1,4,2,3,-5,0]) (-5))
    , Test "lminimise test" (assertEqual (lminimise (sqr) [1,4,2,3,-5,0]) (0))
    , Test "nextState test" (assertEqual 
        (nextState (initialState (9,5))
            (Move Paika (Location 0 0) (Location 0 0)))
        (initialState (9,5)))
    , Test "moves1130 test" (assertEqual 
        (moves1130 (initialState (9,5)))
        (legalMoves (initialState (9,5))))
    , Test "otherTurn test #1" (assertEqual
        (otherTurn (State (Turn Player1) None (0,0) [] []))
        (State (Turn Player2) None (0,0) [] []))
    , Test "otherTurn test #2" (assertEqual
        (otherTurn (State (Turn Player2) None (0,0) [] []))
        (State (Turn Player1) None (0,0) [] []))
    , Test "isCapturing test#1" (assertEqual
        (isCapturing (State (Turn Player1) None (0,0) [] []))
        False)
    , Test "isCapturing test#2" (assertEqual
        (isCapturing 
            (State (Turn Player1) (Captor (Location 0 0) []) (0,0) [] []))
        (True))
    , Test "isTerminal test#1" (assertEqual
        (isTerminal (State (Turn Player1) None (0,0) [] []))
        False)
    , Test "isTerminal test#2" (assertEqual
        (isTerminal (State (GameOver Draw) None (0,0) [] []))
        True)
    , Test "isNotTerminal test" (assertEqual
        (isNotTerminal (State (Turn Player1) None (0,0) [] []))
        True)
    , Test "locToIndex test" (assertEqual (locToIndex (Location 0 0)) (0))
    ]