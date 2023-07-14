{-|
Module      : BoardStates
Description : For the purposes of testing, a collection of game states. 
Copyright   : Paul Anderson
License     : AllRightsReserved
-}

module BoardStates where

import Fanorona
import FanoronaExtra
import FanoronaTests (toBoard)

toState :: Board -> GameState
toState b = State (Turn Player1) None (9,5) b []

toState' :: Board -> GameState
toState' b = State (Turn Player2) None (9,5) b []

initialBoard :: Board 
initialBoard = toBoard
    [ "xxxxxxxxx"
    , "xxxxxxxxx"
    , "xoxo xoxo"
    , "ooooooooo"
    , "ooooooooo" 
    ]

initialBoard2 :: Board 
initialBoard2 = toBoard
    [ "xxxx xxxx"
    , "xxxx xxxx"
    , "xoxooxoxo"
    , "oooo oooo"
    , "ooooooooo" 
    ]

initialBoard3 :: Board 
initialBoard3 = toBoard
    [ "xxxxxxxxx"
    , "xxxxxxxxx"
    , "xox o oxo"
    , "ooooooooo"
    , "ooooooooo" 
    ]

initialBoard4 :: Board 
initialBoard4 = toBoard
    [ "xxxxxx xx"
    , "xxxxx xxx"
    , "xoxooxoxo"
    , "ooo ooooo"
    , "ooooooooo" 
    ]

initialBoard5 :: Board 
initialBoard5 = toBoard
    [ "xx xxxxxx"
    , "xxx xxxxx"
    , "xoxooxoxo"
    , "ooooo ooo"
    , "ooooooooo" 
    ]

iniState :: GameState
iniState = toState initialBoard

testState1 :: GameState 
testState1 = toState . toBoard $
    [ "x        " 
    , "         " 
    , "         " 
    , "         " 
    , "        o" 
    ]

testState2 :: GameState
testState2 = toState . toBoard $
    [ "x        " 
    , "x        " 
    , "         " 
    , "o x      " 
    , "x        " 
    ]


testState2'' :: GameState
testState2'' = otherTurn . toState . toBoard $
    [ "         " 
    , "         " 
    , "o        " 
    , "  x      " 
    , "x        " 
    ]


testState3 :: GameState
testState3 = (\b -> State (GameOver (Winner Player2)) None (9,5) b []) 
    . toBoard $
    [ "x        " 
    , "         " 
    , "         " 
    , "         " 
    , "         " 
    ]

testState4 :: GameState
testState4 = (\b -> State (GameOver Draw) None (9,5) b []) 
    . toBoard $
    [ "x        " 
    , "         " 
    , "         " 
    , "         " 
    , "        o" 
    ]

testState4' :: GameState
testState4' = (\b -> State (Turn Player2) (Captor (Location 0 0) []) (9,5) b []) 
    . toBoard $
    [ "x        " 
    , "         " 
    , "         " 
    , "         " 
    , "        o" 
    ]

testState5 :: GameState 
testState5 = otherTurn $ toState $ toBoard $
    [ "xx xxxxxx" 
    , "xxx xxxxx" 
    , "xoxooxoxo" 
    , "ooooo ooo" 
    , "ooooooooo" 
    ]

testState5' :: GameState 
testState5' = otherTurn $ toState $ toBoard $
    [ "xx xxxxxx" 
    , "xxxxxxxxx" 
    , "xo ooxoxo" 
    , "o ooo ooo" 
    , " oooooooo" 
    ]

testState5'' :: GameState 
testState5'' = otherTurn $ toState $ toBoard $
    [ "xx  xxxxx" 
    , "xxxxxxxxx" 
    , "xox oxoxo" 
    , "ooo o ooo" 
    , "ooo ooooo" 
    ]