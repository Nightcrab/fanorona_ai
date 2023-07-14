{-|
Module      : ZobristTests
Description : Testing zobrist functions.
Copyright   : Paul Anderson
License     : AllRightsReserved
-}

{-# LANGUAGE BinaryLiterals #-}

module ZobristTests where

import Testing
import FanoronaExtra
import Zobrist 
import BoardStates 
import Data.Bits
import Data.Int

type Entry = (Int,Int)

iiempty :: Table Entry 
iiempty = empty

testKeys :: [Int32]
testKeys = 
    [ 0b1010010100000000000000000000000
    , 0b0110100100000000000000000000000
    , 0b0110011010000000000000000000000
    ]

afterxor :: Int32 
afterxor = 0b1010101010000000000000000000000

testTable1 :: Table (Int,Int)
testTable1 = 
    insert testState1   (2,2) $
    insert testState2   (4,1) $
    insert testState5   (4,3) $
    insert testState5'  (7,4) $
    insert testState5'' (6,5) $
    iiempty

testTable2 :: Table (Int,Int)
testTable2 = 
    insert testState1   (1,1) $
    testTable1

zTests :: Test
zTests = TestGroup "Testing Zobrist.hs"
    [ Test "xorlist test on 3 elements" (assertEqual (xorlist testKeys) (afterxor))
    , Test "Z.cmap test" (assertEqual [2,4,6,8] (cmap (\x y-> x+1+y) [1,2,3,4]))
    , Test "1 black piece 1 white piece, white to move" 
        (assertEqual (stateToKey testState1) (
            (0b1110000100001101100000011010111 :: Int32) `xor` 
            (0b0110111011000011101011010101111 :: Int32) `xor`
            (0b0011111110111101011010000001111 :: Int32)))
    , Test "4 black pieces 1 white piece, black to move" 
        (assertEqual (stateToKey (otherTurn testState2)) (
            (0b0111010110000010101111001110011 :: Int32) `xor` 
            (0b1110000100001101100000011010111 :: Int32) `xor`
            (0b0100110110011001001101001001101 :: Int32) `xor`
            (0b1111010110101010101001110011011 :: Int32) `xor`
            (0b0111011000100101001011010100110 :: Int32) `xor`
            (0b1111100101000101111000111010000 :: Int32)))
    , Test "1 black piece 1 white piece, black to move, black capturing" 
        (assertEqual (stateToKey (testState4')) (
            (0b1110000100001101100000011010111 :: Int32) `xor` 
            (0b0110111011000011101011010101111 :: Int32) `xor`
            (0b0111010110000010101111001110011 :: Int32) `xor`
            (0b1100000011011110010010011010100 :: Int32)))
    , Test "inserting and getting one element" 
        (assertEqual (get testState5 (insert testState5 (1,3) iiempty)) (Just (1,3)))
    , Test "inserting and getting #1" 
        (assertEqual (get testState1 (testTable1)) (Just (2,2)))
    , Test "inserting and getting #2" 
        (assertEqual (get testState2 (testTable1)) (Just (4,1)))
    , Test "inserting and getting #3" 
        (assertEqual (get testState5 (testTable1)) (Just (4,3)))
    , Test "inserting and getting #4" 
        (assertEqual (get testState5' (testTable1)) (Just (7,4)))
    , Test "inserting and getting #5" 
        (assertEqual (get testState5'' (testTable1)) (Just (6,5)))
    , Test "overwrite test" 
        (assertEqual (get testState1 (testTable2)) (Just (1,1)))
    ]