{-|
Module      : Keys
Description : Fanorona Zobrist keys.
Copyright   : Paul Anderson
License     : AllRightsReserved
-}

{-# LANGUAGE BinaryLiterals #-}

module Keys where

import Data.Int

whiteTurn :: Int32 
whiteTurn = 0b0011111110111101011010000001111

blackTurn :: Int32 
blackTurn = 0b0111010110000010101111001110011

capturingPiece :: [Int32]
capturingPiece = 
    [ 0b1100000011011110010010011010100 
    , 0b1100001010000001111011110001001 
    , 0b1110011110100101101111110111111 
    , 0b0110100100110011110100000111101 
    , 0b0101000000110101010011100011000 
    , 0b1110110000100100111101001001101 
    , 0b1000111011100010011111101100110 
    , 0b0001100011111010110010111100000 
    , 0b0100000111010110110110000011110 
    , 0b1101010010110001111110100011010 
    , 0b1001100010001111001111111111011 
    , 0b0110101111101101100000001110010 
    , 0b1101000010000000101100000110111 
    , 0b1101110100100010100110011001101 
    , 0b1001011101010011000010101010100 
    , 0b1000110001111011000110010000010 
    , 0b1011100001100110010001101000100 
    , 0b1110101000011000100011010000111 
    , 0b0110111110101010001011100110010 
    , 0b1111101000101001010011110011101 
    , 0b1110000100000011001000001010101 
    , 0b0000101001100010000100010111011 
    , 0b0100100100010101011000001010000 
    , 0b1101100100100101101011010111001 
    , 0b0000101010010011000101110111001 
    , 0b0101101000010110010011010000100 
    , 0b1100000011101010011011001000101 
    , 0b1001000011000010001000011011101 
    , 0b0011101111000111110100010011111 
    , 0b0011111001101000100100111100110 
    , 0b0010010101110100110111110000001 
    , 0b0000100001100001010000000110110 
    , 0b0010100011010100010110010111110 
    , 0b1000011000110010100111000001110 
    , 0b0001000100100101011110100101011 
    , 0b1111100010101010010010110011011 
    , 0b1111001000010000010100100111000 
    , 0b1011011000100011010010111000010 
    , 0b0010000111000011001110100011001 
    , 0b1000100001010001111100011110101 
    , 0b1100001100000011100011110110100 
    , 0b0110010000010100101110101110010 
    , 0b0101011100011000011011010010110 
    , 0b1101101101111000000000111100101 
    , 0b0000010110101110000001100101100
    ] 

whitePieces :: [Int32]
whitePieces = 
    [ 0b0110000000010110001000010000011
    , 0b1111110010100001001101000110100
    , 0b1000110001110001011001011111001
    , 0b1100100111110101001100001111110
    , 0b0101100001100001110110101101001
    , 0b0010001000011000111101111101110
    , 0b1110111110100110000110111001001
    , 0b1101011001011001000101111010100
    , 0b0101100101111010101000110100111
    , 0b1000011001000101001100111100110
    , 0b0011101111110100110000001011001
    , 0b1000010110110110101000100101010
    , 0b0111101010111000001011111100111
    , 0b1110111001000110000100011110011
    , 0b1010111100001010001101100011110
    , 0b0000000000100000000010100111011
    , 0b0001101010010010101001010010000
    , 0b0100000010101011000101001110000
    , 0b0111100000100110010011101000001
    , 0b1111110111101000100101111000010
    , 0b0111101010110110111000110101110
    , 0b1101100001010101011000000101001
    , 0b0010010011101110010111001111111
    , 0b1100100001000100100011000100010
    , 0b1100010001111100001011010111011
    , 0b0111011010001111110110111110010
    , 0b0100111010001101111001111001111
    , 0b1111010110101010101001110011011
    , 0b1000001100010101100010001000100
    , 0b0010111110110000100101010011101
    , 0b1010000100010010011100101110000
    , 0b1001111111010001100101010111000
    , 0b1110000111000001111011101000001
    , 0b0110010011001101001001110101110
    , 0b1111010001110110011000000000100
    , 0b0011011101010101101101010101000
    , 0b1011011101110101000001110011011
    , 0b1000100100000000100011011010110
    , 0b1010100000100011100101110100001
    , 0b1001011111100110101111011000010
    , 0b0000010111101000111101010010011
    , 0b0000111100100101110100110111111
    , 0b1011111100011000011010010100111
    , 0b0011001101110001111010111110101
    , 0b0110111011000011101011010101111 
    ]

blackPieces ::[Int32]
blackPieces = 
    [ 0b1110000100001101100000011010111
    , 0b0100111101100100110100001111101
    , 0b0000011110111001010001001101000
    , 0b1111000011011011110111000010001
    , 0b1001110011110010101011010100001 
    , 0b1010111011011011011111101110011 
    , 0b0110111100001110001011101000110 
    , 0b1111101000101000101000001000111 
    , 0b0110001001101001111011110001010 
    , 0b0100110110011001001101001001101 
    , 0b0011000110111100001001100110100 
    , 0b1010001011000111100011100000100 
    , 0b1000110001100110110101111001100 
    , 0b0001000101011011010110111000000 
    , 0b1110000011001000001010101111011 
    , 0b1100101100001101101011101101100 
    , 0b0101000010100010010001111110011 
    , 0b0001001000111001111001110100100 
    , 0b0101101101000110011111101001110 
    , 0b1001010100110010101010110111010 
    , 0b1100111011000000010100100110001 
    , 0b1000111001101001010010111010111 
    , 0b0110001101110111111001111011010 
    , 0b1011001001111111111010000111111 
    , 0b1101101111000010001000100001100 
    , 0b0001001000101111001101001110001 
    , 0b0101010011011110011001010100100 
    , 0b1110000110011110100000110000111 
    , 0b1010101011001100101110111000010 
    , 0b0111011000100101001011010100110 
    , 0b0000101010111100001100111001000 
    , 0b0111111101011000101011010000010 
    , 0b0010101100001011010010001101110 
    , 0b0011010111111110111111111111101 
    , 0b1000101001010011110011000011111 
    , 0b1001000110111111100111111110100 
    , 0b1111100101000101111000111010000 
    , 0b0100010000100000101100001010010 
    , 0b0101011001101110011111100000101 
    , 0b1011011001100000100001010101000 
    , 0b1101111010111110001011101111111 
    , 0b0111101110100101001000101111011 
    , 0b0001111010000001001100110111011 
    , 0b1010001100110010000100101010101 
    , 0b1101110001000000100000111110011 
    ]
