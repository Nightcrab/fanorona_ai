{-|
Module      : RBTests
Description : Testing self balancing of the red-black tree.
Copyright   : Paul Anderson
License     : AllRightsReserved
-}

module RBTests where

import Testing
import RBTree

height :: Tree a b -> Int 
height (N _ l _ r) = 1 + (max (height l) (height r))
height (E) = 0

-- | Red-black "balanced" property (approximately balanced).
isBalanced :: Tree a b -> Bool 
isBalanced (N _ l _ r) = maxheight <= ((2*minheight) + 1)
    where 
        maxheight = max (height l) (height r)
        minheight = min (height l) (height r)
isBalanced (E) = True

testTree1 :: Tree Int Int 
testTree1 = (N B (N R E (1,2) E) (1,1) (N R E (1,2) E))

testTree2 :: Tree Int Int 
testTree2 = (N B (N R (N B (N R (N B E (1,4) E) (1,4) E) (1,3) E) (1,2) E) (1,1) (N R E (1,2) E))

testTree3 :: Tree Int Int 
testTree3 = 
    insert 1 4 $ 
    insert 2 5 $ 
    insert 3 6 $ 
    insert 4 7 $ 
    insert 5 8 $ 
    insert 6 9 $
    empty 

rbTests :: [Test]
rbTests = 
    [ TestGroup "RBTests unit tests"
        [ Test "height test #1" (assertEqual (height testTree1) (2))
        , Test "height test #2" (assertEqual (height testTree2) (5))
        , Test "isBalanced test #1" (assertEqual (isBalanced testTree1) (True))
        , Test "isBalanced test #2" (assertEqual (isBalanced testTree2) (False))
        ]
    , TestGroup "RBTree tests"
        [ Test "6 insertions, balance check" (assertEqual 
            (isBalanced testTree3) (True))
        ]
    ]