{-|
Module      : RBTree
Description : Haskell Red-Black trees.
Copyright   : Paul Anderson
License     : AllRightsReserved
-}

module RBTree where

type Map k v = Tree k v

data Color = R | B
    deriving Show

data Tree k v = E | N Color (Tree k v) (k,v) (Tree k v) 
    deriving Show

empty :: Tree k v 
empty = E

-- | Regular binary search.
get :: (Ord k) => k -> Tree k v -> Maybe v
get _ E = Nothing
get x (N _ left (key,val) right)
    | x < key     = get x left
    | x == key    = Just val
    | otherwise   = get x right

-- | Insert a red node, balance the tree and make the root black.
insert :: (Ord k) => k -> v -> Tree k v -> Tree k v
insert k v s = N B p q r
    where
        N _ p q r = ins s
        ins E = N R E (k,v) E
        ins (N B a (y,y') b)
            | k<y = balance (ins a) (y,y') b
            | k>y = balance a (y,y') (ins b)
            | otherwise = N B a (k,v) b
        ins (N R a (y,y') b)
            | k<y = N R (ins a) (y,y') b
            | k>y = N R a (y,y') (ins b)
            | otherwise = N R a (k,v) b

-- | Cases for balancing the tree, if a red node is inserted
-- below another red node. Otherwise, do nothing.
balance :: Tree k v -> (k,v) -> Tree k v -> Tree k v
balance (N R a x b) y (N R c z d) = N R (N B a x b) y (N B c z d)
balance (N R (N R a x b) y c) z d = N R (N B a x b) y (N B c z d)
balance (N R a x (N R b y c)) z d = N R (N B a x b) y (N B c z d)
balance a x (N R b y (N R c z d)) = N R (N B a x b) y (N B c z d)
balance a x (N R (N R b y c) z d) = N R (N B a x b) y (N B c z d)
balance a x b = N B a x b
