{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Types.Tree where

import Data.Maybe
import Control.Monad
import Test.QuickCheck
import GHC.Generics( Generic )
import Control.DeepSeq


type Tree a = Maybe (Node a)

data Node a =
	Node {
		node_value :: a,
		node_left :: Tree a,
		node_right :: Tree a
	}
	deriving( Eq, Ord, Functor, Generic, NFData )

instance (Show a) => Show (Node a) where
	show (Node x maybeL maybeR) =
		case (maybeL,maybeR) of
			(Nothing, Nothing) -> "Leaf " ++ show x
			(Just l, Nothing) -> concat [ "Node ", show x, " (", show l, ") ()" ]
			(Nothing, Just r) -> concat [ "Node ", show x, "() (", show r, ")" ]
			(Just l, Just r) -> concat [ "Node ", show x, " (", show l, ") (", show r, ")" ]

---------------------------------------------------------------------------
-- pseudo constructors:
---------------------------------------------------------------------------

leaf :: a -> Tree a
leaf x = Just $ Node x Nothing Nothing

node :: a -> Tree a -> Tree a -> Tree a
node x l r = Just $ Node x l r

children :: forall a . Node a -> [Node a]
children tree =
	concatMap maybeToList $
	([node_left tree, node_right tree] :: [Maybe (Node a)])

type TreePos = (Int, Int)

{-
0
0 1 
01 23
0123 4567
...
-}
positionsTree :: Tree TreePos
positionsTree =
	unfoldTree f (0,0)
	where
		f :: (Int, Int) -> ((Int, Int), (Maybe (Int, Int), Maybe (Int,Int)))
		f pos@(depth, index) =
			(pos, (Just (depth+1, index*2), Just (depth+1, index*2+1)))

zipTree :: Tree a -> Tree b -> Tree (a,b)
zipTree a b =
	case (a, b) of
		(Just (Node x l1 r1), Just (Node y l2 r2)) ->
			node (x,y) (zipTree l1 l2) (zipTree r1 r2)
		_ -> Nothing

unfoldTreeM :: Monad m => (seed -> m (a, (Maybe seed, Maybe seed))) -> seed -> m (Tree a)
unfoldTreeM f seed =
	do
		(val, (leftSeed, rightSeed)) <- f seed
		l <-
			maybe (return Nothing) (unfoldTreeM f) leftSeed
		r <-
			maybe (return Nothing) (unfoldTreeM f) rightSeed
		return $ (node val) l r

unfoldTree :: (seed -> (a, (Maybe seed, Maybe seed))) -> seed -> (Tree a)
unfoldTree f seed =
	let (val, (leftSeed, rightSeed)) = f seed
	in
		(node val) (unfoldTree f =<< leftSeed) (unfoldTree f =<< rightSeed)

treeArbGen :: (Arbitrary a) => Gen (Tree a)
treeArbGen = sized treeFromSizeGen

treeFromSizeGen :: (Arbitrary a) => Int -> Gen (Tree a)
treeFromSizeGen 0 = return Nothing
treeFromSizeGen n
	| n>0 =
		do
			val <- arbitrary
			liftM2 (node val) subtree subtree
	| otherwise = error "treeFromSizeGen error: negative size"
  where
		subtree = treeFromSizeGen (n `div` 2)

{-
treeFromSizeGen :: (Arbitrary a) => Int -> Gen (Tree a)
treeFromSizeGen 0 = return Nothing
treeFromSizeGen n | n>0 =
	oneof $
	[ liftM leaf arbitrary
	, do { val <- arbitrary; liftM2 (node val) subtree subtree }
	]
  where
		subtree = treeFromSizeGen (n `div` 2)
-}
