{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Types.Tree

import Test.QuickCheck
import Criterion.Main

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Identity


main :: IO ()
main =
	defaultMain $
	[
		bgroup "maxIndepNodes" $
		[ env (setupTree 10) $ \ ~(size, tree) -> singleBenchmark (size, tree)
		]
	]

setupTree :: Int -> IO (Int, Tree Int)
setupTree size =
	(size,) <$>
	(generate $ treeFromSizeGen size)
	

singleBenchmark :: (Int, Tree Int) -> Benchmark
singleBenchmark (size, tree) =
	bench ("size " ++ show size) $
		whnf maxIndepNodes tree

---------------------------------------------------------------------------
-- dynamic programming using a "Cache"
---------------------------------------------------------------------------

type Cache key val = M.Map key val

type CacheM' key val a = State (Cache key val) a

type CacheM a res = CacheM' TreePos (MaxIndepRes a) res

type MaxIndepRes a = [(TreePos, a)]

-- evaluate using a cache of partial results (fast)
maxIndepNodes :: forall a . (Num a, Ord a) => Tree a -> MaxIndepRes a
maxIndepNodes tree =
	evalState `flip` M.empty $
	maxIndepNodesWorkWithCache $
	positionsTree `zipTree` tree
	where
		maxIndepNodesWorkWithCache :: 
			Tree (TreePos, a) -> CacheM a (MaxIndepRes a)
		maxIndepNodesWorkWithCache = maxIndepNodesWork $ \case
				Nothing ->
					maxIndepNodesWork maxIndepNodesWorkWithCache Nothing
				Just node@Node{ node_value=(pos,_) } ->
					do
						mSubSolution <- gets (M.lookup pos)
						case mSubSolution of
							Nothing ->
								do
									res <- maxIndepNodesWork maxIndepNodesWorkWithCache $ Just node
									modify $ M.insert pos res
									return res
							Just res ->
								return res

-- naive recursive evaluation strategy (really slow!
maxIndepNodesRec :: forall a . (Num a, Ord a) => Tree a -> MaxIndepRes a
maxIndepNodesRec tree =
	runIdentity $ 
	maxIndepNodesWork' $
	positionsTree `zipTree` tree
	where
		maxIndepNodesWork' :: 
			Monad m =>
			Tree (TreePos, a) -> m (MaxIndepRes a)
		maxIndepNodesWork' =
			maxIndepNodesWork $ maxIndepNodesWork'

---------------------------------------------------------------------------
-- problem specific code:
---------------------------------------------------------------------------

{- |
	given a method how to calculate subsolutions
	calculates the maximum independent set of nodes in a tree
-}
maxIndepNodesWork ::
	forall m a .
	(Monad m, Num a, Ord a) =>
	(Tree (TreePos,a) -> m (MaxIndepRes a))
	-> Tree (TreePos, a) -> m (MaxIndepRes a)
maxIndepNodesWork getSubSolution = \case
	Nothing -> return []
	Just tree ->
		do
			subsubNodes <-
				-- solve the problem for all grandchildren
				fmap concat $ mapM (getSubSolution . Just) $
				children =<< children tree
			subNodes <-
				-- solve the problem for immediate children
				fmap concat $ mapM (getSubSolution . Just) $
				children tree
			if (snd . node_value) tree + (sum . map snd) subsubNodes > (sum . map snd) subNodes
				then return $ node_value tree : subsubNodes
				else return $ subNodes
