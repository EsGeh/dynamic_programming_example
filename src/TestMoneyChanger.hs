{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.Map as M
import Data.Array.IArray
import Data.Maybe
import Data.List


main :: IO ()
main =
	do
		putStrLn "how much money do you want to change?"
		money <- read <$> getLine
		putStrLn $ "your change:"
		putStrLn $ showDistribution $ fromJust $ changeDistributionDynamic money

---------------------------------------------------------------------------
-- both following functions try to represent an integer as
-- a sum of coins which uses the minimal number of coins
---------------------------------------------------------------------------
changeDistributionDynamic, changeDistributionRecursive :: Int -> Maybe Distribution

-- evaluate using dynamic programming evaluation strategy (fast)
changeDistributionDynamic =
	dynamicEvaluate $ changeDistribution coins

-- naive recursive evaluation (really slow!)
changeDistributionRecursive =
	changeDistribution coins getSubSolution
	where
		getSubSolution = changeDistributionRecursive

coins :: [CoinType]
coins = [1,2,5,10,20,50,100,200,1000,5000,10000]


---------------------------------------------------------------------------
-- dynamic programming:
---------------------------------------------------------------------------

dynamicEvaluate :: forall res . ((Int -> Maybe res) -> Int -> Maybe res) -> Int -> Maybe res
dynamicEvaluate f amount = a!amount
	where
		a :: Array Int (Maybe res)
		a = listArray (0,amount) (startVal : map f' [1..amount])
		f' = f getSubSolution
		startVal = f' 0
		getSubSolution i
			| i >=0 = (a!i)
			| otherwise = Nothing


---------------------------------------------------------------------------
-- problem specific code:
---------------------------------------------------------------------------

type CoinType = Int

-- for every CoinType: how many times is it used
type Distribution = M.Map CoinType Int

changeDistribution :: [CoinType] -> (Int -> Maybe Distribution) -> Int -> Maybe Distribution
changeDistribution coinTypes getSubSolution change
	| change < 0 = Nothing
	| change == 0 = Just $ M.fromList $ coinTypes `zip` (repeat 0)
	| otherwise =
		case
			-- Theta( |coinTypes| * T(getSubSolution))
			catMaybes $ map `flip` coinTypes $ \coinType ->
				fmap (M.adjust (+1) coinType)  $
				getSubSolution (change - coinType) 
		of
			[] -> Nothing
			subSolutions ->
				-- Theta( |coinTypes| )
				Just $ minimumOn numberOfCoins subSolutions

numberOfCoins :: Distribution -> Int
numberOfCoins = sum . map snd . M.toList

showDistribution :: Distribution -> String
showDistribution = showDistribution' . M.filter (/=0)
	where
	showDistribution' d =
		(concat [show (numberOfCoins d), " Münzen: ", show summedUp, " = "] ++) $
		intercalate " + " $
		map `flip` (reverse $ sortOn fst . M.toList $ d) $ \(coinType, times) ->
			concat [ show times, "x", show coinType ]
		where
			summedUp =
				sum $
				map (uncurry (*)) (M.toList d)

---------------------------------------------------------------------------
-- Utilities:
---------------------------------------------------------------------------

minimumOn :: (Ord b) => (a -> b) -> [a] -> a
minimumOn f = minimumBy $ \x y -> f x `compare` f y
