-- Solver for word sums
--
-- In a word sum each letter represents a spesific digit
-- The problem is to assign values to each letter so that
-- the equation is correct. For example
--
-- SEND + MORE = MONEY
--
-- If there are N distinct letters then the ptoblem
-- amounts to finding the points on an N-1 dimensional
-- hyperplane with natural number coordinates <= 9

module Solver 
( solve
) where

import qualified Data.Array as Arr
import Utils (WordSumProblem(offset,symbolBounds), constructWordSumProblem, updateWordSumProblemBounds, normalVectorComponent, normalVectorList, symbolBoundsList, symbolBound, sectionHyperplane, dimensionOfWordSumProblem, Index, RangesArray, SymbolValue, SymbolRange, NormalVector, expandRange, divUp, skipValue)


-- Finds the largest and smallest possible values for the symbol at position 'index' consistent with the 
-- known bounds on the other variables. This amounts to finding the points where the hyperplane 
-- intercpets the rectangular prism defined by the bonds on the other symbols
intercepts :: WordSumProblem -> Index -> SymbolRange
intercepts problem index = (minSum `divUp` indexCoeff, maxSum `div` indexCoeff)
    where   indexCoeff = normalVectorComponent problem index
            extremisingSumTerms coeff (lowerBound, upperBound)
                | (signum coeff) == (signum indexCoeff) = (-coeff*upperBound,-coeff*lowerBound)
                | otherwise = (-coeff*lowerBound,-coeff*upperBound)
            sumTerms = [extremisingSumTerms coeff symbolBounds |(i, coeff, symbolBounds) <- zip3 [0,1..] (normalVectorList problem) (symbolBoundsList problem), i /= index]
            (minSum, maxSum) = (foldr (\(mn,mx) (amn,amx) -> (mn+amn,mx+amx)) (offset problem,offset problem) sumTerms) 

-- Finds the new bounds on the range of one symbol, at position 'index',given the bounds on the other symbols
-- and the current known maximum and minimum, given in 'currentRange'. Returns Nothing if no values are possible
findNewRange :: WordSumProblem -> Index -> Maybe SymbolRange
findNewRange problem index
    | newMax < newMin = Nothing
    | otherwise = Just (newMin, newMax) 
    where (lowerIntercept, upperIntercept) = intercepts problem index
          (currentMin, currentMax) = symbolBound index problem
          newMin = max currentMin lowerIntercept
          newMax = min currentMax upperIntercept

-- Given a hyperplane and a list of ranges of possible values for symbols
-- returns a a new list of possible values of symbols obtained by considering
-- where the bonds on the other variables intersect with the hyperplane
-- returns Nothing if no integer solutions are possible
updatedRanges :: WordSumProblem -> Maybe RangesArray
updatedRanges problem = toMaybeArray (map (findNewRange problem) rangeArrayIndicies)
    where   rangeArrayBounds = Arr.bounds $ symbolBounds problem 
            rangeArrayIndicies = Arr.range rangeArrayBounds
            toMaybeArray = sequence . Arr.listArray rangeArrayBounds

-- Explicity trys all possible values for one symbol and returns the possible results
insertTrialValues:: WordSumProblem -> [[SymbolValue]]
insertTrialValues problem
    | symbolsRemaining == 0 = []
    | symbolsRemaining == 1 = map (:[]) trialValues
    | otherwise = concat $ map solutionsWithSubstitution trialValues
    where symbolsRemaining = dimensionOfWordSumProblem problem
          ranges = symbolBounds problem
          trialValues = expandRange (ranges Arr.!0)
          remainingRanges = skipValue 0 ranges
          problemWithTrialValue val = let (newNormal,newOffset) = sectionHyperplane 0 val problem in constructWordSumProblem newNormal newOffset remainingRanges
          solveWithSubstitution = solve . problemWithTrialValue 
          solutionsWithSubstitution subsValue = [subsValue:solution| solution <- (solveWithSubstitution subsValue)]

-- Recursivly solves the Word Sum problem by alternatly trying to tighten the bounds
-- on the possible values of each symbol and substituting in values to try all possiblities
solve:: WordSumProblem -> [[SymbolValue]]
solve problem  = maybe [] insertTrialValues reducedProblem
        where reducedProblem = fmap (updateWordSumProblemBounds problem) (updatedRanges problem)
              
              
              
