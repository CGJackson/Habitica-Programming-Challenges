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

import Utils (Hyperplane(..), normalVectorComponent, sectionHyperplane, SymbolValue, SymbolRange, expandRange, divUp)

--module Solver 
--( solve
--) where

-- Finds the largest and smallest possible values for the symbol at position 'index' consistent with the 
-- known bounds on the other variables. This amounts to finding the points where the hyperplane 
-- intercpets the rectangular prism defined by the bonds on the other symbols
intercepts :: Hyperplane -> [SymbolRange] -> Int -> SymbolRange
intercepts hyperplane ranges index = (minSum `divUp` indexCoeff, maxSum `div` indexCoeff)
    where   indexCoeff = normalVectorComponent hyperplane index
            parity = signum indexCoeff
            sumTerms = [if (signum coeff) == parity then (-coeff*upperLim,-coeff*lowerLim) else (-coeff*lowerLim,-coeff*upperLim)|(i, coeff, (lowerLim, upperLim)) <- zip3 [0,1..] (normal hyperplane) ranges, i /= index]
            (minSum, maxSum) = (foldr (\(mn,mx) (amn,amx) -> (mn+amn,mx+amx)) (offset hyperplane,offset hyperplane) sumTerms) 

-- Finds the new bounds on the range of one symbol, at position 'index',given the bounds on the other symbols
-- and the current known maximum and minimum, given in 'currentRange'. Returns Nothing if no values are possible
findNewRange :: Hyperplane -> [SymbolRange] -> Int -> Maybe SymbolRange
findNewRange hyperplane ranges index 
    | newMax < newMin = Nothing
    | otherwise = Just (newMin, newMax) 
    where (lowerIntercept, upperIntercept) = intercepts hyperplane ranges index
          currentRange = ranges!!index
          currentMin = fst currentRange
          currentMax = snd currentRange
          newMin = max currentMin lowerIntercept
          newMax = min currentMax upperIntercept

-- Given a hyperplane and a list of ranges of possible values for symbols
-- returns a a new list of possible values of symbols obtained by considering
-- where the bonds on the other variables intersect with the hyperplane
-- returns Nothing if no integer solutions are possible
updateRanges :: Hyperplane -> [SymbolRange] -> Maybe [SymbolRange]
updateRanges hyperplane ranges = sequence (map updateRange (zip ranges [0,1..]))
    where updateRange (_, index) = findNewRange hyperplane ranges index

-- Explicity trys all possible values for one symbol and returns the possible results
insertValues:: Hyperplane -> [SymbolRange] -> [[SymbolValue]]
insertValues _ [] = []
insertValues _ (possibleValueRange:[]) = map (:[]) (expandRange possibleValueRange)
insertValues hyperplane (guessRange:remainingRanges) = [[x:solution| solution <- (solveWithSubstitution x)] |x <- (expandRange guessRange)]
    where solveWithSubstitution x = solve (sectionHyperplane 0 x hyperplane) remainingRanges

-- Recursivly solves the Word Sum problem by alternatly trying to tighten the bounds
-- on the possible values of each symbol and substituting in values to try all possiblities
solve:: Hyperplane -> [SymbolRange] -> [[SymbolValue]]
solve hyperplane ranges = maybe [] tryRemainingPossibilities newRanges
        where newRanges = updateRanges hyperplane ranges
              tryRemainingPossibilities = insertValues hyperplane
              
              
