-- Contains utility types and function for word sum solver

module Utils
( SymbolValue
, SymbolRange
, expandRange
, Index
, RangesArray
, NormalVector
, WordSumProblem(offset,symbolBounds,normal)
, constructWordSumProblem
, updateWordSumProblemBounds
, normalVectorList
, normalVectorComponent
, symbolBoundsList
, symbolBound
, sectionHyperplane
, dimensionOfWordSumProblem
, divUp
, skipValue
)where

import qualified Data.Array as Arr

type SymbolValue = Int

type SymbolRange = (SymbolValue, SymbolValue)

type Index = Int

type NormalVector = Arr.Array Index SymbolValue

type RangesArray = Arr.Array Index SymbolRange

-- Expands a SymbolRange from a maximum and minimum value to a list of ossible values
expandRange:: SymbolRange -> [SymbolValue]
expandRange (lowerBound, upperBound)
    | lowerBound > upperBound = error "Invalid symbol range: Lower bound greater than upper bound"
    | otherwise = [lowerBound..upperBound]

data WordSumProblem = WordSumProblem {normal:: NormalVector,
                                      offset:: SymbolValue,
                                      symbolBounds:: RangesArray
                                     } deriving (Show)

-- Returns a WordSumProblem with the given normal, offset and symbolBounds. Enforces the constraint that
-- there should be one symbolBound for each element in the normal vecotr, i.e. that normal and symbolBounds 
-- should have the same length. Raises an error if this is not satisfied
constructWordSumProblem:: NormalVector -> SymbolValue -> RangesArray -> WordSumProblem
constructWordSumProblem normal' offset' symbolBounds'
    | dimNomral /= dimBounds = error dimensionalMismatchErrorMessage
    | otherwise  = WordSumProblem{normal=normal', offset=offset', symbolBounds=symbolBounds'}
        where alength = Arr.rangeSize . Arr.bounds
              dimNomral = alength normal'
              dimBounds = alength symbolBounds'
              dimensionalMismatchErrorMessage = "Invalid WordSumProblem: Dimensions of normal vector " ++ (if dimNomral < dimBounds then "less" else "greater") ++ " than number of symbol bounds."

updateWordSumProblemBounds:: WordSumProblem -> RangesArray -> WordSumProblem
updateWordSumProblemBounds WordSumProblem{normal=normal', offset=offset'} newBounds = constructWordSumProblem normal' offset' newBounds


-- returns the normal vector in a WordSumProblem as a list
normalVectorList:: WordSumProblem -> [SymbolValue]
normalVectorList WordSumProblem{normal=v} = Arr.elems v

-- returns a given component to the normal vector to a hyperplane
normalVectorComponent:: WordSumProblem -> Index -> SymbolValue
normalVectorComponent WordSumProblem{normal=v} index = v Arr.!index

problemOffset :: WordSumProblem -> SymbolValue
problemOffset = offset

symbolBoundsList:: WordSumProblem -> [SymbolRange]
symbolBoundsList WordSumProblem{symbolBounds=symbolBounds'} = Arr.elems symbolBounds'

symbolBoundsArray:: WordSumProblem -> RangesArray
symbolBoundsArray = symbolBounds

symbolBound:: Index -> WordSumProblem -> SymbolRange
symbolBound i WordSumProblem{symbolBounds=symbolBounds'} = symbolBounds' Arr.!i

dimensionOfWordSumProblem:: WordSumProblem -> Int
dimensionOfWordSumProblem WordSumProblem{normal=v} = Arr.rangeSize $ Arr.bounds v

-- takes a cross section of a hyperplane normal to a coordinate axis
-- The normal axis to the cross section is given by index, the value
-- of the coordinate is given by coordinate.
sectionHyperplane:: Index -> SymbolValue -> WordSumProblem -> (NormalVector, SymbolValue)
sectionHyperplane index coordinate (WordSumProblem{normal=v, offset=b}) = (newNormal, newOffset)
    where newNormal = skipValue index v
          newOffset = b - (v Arr.!index)*coordinate

-- Given an index and an array returns a new array with the same lower bound and the upper bound reduced by one
-- The elements of the new array are the same as the old one but the value with the given index is missing
skipValue:: (Arr.Ix a, Num a) => a -> Arr.Array a b -> Arr.Array a b
skipValue skipIndex array = Arr.ixmap (lowerBnd, upperBnd-1) indexSkipper array
    where (lowerBnd, upperBnd) = Arr.bounds array
          indexSkipper i | i < skipIndex = i
                         | i >= skipIndex = i + 1

--performs integer division, trucating towards positive infinity
divUp ::(Integral a) => a -> a -> a
divUp n d
    |remainder == 0 = quotent
    |otherwise = quotent + 1
    where (quotent, remainder) = n `divMod` d
