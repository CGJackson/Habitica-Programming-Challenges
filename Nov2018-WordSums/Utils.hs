-- Contains utility types and function for word sum solver

module Utils
( SymbolValue
, SymbolRange
, expandRange
, Hyperplane(..)
, normalVectorComponent
, sectionHyperplane
, dimensionOfHyperplane
, divUp
)where

type SymbolValue = Int

type SymbolRange = (SymbolValue, SymbolValue)

-- Expands a SymbolRange from a maximum and minimum value to a list of ossible values
expandRange:: SymbolRange -> [SymbolValue]
expandRange (lowerBound, upperBound)
    | lowerBound > upperBound = error "Invalid symbol range: Lower bound greater than upper bound"
    | otherwise = [lowerBound..upperBound]

data Hyperplane = Hyperplane {normal:: [SymbolValue],
                              offset:: SymbolValue
                             } deriving (Show)

-- returns a given component to the normal vector to a hyperplane
normalVectorComponent:: Hyperplane -> Int -> SymbolValue
normalVectorComponent Hyperplane{normal=v} index = v!!index

dimensionOfHyperplane:: Hyperplane -> Int
dimensionOfHyperplane hyperplane = length $ normal hyperplane

-- takes a cross section of a hyperplane normal to a coordinate axis
-- The normal axis to the cross section is given by index, the value
-- of the coordinate is given by coordinate.
sectionHyperplane:: Int -> SymbolValue -> Hyperplane -> Hyperplane
sectionHyperplane index coordinate (Hyperplane{normal=v, offset=b}) = Hyperplane {normal=newNormal, offset=newOffset}
    where newNormal = let (front, back) = splitAt index v in front ++ tail back
          newOffset = b - (v!!index)*coordinate

--performs integer division, trucating towards positive infinity
divUp ::(Integral a) => a -> a -> a
divUp n d
    |remainder == 0 = quotent
    |otherwise = quotent + 1
    where (quotent, remainder) = n `divMod` d
