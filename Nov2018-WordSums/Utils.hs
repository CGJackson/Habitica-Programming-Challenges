-- Contains utility types and function for word sum solver

module Utils
( ErrorMessage
, SymbolValue
, SymbolRange
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
, SymbolData(..)
, suplementSymbolData
, InputStringData(..)
, mapC
, mapCO
, combineWordData
, convertInputData
, divUp
, deleteValueL
, skipValue
, startsWith
, splitPairsList
)where

import qualified Data.Array as Arr
import qualified Data.Map as Map
import Data.List (sortBy)
import qualified Data.Sequence as Seq

type ErrorMessage = String

type SymbolValue = Int

type SymbolRange = Seq.Seq SymbolValue

type Index = Int

type NormalVector = Arr.Array Index SymbolValue

type RangesArray = Arr.Array Index SymbolRange

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


-- Contains the information about a single symbol in th input
data SymbolData = SymbolData{coefficent::SymbolValue, leadingSymbol::Bool} deriving (Show, Eq)

instance Ord SymbolData where
    SymbolData{coefficent=c1} <= SymbolData{coefficent=c2} = (abs c1) <= (abs c2) 

suplementSymbolData:: SymbolData -> SymbolData -> SymbolData
suplementSymbolData SymbolData{coefficent=coef1,leadingSymbol=leading1} SymbolData{coefficent=coef2,leadingSymbol=leading2} = SymbolData{coefficent=(coef1+coef2),leadingSymbol=(leading1 || leading2)}

-- Stores data about the input string at in intermediary stage of processing the input
data InputStringData = InputStringData{symbolData::(Map.Map Char SymbolData), constantTotal::SymbolValue} deriving (Show,Eq)

-- maps a function over the symbol coefficents of an InputStringData
mapC:: (SymbolValue -> SymbolValue) -> InputStringData -> InputStringData
mapC f InputStringData{symbolData=symbols, constantTotal=c} = InputStringData{symbolData=(Map.map applyToSymbolData symbols), constantTotal=c}
    where applyToSymbolData SymbolData{coefficent=coef, leadingSymbol=l} = SymbolData{coefficent=(f coef), leadingSymbol=l}

-- maps a function over the symbol coefficents and constantTotal of an InputStringData
mapCO:: (SymbolValue -> SymbolValue) -> InputStringData -> InputStringData
mapCO f InputStringData{symbolData=symbols, constantTotal=c} = InputStringData{symbolData=(Map.map applyToSymbolData symbols), constantTotal=(f c)}
    where applyToSymbolData SymbolData{coefficent=coef, leadingSymbol=l} = SymbolData{coefficent=(f coef), leadingSymbol=l}

-- Combines the data from 2 'independant' parts of the input, for example 2 different words,
combineWordData:: InputStringData -> InputStringData -> InputStringData
combineWordData InputStringData{symbolData=symbols1, constantTotal=offset1} InputStringData{symbolData=symbols2, constantTotal=offset2} = InputStringData{symbolData=newSymbolData, constantTotal=(offset1+offset2)}
    where newSymbolData = Map.unionWith suplementSymbolData symbols1 symbols2

-- Converts an InputStringData into a WordSumProblem and a list of the letters
-- Both the coefficents in the WordSumProblem and the characters in the list of
-- Symbols are given in order of increasing magnitude of the cooefcient, 
-- in particular they are in the same order in both cases
convertInputData:: InputStringData -> (WordSumProblem, [Char])
convertInputData InputStringData{symbolData=symbols, constantTotal=const} = (constructWordSumProblem normalVector const symbolBounds, symbolList)
    where (symbolList, dataList) = splitPairsList $ (sortBy (\(c,d) (b,e) -> e `compare` d)) $ Map.toList symbols
          (coefList, leadingSymbolList) = splitPairsList $ Prelude.map (\SymbolData{coefficent=c,leadingSymbol=l}->(c,l)) dataList
          arrayBounds = (0, (length symbolList) - 1)
          normalVector = Arr.listArray arrayBounds coefList
          symbolRange True = Seq.fromList [1..9]
          symbolRange False = Seq.fromList[0..9]
          symbolBounds = Arr.listArray arrayBounds $ Prelude.map symbolRange leadingSymbolList

-- Given an index and an array returns a new array with the same lower bound and the upper bound reduced by one
-- The elements of the new array are the same as the old one but the value with the given index is missing
skipValue:: (Arr.Ix a, Num a) => a -> Arr.Array a b -> Arr.Array a b
skipValue skipIndex array = Arr.ixmap (lowerBnd, upperBnd-1) indexSkipper array
    where (lowerBnd, upperBnd) = Arr.bounds array
          indexSkipper i | i < skipIndex = i
                         | i >= skipIndex = i + 1

-- deletes the first occurence of a given value from a sequence starting form the left.
-- If the value does not occure the original sequence is returned
deleteValueL:: (Eq a) => a -> Seq.Seq a -> Seq.Seq a
deleteValueL val sequ
    | isNothing firstOccurence = sequ
    | otherwise = Seq.deleteAt firstOccurence sequ
    where firstOccurence = elemIndexL sequ
--performs integer division, trucating towards positive infinity
divUp ::(Integral a) => a -> a -> a
divUp n d
    |remainder == 0 = quotent
    |otherwise = quotent + 1
    where (quotent, remainder) = n `divMod` d

startsWith:: (Eq a) => a -> [a] -> Bool
startsWith _ [] = False
startsWith y (x:xs) = y == x

-- splits a list of pairs into a pair of lists 
splitPairsList:: [(a,b)] -> ([a],[b])
splitPairsList [] = ([],[])
splitPairsList ((x,y):rem) = (x:xs,y:ys)
    where (xs,ys) = splitPairsList rem

