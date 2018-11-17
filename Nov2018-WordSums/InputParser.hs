-- Processes user input for word sum solver to get it into a form that 
-- the solver can solve

module InputParser
( parser
) where

import Control.Monad (foldM)
import Data.Either (either)
import qualified Data.Map as Map
import qualified Data.Char as Char

import Utils (SymbolValue)

type ErrorMessage = String

-- Contains the information about a single symbol in th input
data SymbolData = SymbolData{coefficent::SymbolValue, leadingSymbol::Bool} deriving (Show, Eq)

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

-- Changes the sign on coefficents and constant in InputStringData object
negateWordData:: InputStringData -> InputStringData
negateWordData = mapCO negate

-- Scales the data in an InputStringData so that it referes to refers to a more significant digit in a decimal number,
-- so 1 -> 10, 43 -> 430, etc. 
shiftDigitsLeft:: InputStringData -> InputStringData
shiftDigitsLeft = mapCO (*numberBase)
    where numberBase = 10

negateIf:: Bool -> InputStringData -> InputStringData
negateIf True = negateWordData
negateIf False = id

-- Processes the data for a single character in a word and adds it to the data for earlier characters in the word. 
-- Shifts the data for previous characters to account for the fact that the represent more significant digits.
parseSymbol:: Bool -> InputStringData -> Char -> Either ErrorMessage InputStringData
parseSymbol isLeading previousSymbols c
    | Char.isDigit c = Right InputStringData{symbolData=symbols', constantTotal=(offset+(Char.digitToInt c))}
    | Char.isLetter c = Right InputStringData{symbolData=(Map.insertWith suplementSymbolData c SymbolData{coefficent=1,leadingSymbol=isLeading} symbols'), constantTotal=offset}
    | otherwise = Left ("Invalid character " ++ [c])
    where InputStringData{symbolData=symbols', constantTotal=offset} = shiftDigitsLeft previousSymbols

-- Processes a string containing a single word and returns the contribution of that
-- word to the sum as an InputStringData object. If the string contains invalid 
-- characters returns an error message. 
parseWord:: String -> Either ErrorMessage InputStringData
parseWord [] = Right InputStringData{symbolData=Map.empty, constantTotal=0}
parseWord (leadingC:remainingCs) = either Left parseRemainingCs leadingCharacterData
    where leadingCharacterData = parseSymbol True InputStringData{symbolData=Map.empty, constantTotal=0} leadingC 
          parseNonLeadingCharacter = parseSymbol False
          parseRemainingCs validLeadingCData = foldM parseNonLeadingCharacter validLeadingCData remainingCs

parser = "TODO"
