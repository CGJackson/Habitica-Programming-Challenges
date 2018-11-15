-- Processes user input for word sum solver to get it into a form that 
-- the solver can solve

module InputParser
( parser
) where

import qualified Data.Map as Map
import qualified Data.Char as Char
import Data.Boolean (xor)

import Utils (SymbolValue)

type ErrorMessage = String

-- Contains the information about a single symbol in th input
data SymbolData = SymbolData{coefficent=SymbolValue, leadingSymbol=Bool} deriving Show, Eq

(+):: SymbolData -> SymbolData -> SymbolData
(+) SymbolData{coefficent=coef1,leadingSymbol=leading1} SymbolData{coefficent=coef2,leadingSymbol=leading2} = SymbolData{coefficent=(coef1+coef2),leadingSymbol=(leading1 `or` leading2)

negate:: SymbolData -> SymbolData
negate SymbolData{coefficent=coef,constantTotal=tot} = SymbolData{coefficent=(-coef), constantTotal=tot}

numberBase:: SymbolValue
numberBase = 10

digitShift:: SymbolData -SymbolData
digitShift SymbolData{coefficent=coef, leadingSymbol=l} = SymbolData{coefficent=(numberBase*coef), leadingSymbol=l}

-- Stores data about the input string at in intermediary stage of processing the input
data InputStringData = InputStringData{symbolData=(Map.Map Char SymbolData), constantTotal=SymbolValue} deriving Eq

-- maps a function over the symbol coefficents of an InputStringData
mapC:: (SymbolValue -> SymbolValue) -> InputStringData -> InputStringData
mapC

-- maps a function over the symbol coefficents and constantTotal of an InputStringData
mapO:: (SymbolValue -> SymbolValue) -> InputStringData -> InputStringData

-- Combines the data from 2 'independant' parts of the input, for example 2 different words,
(+):: InputStringData -> InputStringData -> InputStringData
(+) InputStringData{symbolData=symbols1, constantTotal=offset1} InputStringData{symbolData=symbols2, constantTotal=offset2} = InputStringData{symbolData=newSymbolData, constantTotal=(offset1+offset2)}
    where newSymbolData = Map.unionWith (+) symbols1 symbols2

-- Changes the sign on coefficents and constant in InputStringData object
negate:: InputStringData -> InputStringData
negateInputStringData{symbolData=symbols, constantTotal=offset} = InputStringData{symbolData=newSymbols, constantTotal=(-offset)}
    where newSymbols = map negate symbols

negateIf:: Bool -> InputStringData
negateIf True = negate
negateIf False = id

parseNonLeadingSymbol:: Char -> InputStringData ->  Either ErrorMessage InputStringData
parseNonLeadingSymbol c InputStringData{symbols=symbols', constantTotal=offset}
    | Char.isdigit c = InputStringData{symbols=symbols', constantTotal=(offset+(Char.digitToInt c))}
    | Char.isLetter c = InputStringData{symbols=(Map.insertWith (+) 

parseWord:: String -> Either ErrorMessage InputStringData

