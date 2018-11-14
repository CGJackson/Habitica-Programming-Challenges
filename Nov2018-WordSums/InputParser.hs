-- Processes user input for word sum solver to get it into a form that 
-- the solver can solve

module InputParser
( parser
) where

import qualified Data.Map as Map
import qualified Data.Char as Char
import Data.Boolean (xor)

import Utils (SymbolValue)

type EitherErrorMessage = Either String

parseWordSumString::String -> (Map.Map Char (SymbolValue,Bool), SymbolValue)
parseWordSumString = parseWordSumString' False True

parseWordSumString'::Bool -> Bool -> String -> EitherErrorMessage (Map.Map Char (SymbolValue, Bool), SymbolValue)
parseWordSumString' False _ [] = Left "Word sum does not inculde an equals sign"
parseWordSumString' True _ ('=':str) = Left "Word sum has more than 1 equals sign"
parseWordSumString' rightOfEquals positiveWord c:str
    | Char.isLetter c = 
    | Char.isDigit c = let (symbolData, currentOffset) = parseWordSumString' rightOfEquals positiveWord str in Right (symbolData, currentOffset + characterSign*(Char.digitToInt c))
    | otherwise Left "Word sum contains invalid character " ++ c
        where characterSign = if rightOfEquals `xor` positiveWord then -1 else 1
