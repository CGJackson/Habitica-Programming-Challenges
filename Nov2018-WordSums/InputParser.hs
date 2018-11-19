-- Processes user input for word sum solver to get it into a form that 
-- the solver can solve

module InputParser
( parser
) where

import Control.Monad (foldM)
import Data.Either (either)
import qualified Data.Map as Map
import qualified Data.Char as Char

import Utils (SymbolValue, SymbolData(..), ErrorMessage, InputStringData(..), WordSumProblem, suplementSymbolData, combineWordData, convertInputData, mapC, mapCO, startsWith)


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
    | Char.isDigit c = Right InputStringData{symbolData=symbols', constantTotal=(offset-(Char.digitToInt c))}
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

-- Splits string into words at +, - or =
splitIntoWords:: String -> [String]
splitIntoWords [] = []
splitIntoWords ('+':str) = ['+':word]++(splitIntoWords remainder)
    where (word,remainder) = break (\c -> (c=='+')||(c=='=')||(c=='-')) str
splitIntoWords ('=':str) = ['=':word]++(splitIntoWords remainder)
    where (word,remainder) = break (\c -> (c=='+')||(c=='=')||(c=='-')) str
splitIntoWords ('-':str) = ['-':word]++(splitIntoWords remainder)
    where (word,remainder) = break (\c -> (c=='+')||(c=='=')||(c=='-')) str
splitIntoWords str = [word]++(splitIntoWords remainder)
    where (word,remainder) = break (\c -> (c=='+')||(c=='=')||(c=='-')) str
          
-- Finds the sign in the sum of a word
signWord:: String -> (String,Bool)
signWord ('+':word) = (word,False)
signWord ('-':word) = (word,True)
signWord word = (word,False)

-- Reverses the sign on all terms on the right hand side of the equation, effectively subtracting them accross to the left hand side
-- Also stips out = sign and returns error messages if there is not exactly one equals sign
bringTermsToLeft:: [(String,Bool)] -> Either ErrorMessage [(String,Bool)]
bringTermsToLeft words 
    | rightHandSideTerms == [] = Left "No equals sign in word sum"
    | containSecondEqauals = Left "Word sum contained multiple equals signs"
    | otherwise = Right $ leftHandSideTerms ++ (map (\(w,s)->(w,not s)) equalsStripedRightHandSide)
    where wordStartsWithEquals (word,sign) = startsWith '=' word
          (leftHandSideTerms,rightHandSideTerms) = break wordStartsWithEquals words
          equalsStripedRightHandSide = let (('=':word,sign):rhs) = rightHandSideTerms in (word,sign):rhs
          containSecondEqauals = any wordStartsWithEquals equalsStripedRightHandSide
          
-- removes whitespace and case dependancies from input
sanatizeInput:: String -> String
sanatizeInput = (map Char.toUpper).(filter (not.Char.isSpace))

parseWordSumExpression:: String -> Either ErrorMessage InputStringData
parseWordSumExpression str 
    | any (\w -> (w=="+")||(w=="-")) words = Left "Word sum contains repreated operators with no term in between"
    | otherwise = fmap (foldr combineWordData emptyWordData) wordData
    where words = (splitIntoWords.sanatizeInput) str 
          signedWords = bringTermsToLeft $ (map signWord) words
          wordData = signedWords >>= (sequence.(map (\(w,s)->(fmap (negateIf s) (parseWord w)))))
          emptyWordData = InputStringData{symbolData=Map.empty, constantTotal=0}
          
parser:: String -> Either ErrorMessage (WordSumProblem, [Char])
parser str = fmap convertInputData $ parseWordSumExpression str
          
