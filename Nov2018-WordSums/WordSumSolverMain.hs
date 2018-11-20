
import Data.Char (toLower)
import Data.List (partition)
import Utils (SymbolValue, ErrorMessage, WordSumProblem, allUnique)
import Solver (solve)
import InputParser (parser)

formatSolution:: [Char] -> [SymbolValue] -> String
formatSolution symbolKey solution = (concat symbolResults) ++ "\n"
    where symbolResults = map (\(s,v)->s:" = " ++ (show v) ++ " " )(zip symbolKey solution)

formatResults:: [Char] -> [[SymbolValue]] -> String
formatResults symbolKey = concat . (map (formatSolution symbolKey)) 

printResult:: [Char] -> [[SymbolValue]] -> IO ()
printResult _ [] = putStrLn "None"
printResult symbolKey solutions = putStrLn $ formatResults symbolKey solutions 

invalidInput:: ErrorMessage -> IO ()
invalidInput message = do putStrLn ("Invalid input: " ++ message)
                          main

solveAndPrint:: (WordSumProblem,[Char]) -> IO ()
solveAndPrint (problem, symbolKey) = do putStrLn "Solutions with all values unique"
                                        printResult symbolKey uniqueVals 
                                        putStrLn "Solutions which include duplicated values"
                                        printResult symbolKey dupVals
    where (uniqueVals, dupVals) = partition allUnique $ solve problem

repeatProgram:: Char -> IO ()
repeatProgram 'y' = do getLine -- Clears input line befor restarting
                       main
repeatProgram 'n' = return ()
repeatProgram _ = do putStrLn "Invalid input: please enter y or n"
                     getLine
                     repeatProgram . toLower =<< getChar

main = do putStrLn "Enter a word sum problem:"
          ((either invalidInput solveAndPrint).parser) =<< getLine
          putStrLn "Would you like to enter another word sum problem? [y/n]"
          repeatProgram . toLower =<< getChar
          
          

