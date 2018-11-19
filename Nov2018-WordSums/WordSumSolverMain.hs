
import Data.Char (toLower)
import Utils (SymbolValue, ErrorMessage, WordSumProblem)
import Solver (solve)
import InputParser (parser)

formatSolution:: [Char] -> [SymbolValue] -> String
formatSolution symbolKey solution = (concat symbolResults) ++ "\n"
    where symbolResults = map (\(s,v)->s:" = " ++ (show v) ++ " " )(zip symbolKey solution)

formatResults:: [Char] -> [[SymbolValue]] -> String
formatResults symbolKey = concat . (map (formatSolution symbolKey)) 

printResult:: [Char] -> [[SymbolValue]] -> IO ()
printResult symbolKey = putStrLn .(formatResults symbolKey) 

invalidInput:: ErrorMessage -> IO ()
invalidInput message = do putStrLn ("Invalid input: " ++ message)
                          main

solveAndPrint:: (WordSumProblem,[Char]) -> IO ()
solveAndPrint (problem, symbolKey) = (printResult symbolKey) (solve problem)

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
          
          

