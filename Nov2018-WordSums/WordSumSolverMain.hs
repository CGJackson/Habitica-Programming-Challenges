
import Utils (SymbolValue, ErrorMessage, WordSumProblem)
import Solver (solve)
import InputParser (parser)

formatSolution:: [Char] -> [SymbolValue] -> String
formatSolution symbolKey solution = (concat symbolResults) ++ "\n"
    where symbolResults = map (\(s,v)->s:" = " ++ (show v) ++ "\n" )(zip symbolKey solution)

formatResults:: [Char] -> [[SymbolValue]] -> String
formatResults symbolKey = concat . (map (formatSolution symbolKey)) 

printResult:: [Char] -> [[SymbolValue]] -> IO ()
printResult symbolKey = putStrLn .(formatResults symbolKey) 

invalidInput:: ErrorMessage -> IO ()
invalidInput message = do putStrLn ("Invalid input: " ++ message)
                          main

solveAndPrint:: (WordSumProblem,[Char]) -> IO ()
solveAndPrint (problem, symbolKey) = (printResult symbolKey).solve problem

main = do putStrLn "Enter a word sum problem:"
          input <- (getLine >>= parser)
          either invalidInput solveAndPrint input
          
          

