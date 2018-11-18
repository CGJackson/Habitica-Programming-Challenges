
import Utils (SymbolValue)
import Solver (solve)
import InputParser (parser)

formatSolution:: [Char] -> [SymbolValue] -> String
formatSolution symbolKey solution = (concat symbolResults) ++ "\n"
    where symbolResults = map (\(s,v)->s:" = " ++ (show v) ++ "\n" )(zip symbolKey solution)

formatResults:: [Char] -> [[SymbolValue]] -> String
formatResults symbolKey = concat . (map (formatSolution symbolKey)) 

printResult:: [Char] -> [[SymbolValue]] -> IO ()
printResult symbolKey = putStrLn .(formatResults symbolKey) 

main = do putStrLn "Enter a word sum problem:"
          

