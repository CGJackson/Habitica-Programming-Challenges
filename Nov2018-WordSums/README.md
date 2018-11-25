Finds solutions to "word sums". A word sum is an expression such as 
~~~~
SEND+MORE=MONEY
~~~~
In which each letter represents one digit. The problem is to find the assign a digit to each symbol in such a way that the equation is correct.

This program solves these word sum problems, finding separatly all solutions where each letter represents a unique digit and where duplicate digits are allowed. Symbols take the values of digits 0-9 and it is assumed that the numbers represented have no leading 0s, i.e. in the example above 'S' and 'M' cannot be 0 as they are the leading digits of numbers in the sum but any other symbol could represent a 0.

When run the user is asked to enter a word sum problem. The valid symbols for the sum are letters A-Z (case insensitive), to represent values to solve for, the numbers 0-9, representing fixed digits in the expression and the operators '+' and '-' together with the euals sign to denote the usual mathematical operations. The expression must contain exactly one '=' sign. 

The program works by alternately using the sum to constrain the possible values of each symbol and recursively substituting the possible values of a symbol. The constraining step substritutes either the maximum or minimum allowed value for each symbol in the sum except one and comparing to the largest and smallest possible value of the remaining symbol. For example in the sum
~~~~
X+X=Y 
~~~~
X < 5 because if ti was larger this would require Y = 10, which is too large. The substitution step takes one of the unknown symbols, and, for each possible value of that symbol, recusively runs the solver with chosen symbol replaced with a fixed constant. At the end solutions are separated into those where every symbol takes a unique value and those where the some symbols take duplicate values. 
