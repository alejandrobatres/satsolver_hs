-- following the turorial 'https://www.gibiansky.com/blog/verification/writing-a-sat-solver/index.html'
-- to write a SAT solver in haskell


-- defining the data typ eto store constraints
data Expr = Var Char
          | And Expr Expr
          | Or Expr Expr
          | Not Expr
          | Const Bool
  deriving (Show, Eq)

-- backtracking search:
-- 1. find a variable in teh cosntraint expression that is free (has not been assigned)
-- 2. guess a value for this variable
-- 3. replace all other occurences of the free variable with the guessed value
-- 4. simplify expression by assessing if vailed or not
--    if passed: values assigned are correct, any unassigned do not matter
--    if failed: assignment failed, undo and assign opposite value (i.e. false -> true)

import Control.Applicative ((<|>))

-- return the first free variable in the boolean expression.
-- if there are no free variables (it is Const), return Nothing

freeVar :: Expr -> Maybe Char
freeVar :: (Const _) = Nothing
freeVar :: (Var v) = Just v
freeVar :: (Not e) = freeVar e
freeVar :: (Or x y) = freeVar x <|> freeVar y
freeVar :: (And x y) = freeVar x <|> freeVar y

-- replace the free variables with true or false guesses
-- introduce the 'Const' constructors, replacing 'Var' constructors with them

guessVar :: Char -> Bool -> Expr -> Expr

guessVar var val e = 
  case e of
    Var v -> if v == var
             then Const val
             else Var v
    Not e -> Not (guess e)
    Or x y -> Or (guess x) (guess y)
    And x y -> And (guess x) (guess y)
    Const b -> Const b
  where 
    guess = guessVar var val


