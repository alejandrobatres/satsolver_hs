-- following the turorial 'https://www.gibiansky.com/blog/verification/writing-a-sat-solver/'
-- to write a SAT solver in haskell
-- including my notes

import Control.Applicative ((<|>))
import Data.Maybe (mapMaybe)
import Data.List ((\\))
import Data.Set (Set)
import qualified Data.Set as Set
-- defining the data type to store constraints
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



-- return the first free variable in the boolean expression.
-- if there are no free variables (it is Const), return Nothing

freeVar :: Expr -> Maybe Char
freeVar (Const _) = Nothing
freeVar (Var v) = Just v
freeVar (Not e) = freeVar e
freeVar (Or x y) = freeVar x <|> freeVar y
freeVar (And x y) = freeVar x <|> freeVar y

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

-- now we are going to simplify functions to return cases for each argument

simplify :: Expr -> Expr
simplify (Const b) = Const b
simplify (Var v) = Var v
simplify (Not e) =
  case simplify e of
    Const b -> Const (not b)
    e -> Not e
simplify (Or x y) =
  -- get rid of False values, which are not relevant
  let es = filter (/= Const False) [simplify x, simplify y]
  in
     -- if True in tree, expression is True
     if Const True `elem` es
     then Const True
     else
      case es of  
        -- if all values are False, 'or' is False
        [] -> Const False
        [e] -> e
        [e1, e2] -> Or e1 e2

-- dual?? to the simplify (Or x y) definition
simplify (And x y) =
  let es = filter (/= Const True) [simplify x, simplify y]
  in
    if Const False `elem` es
    then Const False
    else
      case es of
        [] -> Const True
        [e] -> e
        [e1, e2] -> And e1 e2

-- implementation of BFS (recursive)
-- extract the boolean from the Const constructor
unConst :: Expr -> Bool
unConst (Const b) = b
unConst _ = error "Not Const"

satisfiable :: Expr -> Bool
satisfiable expr =
  case freeVar expr of
    Nothing -> unConst expr
    Just v ->
      -- variable exists to guess
      -- construct 2 guesses (True or False)
      -- return whether either works
      let trueGuess = simplify (guessVar v True expr)
          falseGuess = simplify (guessVar v False expr)
      in satisfiable trueGuess || satisfiable falseGuess

-- using the SAT to solve store & time problem: 

data Store = Walmart | HomeDepot | Costco deriving (Show, Eq)

data Time = Morning | Evening deriving (Show, Eq)

availability :: Store -> [Time]
availability Walmart = [Morning]
availability HomeDepot = [Evening]
availability Costco = [Morning, Evening]

variable :: Store -> Time -> Expr
variable Walmart    Morning = Var 'a'
variable Walmart    Evening = Var 'b'
variable HomeDepot  Morning = Var 'c'
variable HomeDepot  Evening = Var 'd'
variable Costco     Morning = Var 'e'
variable Costco     Evening = Var 'f'

-- create the contstraint 
visitConstraint :: [Time] -> Store -> Expr
visitConstraint times store =
  foldl1 Or $ map (variable store) times

-- constraint requiring 
visitAllConstraint :: [Store] -> [Time] -> Expr
visitAllConstraint stores times =
  foldl1 And $ map (visitConstraint times) stores

-- apply a function and keep all Just results
storeConstraint :: [Time] -> Store -> Expr
storeConstraint allTimes store =
  case mapMaybe timeConstraint allTimes of
    [] -> Const True -- only go to one store at a time
    cs -> foldl1 And cs
  where
    timeConstraint :: Time -> Maybe Expr
    timeConstraint time
      | time `elem` availability store = Nothing
      | otherwise = Just (Not (variable store time))

-- constraints for each time (can't be in multiple places at once)
timeConstraint :: [Store] -> Time -> Expr
timeConstraint allStores time =
  foldl1 Or $ map chooseStore allStores
  where
    chooseStore store =
      And (variable store time)
          (foldl1 And (map (Not . flip variable time) (allStores \\ [store])))

-- now create the full set of constraints
stores = [Walmart, HomeDepot, Costco]
times = [Morning, Evening]

constraints :: Expr
constraints =
  foldl1 And $
    [visitAllConstraint stores times] ++
    map (storeConstraint times) stores ++
    map (timeConstraint stores) times


-- optimizing the program
-- removing double negations
fixNegations :: Expr -> Expr
fixNegations epxr = 
  case expr of
    -- removing double negatives
    Not (Not x) -> fixNegations x

    -- de morgan's laws 
    Not (And x y) -> Or (fixNegations $ Not x) (fixNegations $ Not y)
    Not (Or x y) -> And (fixNegations $ Not x) (fixNegations $ Not y)

    -- dealing with constants
    Not (Cost b) -> Const (not b)
  
    -- recurse on the subterms
    Not x -> Not (fixNegations x)
    And x y -> And (fixNegations x) (fixNegations y)
    Or x y -> Or (fixNegations x) (fixNegations y)
    x -> x

main :: IO ()
main = do
-- testing BSF with examples
  print ("Testing the backtracking on a few samples:")
  print ((satisfiable (Const True), satisfiable (Const False)))
-- expect - (True, False)
  print (satisfiable $ And (Var 'x') (Not (Var 'x')))
-- expect - False
  print (satisfiable $ Or (Var 'x') (Not (Var 'x')))
-- expect True

  print ("--")

-- printing constraints list
  print (constraints)

-- assessing satisfiability of the constraints
  print(satisfiable constraints)


