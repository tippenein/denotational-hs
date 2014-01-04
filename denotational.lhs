Homework 7
=======
Brady Ouren
-------

> module Denotational where

> import Data.Map (fromList, toList, insert)

State is given this way based on the specification of this homework problem. In
my opinion it should be a Map String -> Integer (I do use it this way at one
point)

> type State = [ (String, Integer) ]

`Expr` type defines the constructors for each Expression type.

> data Expr = Add Expr Expr
>           | Sub Expr Expr
>           | Mult Expr Expr
>           | Div Expr Expr
>           | Const Integer
>           | VarName String
>           | TrueE
>           | FalseE
>           | And    Expr Expr
>           | Or     Expr Expr
>           | Not    Expr
>           | Equals Expr Expr
>           | Gt     Expr Expr
>           | Lt     Expr Expr
>   deriving (Show,Eq)

`Stmt` type defines the types of control structures allow in this language.

> data Stmt = Assign String Expr
>           | IfThen Expr Stmt
>           | IfThenElse Expr Stmt Stmt
>           | While Expr Stmt
>           | Seq Stmt Stmt
>           | Skip
>   deriving (Show, Eq)


> eval :: Expr -> State -> Integer
> eval (Add e1 e2) state = eval e1 state + eval e2 state
> eval (Sub e1 e2) state = eval e1 state - eval e2 state
> eval (Mult e1 e2) state = eval e1 state * eval e2 state
> eval (Div e1 e2) state = eval e1 state `div` eval e2 state
> eval (Const x) _ = x
> eval TrueE _ = 1
> eval FalseE _ = 0

These all work with booleans, so they return 0 or 1.

> eval (And e1 e2) state = if (eval e1 state + eval e2 state) > 1 then 1 else 0
> eval (Or e1 e2) state  = if (eval e1 state + eval e2 state) > 0 then 1 else 0
> eval (Gt e1 e2) state  = if eval e1 state > eval e2 state  then 1 else 0
> eval (Lt e1 e2) state  = if eval e1 state < eval e2 state  then 1 else 0
> eval (Equals e1 e2) state = if eval e1 state == eval e2 state then 1 else 0
> eval (Not e) state = flipbit $ eval e state
> eval (VarName v) state =
>   case lookup v state of
>     Nothing -> error ("Undefined variable \"" ++ v ++ "\".")
>     Just x -> x

This is the update function encouraged by the homework writeup. I chose not to
use it because it makes other functions later on unnecessarily complicated.

> update :: String -> Integer -> State -> State
> update var x state = (var,x):state

Instead I defined 'update'` which uses State as it should be (Data.Map)

> update' :: String -> Integer -> State -> State
> update' var x state = toList $ insert var x $ fromList state

Given a statement and a state, this “executes” the statement to produce an updated state.

> exec :: Stmt -> State -> State
> exec (Skip) s = s
> exec (Seq s1 s2) s = exec s2 $ exec s1 s
> exec (Assign str e) s =
>   let e' = eval e s
>   in
>     update' str e' s

> exec (IfThen e stmt) s =
>   let e' = eval e s
>   in
>     if (e' == 1) then exec stmt s else exec Skip s
> exec (IfThenElse e stmt1 stmt2) s =
>   let e' = eval e s
>   in
>     if (e' == 1) then exec stmt1 s else exec stmt2 s
> exec (While e stmt) s =
>   let e' = eval e s
>   in
>     if (e' == 1) then exec (While e stmt) (exec stmt s)
>     else exec Skip s

Return a state consisting of _only_ the variables that appeared in initial state provided.
This is where update' pays off because we don't have a huge state list full of
redundant variable mappings, we just have the relevant key value pairs.

> run :: Stmt -> State -> State
> run stmt state = filter (\(key,_) -> key `elem` stateKeys) finalState
>   where
>     stateKeys = map fst state
>     finalState = exec stmt state

The goal of this assignment ultimately was to compute the state after executing
an AST using the grammar we defined.

For example, this pseudocode:

    i = 0
    z = 1
    while i < y
    z = z * x
    i = i + 1

would be represented by the AST:

> exponentAST =
>   (Seq
>     (Assign "i" (Const 0))
>     (Seq
>       (Assign "z" (Const 1))
>       (While (Lt (VarName "i") (VarName "y"))
>         (Seq
>           (Assign "z" (Mult (VarName "z") (VarName "x")))
>           (Assign "i" (Add (VarName "i") (Const 1)))))))

Also,

    low = 1 ;
    up = n ;
    acc = 1 ;
    guess = ((low + up) / 2) ;
    while ((up - low) > acc)
      guess = ((low + up) / 2)
      if ((guess * guess) > n)
        up = guess ;
      else
        low = guess ;

> gcdAST =
>   (Seq
>     (Assign "low" (Const 1))
>     (Seq (Assign "up" (VarName "n"))
>     (Seq (Assign "acc" (Const 1))
>     (Seq (Assign "guess" (Div (Add (VarName "low") (VarName "up")) (Const 2)))
>       (While (Gt (Sub (VarName "up") (VarName "low")) (VarName "acc"))
>         (Seq (Assign "guess" (Div (Add (VarName "low") (VarName "up")) (Const 2)))
>           (IfThenElse (Gt (Mult (VarName "guess") (VarName "guess")) (VarName "n"))
>             (Assign "up" (VarName "guess"))
>             (Assign "low" (VarName "guess")))))))))

> run exponentAST [("x",4), ("y",3), ("z",0)]

Which will return the state:

    [("x",4),("y",3),("z",64)]

This is just a silly utility function used in the eval function for booleans.

> flipbit 1 = 0
> flipbit 0 = 1
> flipbit _ = error ("non-boolean")
