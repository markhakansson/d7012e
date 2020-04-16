-- Code to Haskell lab assignment 2 in the course D7012E by Håkan Jonsson

import Data.Char

data EXPR = Const Int
     | Var String
     | Op String EXPR EXPR
     | App String EXPR deriving (Eq, Ord, Show)

parse :: String -> EXPR
parse = fst . buildexpr
  where
    notfirst p (_,[]) = True
    notfirst p (_,x:xs) = not (p x)
    
    buildnumber :: String -> (EXPR,String)
    buildnumber xs = until (notfirst isDigit) accdigits (Const 0, xs)
      where
        accdigits :: (EXPR,String) -> (EXPR,String)
        accdigits (Const n, y:ys) = (Const(10*n+(ord y - 48)), ys)
    
    buildvar :: String -> (EXPR,String)
    buildvar xs = until (notfirst isLetter) accletters (Var "", xs)
      where
        accletters :: (EXPR,String) -> (EXPR,String)
        accletters (Var s, y:ys) = (Var (s ++[y]), ys)
    
    
    buildexpr :: String -> (EXPR,String)
    buildexpr xs = until (notfirst (\c -> c=='-' || c=='+')) accterms (buildterm xs)
      where
        accterms :: (EXPR,String) -> (EXPR,String)
        accterms (term, y:ys) = (Op (y:[]) term term1, zs)
          where
            (term1,zs) = buildterm ys
    
    buildterm :: String -> (EXPR,String)
    buildterm xs = until (notfirst (\c -> c=='*' || c=='/')) accfactors (buildfactor xs)
      where
        accfactors :: (EXPR,String) -> (EXPR,String)  
        accfactors (fact, y:ys) = (Op (y:[]) fact fact1, zs)
          where
            (fact1,zs) = buildfactor ys
    
    buildfactor :: String -> (EXPR,String)
    buildfactor [] = error "missing factor"
    buildfactor ('(':xs) =  case buildexpr xs of (e, ')':ws) -> (e, ws); _ -> error "missing factor"
    buildfactor (x:xs)
      | isDigit x = buildnumber (x:xs)
      | isLetter x = case buildvar (x:xs) of
                       (Var s, '(':zs) -> let (e,ws)=buildfactor ('(':zs) in (App s e,ws)
                       p -> p
      | isSignedDigit (x:xs) = buildnumber (x:xs)
      | otherwise = error "illegal symbol"

isSignedDigit :: String -> Bool
isSignedDigit (x:xs)
    | x == '-' && isDigit (head xs) = True
    | isDigit (head xs) = False
    | otherwise = error "illegal symbol" 

unparse :: EXPR -> String
unparse (Const n) = show n
unparse (Var s) = s
unparse (Op oper e1 e2) = "(" ++ unparse e1 ++ oper ++ unparse e2 ++ ")"
unparse (App func expr) = func ++ "(" ++ unparse expr ++ ")"

eval :: EXPR -> [(String,Float)] -> Float
eval (Const n) _ = fromIntegral n
eval (Var x) env = case lookup x env of Just y -> y ; _ -> error (x ++ " undefined")
eval (Op "+" left right) env = eval left env + eval right env
eval (Op "-" left right) env = eval left env - eval right env
eval (Op "*" left right) env = eval left env * eval right env
eval (Op "/" left right) env = eval left env / eval right env
eval (App "sin" expr) env = sin (eval expr env) 
eval (App "cos" expr) env = cos (eval expr env)
eval (App "log" expr) env = log (eval expr env)
eval (App "exp" expr) env = exp (eval expr env)

diff :: EXPR -> EXPR -> EXPR
diff _ (Const _) = Const 0
diff (Var id) (Var id2)
  | id == id2 = Const 1
  | otherwise = Const 0
diff v (Op "+" e1 e2) = Op "+" (diff v e1) (diff v e2)
diff v (Op "-" e1 e2) = Op "-" (diff v e1) (diff v e2)
diff v (Op "*" e1 e2) = Op "+" (Op "*" (diff v e1) e2) (Op "*" e1 (diff v e2))
diff v (Op "/" e1 e2) = Op "/" (Op "-" (Op "*" (diff v e1) e1) (Op "*" e1 (diff v e2))) (Op "*" e2 e2)
diff v (App "sin" expr) = Op "*" (diff v expr) (App "cos" expr)
diff v (App "cos" expr) = Op "*" (parse "-1") (Op "*" (diff v expr) (App "sin" expr))
diff v (App "log" expr) = Op "/" (diff v expr) expr
diff v (App "exp" expr) = Op "*" (diff v expr) (App "exp" expr)
diff _ _ = error "can not compute the derivative" 

simplify :: EXPR -> EXPR
simplify (Const n) = Const n
simplify (Var id) = Var id
simplify (Op oper left right) =
  let (lefts,rights) = (simplify left, simplify right) in
    case (oper, lefts, rights) of
      ("+", e, Const 0) -> e
      ("+", Const 0, e) -> e
      ("*", e, Const 0) -> Const 0
      ("*", Const 0, e) -> Const 0
      ("*", e, Const 1) -> e
      ("*", Const 1, e) -> e
      ("-", e, Const 0) -> e
      ("/", e, Const 1) -> e
      ("-", le, re)     -> if left==right then Const 0 else Op "-" le re
      (op, le, re)      -> Op op le re
simplify (App func expr) = (App func expr)

mkfun :: (EXPR, EXPR) -> (Float -> Float)
mkfun (e1, e2) = \x -> eval e1 [(unparse e2, x)]

findzero ::  String -> String -> Float -> Float
findzero var func val = 
    let f = mkfun (parse func, parse var)
        f' = mkfun ((diff (parse var) (parse func), parse var))
    in newtonRaphson f f' val 

newtonRaphson :: (Float -> Float) -> (Float -> Float) -> Float -> Float
newtonRaphson f f' x 
    | x_i - x_i_1 < 0.0001 = x_i_1   
    | otherwise = newtonRaphson f f' x_i_1 
  where
      x_i = newtonRaphsonNext f f' x
      x_i_1 = newtonRaphsonNext f f' x_i

newtonRaphsonNext :: (Float -> Float) -> (Float -> Float) -> Float -> Float
newtonRaphsonNext f f' x = x - (f x)/(f' x)

