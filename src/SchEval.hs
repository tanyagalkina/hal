

module SchEval 
where
 
import Grammer
import Data.Fixed
import Types
import Lexer
import Debug.Trace
import Control.Applicative
import Data.Function


addition :: Double -> [SchExpr] -> Ctx -> SchVal
addition base (x:[]) ctx = let (SchFloat n1) = eval x ctx in
                           let (SchFloat n2) = eval (Float base) ctx in 
                                  SchFloat (n1 + n2)
addition base (x:xs) ctx = let (SchFloat n1) = eval x ctx in
                           let (SchFloat n2) = eval (Float base) ctx in 
                                  addition (n1 + n2) xs ctx

modulo :: [SchExpr] -> Ctx -> SchVal
modulo e@(a:b) ctx = let (SchFloat n1) = eval a ctx in
                     let (SchFloat n2) = eval (head b) ctx in
                          SchFloat(mod' n1 n2)


less :: [SchExpr] -> Ctx -> SchVal
less e@(a:b) ctx = let (SchFloat n1) = eval a ctx in
                   let (SchFloat n2) = eval (head b) ctx in
                          SchBool (n1 < n2)
-- HERE YOU SEE THE PROBLEM WITH TYPES
divide :: [SchExpr] -> Ctx -> SchVal
-- divide e _ = traceShow (length e) SchFloat 8
divide e@(a:b) ctx | ((length e) /= 2) = Error "The function takes exaclty 2 args!" 
                   | otherwise = let (SchFloat n1) = eval a ctx in
                           let (SchFloat n2) = eval (head b) ctx in
                                  --SchFloat (n1 / n2)
                           if (n2 /= 0) then SchFloat $ fromIntegral $ round (n1 / n2) else Error "Division by zero" 

multiply :: Double -> [SchExpr] -> Ctx -> SchVal
multiply base (x:[]) ctx = let (SchFloat n1) = eval x ctx in
                           let (SchFloat n2) = eval (Float base) ctx in 
                                  SchFloat (n1 * n2)
multiply base (x:xs) ctx = let (SchFloat n1) = eval x ctx in
                           let (SchFloat n2) = eval (Float base) ctx in 
                                  multiply (n1 * n2) xs ctx



substraction :: [Double] -> [SchExpr] -> Ctx -> SchVal
substraction [] (x:[]) ctx =  let (SchFloat n ) = eval x ctx in
                              SchFloat (0 - n)
substraction base (x:[]) ctx = let (SchFloat n1) = eval x ctx in
                               let (SchFloat n2) = eval (Float $ head base) ctx in 
                                   SchFloat (n2 - n1)
substraction [] (x:xs) ctx = let (SchFloat n1) = eval x ctx in
                                  substraction [n1] xs ctx

substraction base (x:xs) ctx = let (SchFloat n1) = eval x ctx in
                               let (SchFloat n2) = eval (Float $ head base) ctx in 
                                  substraction [(n2 - n1)] xs ctx


construction :: [SchExpr] -> Ctx -> SchVal
construction (a:b) ctx = DottedList [(eval a ctx)] (eval (head b) ctx)     

direct_env :: SchVal -> Ctx ->(Int, SchVal, Ctx)
direct_env (Env i) old = (0, SchString (fst $ head i), i)
direct_env n      old  = (0, n, old)

evalSch :: String -> Ctx -> (Int, SchVal, Ctx)
evalSch input  ctx | output == [] =  (84, (SchString "something went wrong"), ctx)
                   | snd (head output) /= "" = (84, (SchString "Did Not Parse Everything"), ctx)
                   | True = (direct_env (eval (fst $ head output) ctx) ctx)
                        -- where output = parse scheme input
                          where output =  parse atom input

-- boolean :: Parser SchExpr
-- boolean = do
--           n <- string "#f" <|> string "#t"
--           if' (n == "#f") (return (Bool False)) (return (Bool True))
--           <|> return (Err "Hello") 


atom :: Parser SchExpr
atom = do
                n <- tokenFlt
                return n
                <|> quotedString
                <|> eq
                <|> atm
                <|> token
                <|> symb
                <|> boolean
                <|> quote
                <|> list
                

listMan :: [SchExpr] -> SchExpr
listMan (Var "+": xs) = (Plus xs)
listMan (Var "-":xs) = (Minus xs)
listMan (Var "*": xs) = (Mult xs)
-- listMan (Var "eq?":xs) = (Equals xs)
listMan list = (Li list)
-- listMan (Var "+": xs) = (Plus xs)
-- listMan (Var "+": xs) = (Plus xs)
-- listMan (Var "+": xs) = (Plus xs)
-- listMan (Var "+": xs) = (Plus xs)
-- listMan (Var "+": xs) = (Plus xs)
-- listMan (Var "+": xs) = (Plus xs)
-- listMan (Var "+": xs) = (Plus xs)



list :: Parser SchExpr
list = do
       _ <- string "("
       _ <- spaces
       ex <- many atom
       _ <- string ")"
       return (listMan ex)
              

-- exam2 :: Parser SchExpr
-- exam2 = do
--        _ <- tokenFlt
--        return (Apply (Var "sum") [Float 10])
--        --return (Var "newKuku")

-- exam :: Parser SchExpr
-- exam = do
--        _ <- string "mama"
--        --return (Let (SchVal "x" $ (Float 10)) (Var "x"))
--        -- return (Var "name")
--        --return (Def (SchVal "newKuku" (Float 78)))
--        return (Def (Rec "sum" (Lam ["n"] (If (Equals (Var "n") (Float 0)) (Float 0) (Plus [(Var "n") ,(Apply (Var "sum") [Minus [(Var "n") ,(Float 1)]])])))))
--        --return (Def (Rec "add" (Lam ["x", "y"] (Plus [(Var "x"), (Var "y")]))))
--        --return (Let (Rec "add" (Lam ["x", "y"] (Plus [(Var "x"), (Var "y")]))) (Apply (Var "add") [Float 1, Float 3]))
--        <|> exam2

find :: Ctx -> Ident -> SchVal 
find env i | found == [] = (Error $ "Exception: variable " ++ i ++ " is not bound")
           | otherwise = snd $ head found
            where found = filter (\(i', _) -> i == i') env
         

apply :: SchVal -> [SchVal] -> SchVal
apply (Closure ids e ctx) vals | (length ids /= length vals) = Error "Exception: incorrect argument count in call" 
                               | otherwise = eval e (zip ids vals ++ ctx)
apply _     _                   = Error "no such function" 


elab :: Ctx -> Defn -> Ctx
elab ctx (SchVal i e) = (i, eval e ctx ):ctx
elab ctx (Rec i (Lam args e)) =  ctx'
                             where ctx' = (i, Closure args e ctx'):ctx


-- THIS IS LEFT FOR FUNCTIONS# LENGTH
-- eval1 :: SchExpr -> Ctx -> SchVal

-- eval1 (Apply f xs) ctx        = apply f' xs'
--                                where f'   = eval f ctx
--                                      xs'  = map (flip eval ctx) xs                    

trans :: [SchVal] -> [SchExpr] -> Ctx -> [SchVal]
trans base (a: []) ctx = base ++ [(eval a ctx)]
trans base (a:as) ctx = trans (base ++ [(eval a ctx)]) as ctx



eval :: SchExpr -> Ctx -> SchVal
eval (Err i)           ctx      =  Error i
eval (Number i)        ctx      = (SchNumber i)
eval (Str s)           ctx      = (SchString s)
eval (Float i)         ctx      = SchFloat i
eval (Plus e)          ctx      =  addition 0.0 e ctx
eval (Minus e)         ctx      =  substraction [] e ctx 
eval (Mult e)          ctx      =  multiply 1 e ctx
eval (Div e)           ctx      =  divide e ctx
eval (Mod e)           ctx      =  modulo e ctx
eval (Less e)          ctx      =  less e ctx  
--  length e /= 2 = Error "Exception: incorrect argument count in call"
eval (Bool b)          ctx      =  SchBool b
eval (Cons e)          ctx      =  construction e ctx
eval (ArgLi al)        ctx      =  (List $ trans [] al ctx)
eval (Li l)            ctx      = eval (Apply (head l)  (tail l)) ctx
-- eval (QList q)         ctx      =  SchQList q 
eval (Equals (a:b))    ctx      =  SchBool $ (eval a ctx) == (eval (head b) ctx)
eval (If g e1 e2)      ctx      = case eval g ctx of 
                                   (SchBool True) -> eval e1 ctx
                                   (SchBool False) -> eval e2 ctx
eval (Var i)           ctx      = find ctx i
eval (Let d e)         ctx      = eval e (elab ctx d )
eval (Def d)           ctx      = Env (elab ctx d) 
eval (Lam ids e)       ctx      = Closure ids e ctx
eval (Quote q)         ctx      = SchQuote q
eval (Apply f xs)      ctx      = apply f' xs'
                               where f'   = eval f ctx
                                     xs'  = map (flip eval ctx) xs                    
