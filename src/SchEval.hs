module SchEval where
import Grammer
import Types
import Lexer
import Debug.Trace
import Control.Applicative


addition :: Float -> [SchExpr] -> Ctx -> SchVal
addition base (x:[]) ctx = let (SchFloat n1) = eval x ctx in
                           let (SchFloat n2) = eval (Float base) ctx in 
                                  SchFloat (n1 + n2)
addition base (x:xs) ctx = let (SchFloat n1) = eval x ctx in
                           let (SchFloat n2) = eval (Float base) ctx in 
                                  addition (n1 + n2) xs ctx


substraction :: [Float] -> [SchExpr] -> Ctx -> SchVal
substraction [] (x:[]) ctx = eval x ctx 
substraction base (x:[]) ctx = let (SchFloat n1) = eval x ctx in
                               let (SchFloat n2) = eval (Float $ head base) ctx in 
                                   SchFloat (n2 - n1)
substraction [] (x:xs) ctx = let (SchFloat n1) = eval x ctx in
                                  substraction [n1] xs ctx

substraction base (x:xs) ctx = let (SchFloat n1) = eval x ctx in
                               let (SchFloat n2) = eval (Float $ head base) ctx in 
                                  substraction [(n2 - n1)] xs ctx

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
                <|> token
                <|> symb
                <|> boolean
                <|> quotedString
                <|> quote
                <|> list
                

list :: Parser SchExpr
list = do
       _ <- string "("
       ex <- many atom
       _ <- string ")"
       return (Li ex)
              
-- symbol :: Parser SchExpr
-- symbol = do


exam2 :: Parser SchExpr
exam2 = do
       _ <- tokenFlt
       return (Apply (Var "sum") [Float 10])
       --return (Var "newKuku")

exam :: Parser SchExpr
exam = do
       _ <- string "mama"
       --return (Let (SchVal "x" $ (Float 10)) (Var "x"))
       -- return (Var "name")
       --return (Def (SchVal "newKuku" (Float 78)))
       return (Def (Rec "sum" (Lam ["n"] (If (Equals (Var "n") (Float 0)) (Float 0) (Plus [(Var "n") ,(Apply (Var "sum") [Minus [(Var "n") ,(Float 1)]])])))))
       --return (Def (Rec "add" (Lam ["x", "y"] (Plus [(Var "x"), (Var "y")]))))
       --return (Let (Rec "add" (Lam ["x", "y"] (Plus [(Var "x"), (Var "y")]))) (Apply (Var "add") [Float 1, Float 3]))
       <|> exam2

find :: Ctx -> Ident -> SchVal 
find env i | found == [] = (Error $ "Exception: variable " ++ i ++ " is not bound")
           | otherwise = snd $ head found
            where found = filter (\(i', _) -> i == i') env
         

apply :: SchVal -> [SchVal] -> SchVal
apply (Closure ids e ctx) vals = eval e (zip ids vals ++ ctx)
apply _     _                   = error "no such function" 


elab :: Ctx -> Defn -> Ctx
elab ctx (SchVal i e) = (i, eval e ctx ):ctx
elab ctx (Rec i (Lam args e)) =  ctx'
                             where ctx' = (i, Closure args e ctx'):ctx


-- THIS IS LEFT FOR FUNCTIONS# LENGTH
-- eval1 :: SchExpr -> Ctx -> SchVal

-- eval1 (Apply f xs) ctx        = apply f' xs'
--                                where f'   = eval f ctx
--                                      xs'  = map (flip eval ctx) xs                    


eval :: SchExpr -> Ctx -> SchVal
eval (Err i)           ctx      = traceShow ctx Error i
eval (Number i)        ctx      = (SchNumber i)
eval (Str s)           ctx      = (SchString s)
eval (Float i)         ctx      = SchFloat i
eval (Plus e)          ctx      =  addition 0.0 e ctx
eval (Bool b)          ctx      =  SchBool b
-- eval (QList q)         ctx      =  SchQList q 
eval (Equals e1 e2)    ctx      =  SchBool $ (eval e1 ctx) == (eval e2 ctx)
eval (If g e1 e2)      ctx      = case eval g ctx of 
                                   (SchBool True) -> eval e1 ctx
                                   (SchBool False) -> eval e2 ctx
eval (Minus e)         ctx      = substraction [] e ctx 
eval (Var i)           ctx      = find ctx i
eval (Let d e)         ctx      = eval e (elab ctx d )
eval (Def d)           ctx      = Env (elab ctx d) 
eval (Lam ids e)       ctx      = Closure ids e ctx
eval (Quote q)         ctx      = SchQuote q
eval (Apply f xs)      ctx      = apply f' xs'
                               where f'   = eval f ctx
                                     xs'  = map (flip eval ctx) xs                    
