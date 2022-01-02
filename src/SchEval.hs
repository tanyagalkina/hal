

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
substraction []   (x:[]) ctx = let (SchFloat n ) = eval x ctx in
                                    SchFloat (0 - n)
substraction base (x:[]) ctx = let (SchFloat n1) = eval x ctx in
                               let (SchFloat n2) = eval (Float $ head base) ctx in
                                     SchFloat (n2 - n1)
substraction []   (x:xs) ctx = let (SchFloat n1) = eval x ctx in
                                   substraction [n1] xs ctx

substraction base (x:xs) ctx = let (SchFloat n1) = eval x ctx in
                               let (SchFloat n2) = eval (Float $ head base) ctx in 
                                   substraction [(n2 - n1)] xs ctx


construction :: [SchExpr] -> Ctx -> SchVal
construction (a:b) ctx = (DottedPair (Carr (eval a ctx)) (Cdrr (eval (head b) ctx)))


consQList :: [SchExpr] -> Ctx -> SchVal
--consQList []     ctx =  
consQList (x:[]) ctx = eval (Cons [x ,(QList [])]) ctx
consQList (x:xs) ctx = DottedPair (Carr (eval x ctx)) (Cdrr (consQList xs ctx))


-- evalCond :: SchExpr -> SchVal
-- evalCond (a, 

conditional :: [SchExpr] -> Ctx -> SchVal

conditional ((Li (x:xs)):[]) ctx 
              | eval (x) ctx /= (SchBool True) = Error "Cond error"
              | otherwise                      = eval (head xs) ctx
conditional ((Li (x:xs)):xxs) ctx 
              | eval (x) ctx == (SchBool True) = eval (head xs) ctx
              | otherwise = conditional xxs ctx 

conditional ((Var x):xs) ctx | result /= (SchBool False) = result
                             | otherwise = conditional xs ctx
                              where result = eval (Var x) ctx


-- eval (Var x) ctx of
--                                (SchBool True , value) -> value
--                                (SchBool False, _)  -> conditional xs ctx 
             -- otherwise                      = eval (head x) ctx
-- conditional (((Var x):xs):xxs) ctx 
--               | eval (Var x) ctx == (SchBool True) = eval (head xs) ctx
--               | otherwise = conditional xxs ctx                    

-- conditional list ctx = traceShow list Error "NOT-EXHAUSTIVE"                       


cdr :: [SchExpr] -> Ctx -> SchVal
-- cdr (x:xs) ctx = eval x ctx
--cdr (x:xs) ctx = traceShow x SchString "found"
cdr (x:xs) ctx = case (eval x ctx)  of
                 (DottedPair (Carr a) (Cdrr b)) -> b 
       --        --    a -> SchEmpty ()
              --    ((SchEmpty ()))    -> SchEmpty()
              --    ((SchQList [])) -> SchQList []
                 

car :: [SchExpr] -> Ctx -> SchVal
car (x:xs) ctx = case (eval x ctx)  of
                 (DottedPair (Carr a) b) -> a
                -- _ -> (SchFloat 9)
                --- (SchQList q)  -> (SchQList [drop 1 $ head q])

isAtom :: [SchExpr] -> Ctx -> SchVal
isAtom e ctx = case (eval (head e) ctx) of 
               (DottedPair a b) -> SchBool False
               (SchQList []) -> SchBool True
               (SchQList l) -> SchBool False
               _ -> SchBool True
                              

find :: Ctx -> Ident -> SchVal 
find env i | found == [] = (Error $ "Exception: variable " ++ i ++ " is not bound")
           | otherwise = snd $ head found
            where found = filter (\(i', _) -> i == i') env
         

apply :: SchVal -> [SchVal] -> SchVal
apply (Closure ids e ctx) vals 
--  (length ids /= length vals) = Error "Exception: incorrect argument count in call" 
                             = eval e (zip ids vals ++ ctx)
apply (SchBool False) vals = SchBool False
apply (SchBool True) vals = head vals                             
apply cl v                  = traceShow (cl, v) Error "no such function" 


elab :: Ctx -> Defn -> Ctx
elab ctx (SchVal i e) = (i, eval e ctx ):ctx
elab ctx (Rec i (Lam args e)) =  ctx'
                             where ctx' = (i, Closure args e ctx'):ctx


takeFirst:: [String] -> [SchExpr] -> [Ident]
takeFirst base ((Li (Var x:_)):[]) = x:base
takeFirst base ((Li (Var x:_)):xx) = takeFirst (x:base) xx

takeSecond:: [SchExpr] -> [SchExpr] -> [SchExpr]
takeSecond base ((Li (Var x:xs)):[]) = (head xs):base
takeSecond base ((Li (Var x:xs)):xx) = takeSecond ((head xs):base) xx


-- elabor :: [SchExpr] -> Ctx -> Ctx
-- -- (SchVal nam val)
-- elabor ((Li (Var x:xs)):[]) ctx = elab ctx (SchVal x (head xs))
-- elabor ((Li (Var x:xs)):xx) ctx = elabor xx ctx

-- letSequence :: [SchExpr] -> SchExpr -> Ctx -> SchVal
-- letSequence ins ex ctx =  eval (Lam (takeFirst [] ins) ex) (elabor ins ctx)
-- -- takeSecond base ((Li (_:x)):[]) = traceShow x base
-- -- takeSecond base ((Li (_:x)):xx) = traceShow x takeSecond base xx

-- ap_brace :: [SchVal] -> [SchVal]
-- ap_brace []        = []
-- ap_brace list      = (Var "("):list
-- -- ap_brace (x:xs) = ('(':x):xs

el:: [SchExpr] -> [SchExpr]
el list = reverse $ (Empty()):list

trans :: [SchVal] -> [SchExpr] -> Ctx -> [SchVal]
trans base (a: []) ctx = base ++ [(eval a ctx)]
trans base (a:as) ctx = trans (base ++ [(eval a ctx)]) as ctx


eval :: SchExpr -> Ctx -> SchVal
eval (QSymb symb)           ctx = (SchQuote symb)
eval (Quote (Var q))   ctx      = (SchQuote q)
eval (Quote k)         ctx      = eval k ctx
-- eval (CondArg (b, x))  ctx      | b == SchBool False = b
--                                 | otherwise = head x
--eval (Quote (Var q))   ctx      = (SchQuote q)
--eval (Quote k)         ctx      = eval k ctx
eval (Empty ())        ctx      = SchEmpty ()
eval (Err i)           ctx      = Error i
eval (Lett ins ex)     ctx      = eval (Apply (Lam (takeFirst [] ins) ex) (takeSecond [] ins))  ctx 
       --letSequence ins ex ctx
eval (Number i)        ctx      = (SchNumber i)
eval (Str s)           ctx      = (SchString s)
eval (Float i)         ctx      = SchFloat i
--eval (Carr (Float i))         ctx      = SchFloat i
--eval (Cdrr (Float i))         ctx      = SchFloat i
eval (Plus e)          ctx      = addition 0.0 e ctx
eval (Minus e)         ctx      = substraction [] e ctx 
eval (Mult e)          ctx      = multiply 1 e ctx
eval (Div e)           ctx      = divide e ctx
eval (Mod e)           ctx      = modulo e ctx
eval (Less e)          ctx      = less e ctx
eval (Car e)           ctx      = car e ctx
eval (Cdr e)           ctx      = cdr e ctx 
-- eval (Cdr e)           ctx      = 
eval (Bool b)          ctx      = SchBool b
--eval (DPair a b)       ctx      =   
eval (Cons e)          ctx      = construction e ctx
eval (Cond e)          ctx      = conditional e ctx 
eval (ArgLi al)        ctx      = (List $ trans [] al ctx)
--eval (Li SchBool [SchVal])
-- eval (Li ((SchBool False):lx))  ctx = SchBool False
-- eval (Li ((SchBool True):lx)) ctx      = lx
eval (Li li@(l:lx))            ctx      =   
 eval (Apply (head li)  (tail li)) ctx
-- eval (QList q)         ctx      =  SchQList q 
eval (Equals (a:b))    ctx      = SchBool $ (eval a ctx) == (eval (head b) ctx)
eval (IsAtom e)        ctx      = isAtom e ctx 
eval (If g e1 e2)      ctx      = case eval g ctx of 
                                  (SchBool True) -> eval e1 ctx
                                  (SchBool False) -> eval e2 ctx
-- eval (Var i val)       ctx      =    
eval (Var i)           ctx      = find ctx i
eval (Let d e)         ctx      = eval e (elab ctx d)
eval (Def d)           ctx      = Env (elab ctx d) 
eval (Lam ids e)       ctx      = Closure ids e ctx
eval (QList [])        ctx      = SchQList []
eval (QList q)         ctx      = consQList q ctx
       -- consQList q ctx 
       -- construction q ctx 
        -- eval e (zip ids vals ++ ctx)
eval (Apply f xs)      ctx      =  apply f' xs'
                                   where f'   =  eval f ctx
                                         xs'  = map (flip eval ctx) xs                    
-- | f == (Var "cond") = eval (Cond map (flip eval ctx) xs) ctx 