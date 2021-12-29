

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


cdr :: [SchExpr] -> Ctx -> SchVal
cdr (x:xs) ctx = case (eval x ctx)  of
                 (DottedPair a b) -> b
                 (SchQList q) -> (SchQList $ ap_brace $ tail q)


car :: [SchExpr] -> Ctx -> SchVal
car (x:xs) ctx = case (eval x ctx)  of
                 (DottedPair a b) -> a
                 (SchQList q)  -> (SchQList [drop 1 $ head q])

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


ap_brace :: [String] -> [String]
ap_brace [] = []
ap_brace (x:xs) = ('(':x):xs

trans :: [SchVal] -> [SchExpr] -> Ctx -> [SchVal]
trans base (a: []) ctx = base ++ [(eval a ctx)]
trans base (a:as) ctx = trans (base ++ [(eval a ctx)]) as ctx


eval :: SchExpr -> Ctx -> SchVal
eval (Empty ())        ctx      = SchEmpty ()
eval (Err i)           ctx      = Error i
eval (Number i)        ctx      = (SchNumber i)
eval (Str s)           ctx      = (SchString s)
eval (Float i)         ctx      = SchFloat i
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
eval (ArgLi al)        ctx      = (List $ trans [] al ctx)
eval (Li l)            ctx      = eval (Apply (head l)  (tail l)) ctx
-- eval (QList q)         ctx      =  SchQList q 
eval (Equals (a:b))    ctx      = SchBool $ (eval a ctx) == (eval (head b) ctx)
eval (If g e1 e2)      ctx      = case eval g ctx of 
                                  (SchBool True) -> eval e1 ctx
                                  (SchBool False) -> eval e2 ctx
eval (Var i)           ctx      = find ctx i
eval (Let d e)         ctx      = eval e (elab ctx d )
eval (Def d)           ctx      = Env (elab ctx d) 
eval (Lam ids e)       ctx      = Closure ids e ctx
eval (Quote q)         ctx      = SchQuote q
eval (QList ql)        ctx      = SchQList $ ap_brace ql 
eval (Apply f xs)      ctx      = apply f' xs'
                                   where f'   = eval f ctx
                                         xs'  = map (flip eval ctx) xs                    