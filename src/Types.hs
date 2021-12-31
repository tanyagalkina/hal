module Types where


import Control.Applicative


type Ident = String

data Defn = SchVal Ident SchExpr
          | Rec Ident SchExpr
          deriving (Show, Eq) 


data SchExpr = Number Int
            | Str String
            | Float Double 
            | Lett [SchExpr] SchExpr   
            | Plus [SchExpr] 
            | Minus [SchExpr]
            | Mult [SchExpr]
            | Div [SchExpr]
            | Mod [SchExpr]
            | IsAtom [SchExpr]
            | Less [SchExpr]
            | Var Ident
            | Cons [SchExpr]
            | ArgLi [SchExpr]
            | DPair SchExpr SchExpr
            | Li [SchExpr] 
            | Empty () 
            | If SchExpr SchExpr SchExpr
            | Equals [SchExpr]
            | Let Defn SchExpr
            | Def Defn 
            | Quote SchExpr 
            | Lam [Ident] SchExpr
            | Apply SchExpr [SchExpr]
            | Bool Bool
            | Err String
            | Car [SchExpr] 
            | Cdr [SchExpr]
            | QList [SchExpr]
            | Head SchExpr
            | QSymb Ident
            deriving (Show, Eq)


data SchVal = Atom Integer
             | AtmString String
             | Schlist [SchVal]            
             | List [SchVal]
             | Carr SchVal
             | Cdrr SchVal
             | SchHead SchVal
             | SchQList [SchVal]
             | DottedList [SchVal]
             | DottedPair SchVal SchVal
             | Unbraced SchVal
             | SchNumber Int
             | SchEmpty ()
             | SchFloat Double 
             | SchString String
             | SchChar Char
             | SchBool Bool
             | Error String
             | SchQuote String
             | Env Ctx
             | Closure [Ident] SchExpr Ctx
      deriving (Show, Eq)       

type Ctx = [(Ident, SchVal)]

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y



newtype Parser a = P (String ->[(a, String)])

parse :: Parser a -> String -> [(a,String)]
--parse (P p) input = traceShow ("trace" ++ show input) p input
parse (P p) input = p input


item :: Parser Char
item = P (\input -> case input of
                     []     -> []
                     (x:xs) -> [(x,xs)])


instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = P (\input -> case parse p input of
                            []        -> []
                            [(v,out)] -> [(g v, out)])

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\input -> [(v,input)])

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = P (\input -> case parse pg input of
                             []        -> []
                             [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P (\input -> case parse p input of
                           []        -> []
                           [(v,out)] -> parse (f v) out)


instance Alternative Parser where
   -- empty :: Parser a
   empty = P (\input -> [])

   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P (\input -> case parse p input of
                           []        -> parse q input
                           [(v,out)] -> [(v,out)])