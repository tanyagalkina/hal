module Types where


import Control.Applicative


type Ident = String

data Defn = SchVal Ident SchExpr
          | Rec Ident SchExpr
          deriving (Show, Eq) 


data SchExpr = Number Int
            | Str String
            | Float Double    
            | Plus [SchExpr] 
            | Minus [SchExpr]
            | Mult [SchExpr]
            | Div [SchExpr]
            | Mod [SchExpr]
            | Less [SchExpr]
            | Var Ident
            | Cons [SchExpr]
            | ArgLi [SchExpr]
            | Li [SchExpr] 
            | If SchExpr SchExpr SchExpr
            | Equals [SchExpr]
            | Let Defn SchExpr
            | Def Defn 
            | Quote Ident 
            | Lam [Ident] SchExpr
            | Apply SchExpr [SchExpr]
            | Bool Bool
            | Err String
            | QList [SchExpr]
            deriving (Show, Eq)


data SchVal = Atom Integer
             | AtmString String           
             | List [SchVal]
             | SchQList [SchVal]
             | DottedList [SchVal] SchVal
             | DottedPair SchVal SchVal
             | SchNumber Int
             | SchFloat Double 
             | SchString String
             | SchChar Char
             | SchBool Bool
             | Error String
             | SchQuote String
             | Env Ctx
             | Closure [Ident] SchExpr Ctx
      deriving (Show, Eq)       


-- data Ctx = Ctx { vars :: [(String, SchVal)] , funcs:: [String] } 
type Ctx = [(Ident, SchVal)]
-- mama = Ctx [][]

--initCtx::Ctx
--initCtx = Ctx [("Nil", (SchNumber 0)), ("Three", (Schfloat 3.0))]

-- initCtx :: Ctx
-- initCtx = Ctx [("Nil", (SchNumber 0)), ("Three", (SchFloat 3.0)), ("Name", (SchString "Tanya" ))]["Mama"]

-- instance Show Ctx where
--    show c = show (head c)
      
      --"Context {Vars = " ++ show (vars c) ++ ", funcs = " ++ show (funcs c) ++ "}"


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



