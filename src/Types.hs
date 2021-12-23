module Types where

data Ctx = Ctx { vars :: [String], funcs:: [String] } 

initCtx :: Ctx
initCtx = Ctx ["Nil", "Var1", "Var2"]["Mama"]


instance Show Ctx where
  show c = "Context {Vars = " ++ show (vars c) ++ ", funcs = " ++ show (funcs c) ++ "}"