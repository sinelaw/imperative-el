{-# LANGUAGE GADTs, EmptyDataDecls, TypeSynonymInstances, DeriveDataTypeable #-}
module Main where

import Data.Typeable

data Variable a = Variable String
                deriving (Show)

data Expr a where
  Literal    :: Show a => a -> Expr a
  Tuple      :: (Show a, Show b) => Expr a -> Expr b -> Expr (a,b)
  VarExp     :: Variable a -> Expr a
  Assignment :: Variable a -> Expr a -> Expr a
  deriving (Typeable)

--struct (exprA:(exprB:[])) = Tuple exprA exprB
--struct (expr:exprs)       = Tuple expr (struct exprs)
  
instance Show a => Show (Expr a) where
  show (Literal l) = "Literal " ++ show l
  show (Assignment var val) = show var ++ " = " ++ show val
  show (VarExp v) = show v
  show (Tuple a b) = "(" ++ show a ++ ", " ++ show b ++ ")"

class DSLType a where
  showType :: Expr a -> String

instance DSLType Integer where showType _ = "Integer"
instance DSLType Double where  showType _ = "Double"    
instance DSLType String where  showType _ = "String"

-- need something that does:
-- instance (DSLType a, DSLType b) => DSLType (a,b) where  
--   showType _ = "Tuple<" ++ showType a ++ ", " ++ showType b ++ ">"

-- Doesn't work! vEx's type is not determined for some reason
l = Literal (3::Integer)
v = Variable "v"
vEx = VarExp v
a = Assignment v vEx
b = Assignment v (Tuple l l)
      
main :: IO ()
main = do
  print b
  print (showType b)
--  print (showType vEx)
  return ()

  