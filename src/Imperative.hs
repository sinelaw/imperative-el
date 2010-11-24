{-# LANGUAGE GADTs, EmptyDataDecls, TypeSynonymInstances, DeriveDataTypeable #-}
module Main where

import Data.Typeable
import Data.List(intersperse)

data Variable a = Variable String
                deriving (Show)

data Tuple a b = Tuple a b
               deriving (Show, Typeable)

data Expr a where
  Literal    :: Show a => a -> Expr a
  TupleExp   :: (Show a, Show b) => Expr a -> Expr b -> Expr (Tuple a b)
  VarExp     :: Variable a -> Expr a
  Assignment :: Variable a -> Expr a -> Expr a
  deriving (Typeable)

--struct (exprA:(exprB:[])) = Tuple exprA exprB
--struct (expr:exprs)       = Tuple expr (struct exprs)
  

instance Show a => Show (Expr a) where
  show (Literal l) = "Literal " ++ show l
  show (Assignment var val) = show var ++ " = " ++ show val
  show (VarExp v) = show v
  show (TupleExp a b) = "(" ++ show a ++ ", " ++ show b ++ ")"


showType :: (TyCon, [TypeRep]) -> String
showType (tCon, []) = show tCon
showType (tCon, tReps) = concat ([show tCon, "<", tRepStrs, ">"])
  where tRepStrs = concat . intersperse "," . map (showType . splitTyConApp) $ tReps

l = Literal (3::Integer)
v = Variable "v"
b = Assignment v (TupleExp l l)
      
main :: IO ()
main = do
  print b
  print (typeOf b)
  print (showType (splitTyConApp . typeOf $ b))
  return ()

  