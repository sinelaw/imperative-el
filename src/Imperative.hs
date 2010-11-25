{-# LANGUAGE GADTs, EmptyDataDecls, TypeSynonymInstances, DeriveDataTypeable, MonoLocalBinds #-}
module Main where

import Data.Typeable
import Data.List(intersperse)

-- a is the type of the variable, v is the type of the variable's tag
data Variable a v = Variable String v
                deriving (Show)

data Tuple a b = Tuple a b
               deriving (Show, Typeable)

-- First type parameter is expression type, second is variables' tags type
data Expr t v where
  Literal    :: Show t => t -> Expr t v
  VarExp     :: (Show t) => Variable t v -> Expr t v
  
  TupleExp   :: (Show t, Show t') => Expr t v -> Expr t' v -> Expr (Tuple t t') v
  Sequence   :: (Show t, Show t') => Expr t v -> Expr t' v -> Expr t' v
  
  Assignment :: (Show t) => Variable t v -> Expr t v -> Expr t v
  Lambda     :: (Show t, Show t') => Variable t v -> Expr t' v -> Expr (t -> t') v
  Apply      :: (Show t, Show t') => Expr (t -> t') v -> Expr t v -> Expr t' v
  
  If         :: (Show t) => Expr Bool v -> Expr t v -> Expr t v -> Expr t v
  
  deriving (Typeable)


instance Functor (Variable a) where
  fmap f (Variable x v) = Variable x (f v)
  
-- Functor instance: transforms variable tags
instance Functor (Expr a) where
--  fmap :: (a -> b) -> Expr t a -> Expr t b
  fmap _ (Literal x) = Literal x
  fmap f (VarExp var) = VarExp (fmap f var)
  
  fmap f (TupleExp x y) = TupleExp (fmap f x) (fmap f y)
  fmap f (Sequence x y) = Sequence (fmap f x) (fmap f y)
  
  fmap f (Assignment var x) = Assignment (fmap f var) (fmap f x)
  fmap f (Lambda var x) = Lambda (fmap f var) (fmap f x)
  fmap f (Apply x y) = Apply (fmap f x) (fmap f y)

  fmap f (If x y z) = If (fmap f x) (fmap f y) (fmap f z)


instance (Show v) => Show (Expr a v) where
  show (Literal l) = "Literal " ++ show l
  show (VarExp v) = show v
  
  show (TupleExp a b) = "(" ++ show a ++ ", " ++ show b ++ ")"
  show (Sequence a b) = show a ++ "; " ++ show b
  
  show (Assignment var val) = show var ++ " = " ++ show val
  show (Lambda var x) = "\\" ++ show var ++ " -> " ++ show x
  show (Apply x y) = show x ++ " " ++ show y
  
  show (If x y z) = "if (" ++ show x ++ ") then (" ++ show y ++ ") else (" ++ show z ++ ")"
    
showType :: (TyCon, [TypeRep]) -> String
showType (tCon, []) = show tCon
showType (tCon, tReps) = concat ([show tCon, "<", tRepStrs, ">"])
  where tRepStrs = concat . intersperse "," . map (showType . splitTyConApp) $ tReps

showVal :: (Show a, Typeable a) => a -> String
showVal x = show x ++ " :: " ++ (show . typeOf $ x)
      
main :: IO ()
main = do
  let l = Literal (3::Integer)
      l' = Literal 2
      v = Variable "v" (0::Int)
      vEx = VarExp v
      c = TupleExp l l'
      a = Assignment v (TupleExp l l)
      b = Assignment v c

  print b
  print (showVal a)
  print (showVal c)
  print (showVal l')
  print (showVal b)
  print (showVal vEx)
  return ()

  