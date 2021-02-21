{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE PolyKinds #-}
module Type where

import           Data.Kind

data Expression a
  = EmptySet
  | EmptyStr
  | Char a
  | App (Expression a) (Expression a)
  | Union (Expression a) (Expression a)
  | Star (Expression a)

--exprMatch :: (Eq a) => [a] -> Expression a -> Bool
--exprMatch [] EmptyStr = True
--exprMatch [c] (Char c) = True

--data ExprMatch :: [a] -> Expression a -> Type where
  --MEmpty :: ExprMatch '[] EmptyStr
  --MChar :: (c :: Char) -> ExprMatch '[c] (EChar c)
