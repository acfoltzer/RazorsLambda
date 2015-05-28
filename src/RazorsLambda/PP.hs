{-# LANGUAGE MultiParamTypeClasses #-}
module RazorsLambda.PP where

import Text.PrettyPrint.Annotated.Leijen

data OutputAnnotation
  = AnnConst
  | AnnVar
  | AnnType
  | AnnKeyword
  deriving Show

class PP a where
  pp :: a -> Doc OutputAnnotation
