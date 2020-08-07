module Mu.Util (prettyAST) where

import qualified Data.Text as T
import Mu.Parser

prettyAST :: AST -> T.Text
prettyAST (Variable v)                        = v
prettyAST (Abstraction v b@(Application _ _)) = "\\" <> v <> ".(" <> prettyAST b <> ")"
prettyAST (Abstraction v b)                   = "\\" <> v <> "." <> prettyAST b
prettyAST (Application f a)                   = prettyAST f <> " " <> prettyAST a
prettyAST (Alias n e)                         = n <> " = " <> prettyAST e
