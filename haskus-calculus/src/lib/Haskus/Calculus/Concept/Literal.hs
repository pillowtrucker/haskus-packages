{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Haskus.Calculus.Concept.Literal
   ( LiteralF (..)
   , pattern LitInt
   , pattern LitString
   )
where

import Haskus.Utils.EADT
import Haskus.Utils.EADT.TH

import Haskus.Calculus.PrettyPrint
import Haskus.Calculus.FreeVars

import Data.Set as Set

-- | Literal
data LiteralF e
   = LitIntF Integer
   | LitStringF String
   deriving (Functor)
-- https://gitlab.haskell.org/ghc/ghc/-/issues/18806
-- https://github.com/haskus/haskus-manual/blob/master/source/eadt/basics.rst
-- $(eadtPattern 'LitIntF    "LitInt")
pattern LitInt i = VF (LitIntF i)
-- $(eadtPattern 'LitStringF "LitString")
pattern LitString s = VF (LitStringF s)
instance PrettyPrintF LiteralF where
   prettyPrintF (LitIntF n)    = (False,show n)
   prettyPrintF (LitStringF n) = (False,show n)

instance FreeVarsF n LiteralF where
   freeVarsF _ = Set.empty
