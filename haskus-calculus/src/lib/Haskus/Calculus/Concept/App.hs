{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Haskus.Calculus.Concept.App
   ( AppF (..)
   , pattern App
   )
where

import Haskus.Utils.EADT
import Haskus.Utils.EADT.TH

import Haskus.Calculus.PrettyPrint
import Haskus.Calculus.FreeVars

import Data.Set as Set

-- | Application
data AppF e = AppF e e deriving (Functor)
-- https://gitlab.haskell.org/ghc/ghc/-/issues/18806
-- https://github.com/haskus/haskus-manual/blob/master/source/eadt/basics.rst
-- $(eadtPattern 'AppF "App")
pattern App e o = VF (AppF e o)


instance PrettyPrintF AppF where
   prettyPrintF (AppF (b1,e1) (b2,e2)) = (True,withParen b1 e1 ++ " "++ withParen b2 e2)

instance Ord n => FreeVarsF n AppF where
   freeVarsF (AppF s1 s2) = Set.union s1 s2
