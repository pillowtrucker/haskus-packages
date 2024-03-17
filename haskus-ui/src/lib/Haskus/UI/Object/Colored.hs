{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Haskus.UI.Object.Colored
   ( ColoredF (..)
   , pattern Colored
   )
where

import Haskus.Utils.Flow
import Haskus.Utils.EADT
import Haskus.Utils.EADT.TH

import Haskus.UI.Object
import Haskus.UI.Common

-- | Add default color to inner object
data ColoredF e = ColoredF Color e deriving (Functor)

-- https://gitlab.haskell.org/ghc/ghc/-/issues/18806
-- https://github.com/haskus/haskus-manual/blob/master/source/eadt/basics.rst
--eadtPattern 'ColoredF "Colored"
pattern Colored c o = VF (ColoredF c o)
instance Object e => Object (ColoredF e) where
   hit r (ColoredF c o) = hit r o ||> (\x -> x { hitColor = Just c })

