{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Haskus.UI.Object.Plane
   ( PlaneF (..)
   , pattern Plane
   )
where

import Haskus.Utils.EADT
import Haskus.Utils.EADT.TH

import Haskus.UI.Maths.Linear
import Haskus.UI.Object
import Haskus.UI.Common

-- | Plane
data PlaneF e = PlaneF
   { planePoint  :: Point3D -- ^ A point on the plane
   , planeNormal :: Normal  -- ^ Normal of the plane
   }
   deriving (Show,Functor)
-- https://gitlab.haskell.org/ghc/ghc/-/issues/18806
-- https://github.com/haskus/haskus-manual/blob/master/source/eadt/basics.rst
-- eadtPattern 'PlaneF "Plane"
pattern Plane p o = VF (PlaneF p o)

instance Object (PlaneF e) where
   hit Ray{..} PlaneF{..} =
      let
         dist = ((planePoint ^-^ rayOrigin) `dot` planeNormal) / (rayDirection `dot` planeNormal)
                -- we don't check that we divide by zero: if we do, we expect +INF
         h    = Hit
                  { hitPoint    = rayOrigin ^+^ (dist *^ rayDirection)
                  , hitNormal   = planeNormal
                  , hitDistance = dist
                  , hitColor    = Nothing
                  }
      in if nearZero dist || dist < 0  -- we use `nearZero` instead of "== 0" which isn't right for floats
            then Nothing
            else Just h
