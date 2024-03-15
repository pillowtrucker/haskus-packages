{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

-- | Types with finite bit count
module Haskus.Binary.Bits.Finite
   ( FiniteBits (..)
   )
where

import Haskus.Utils.Types
import Haskus.Number.Word
import Haskus.Number.Int
import GHC.Exts

#include "MachDeps.h"

-- | Type representable by a fixed amount of bits
class FiniteBits a where

   -- | Number of bits
   type BitSize a :: Nat

   -- | Number of bits (the value is ignored)
   bitSize :: (Integral i, KnownNat (BitSize a)) => a -> i
   bitSize _ = natValue @(BitSize a)
   
   -- | All bits set to 0
   zeroBits :: a

   -- | All bits set to 1
   oneBits :: a
   oneBits = complement zeroBits

   -- | Count number of zero bits preceding the most significant set bit
   countLeadingZeros :: a -> Word

   -- | Count number of zero bits following the least significant set bit
   countTrailingZeros :: a -> Word

   -- | Complement
   complement :: a -> a


instance FiniteBits Word where
   type BitSize Word          = WORD_SIZE_IN_BITS
   zeroBits                   = 0
   oneBits                    = maxBound
   countLeadingZeros  (W# x#) = W# (clz# x#)
   countTrailingZeros (W# x#) = W# (ctz# x#)
   complement (W# x#)         = W# (x# `xor#` mb#)
      where !(W# mb#) = maxBound

instance FiniteBits Word8 where
   type BitSize Word8          = 8
   zeroBits                    = 0
   oneBits                     = maxBound
   countLeadingZeros  (W8# x#) = W# (clz8# (word8ToWord# x#))
   countTrailingZeros (W8# x#) = W# (ctz8# (word8ToWord# x#))
   complement (W8# x#)         = W8# (wordToWord8# (word8ToWord# x# `xor#` (word8ToWord# mb#)))
      where !(W8# mb#) = maxBound

instance FiniteBits Word16 where
   type BitSize Word16          = 16
   zeroBits                     = 0
   oneBits                      = maxBound
   countLeadingZeros  (W16# x#) = W# (clz16# (word16ToWord# x#))
   countTrailingZeros (W16# x#) = W# (ctz16# (word16ToWord# x#))
   complement (W16# x#)         = W16# (wordToWord16# (word16ToWord# x# `xor#` (word16ToWord# mb#)))
      where !(W16# mb#) = maxBound

instance FiniteBits Word32 where
   type BitSize Word32          = 32
   zeroBits                     = 0
   oneBits                      = maxBound
   countLeadingZeros  (W32# x#) = W# (clz32# (word32ToWord# x#))
   countTrailingZeros (W32# x#) = W# (ctz32# (word32ToWord# x#))
   complement (W32# x#)         = W32# (wordToWord32# (word32ToWord# x# `xor#` (word32ToWord# mb#)))
      where !(W32# mb#) = maxBound

instance FiniteBits Word64 where
   type BitSize Word64          = 64
   zeroBits                     = 0
   oneBits                      = maxBound
   countLeadingZeros :: Word64 -> Word
   countLeadingZeros  (W64# x#) = W# (clz64# x#)
   countTrailingZeros (W64# x#) = W# (ctz64# x#)
   complement (W64# x#)         = W64# (x# `xor64#` mb#)
      where !(W64# mb#) = maxBound


instance FiniteBits Int where
   type BitSize Int           = WORD_SIZE_IN_BITS
   zeroBits                   = 0
   oneBits                    = (-1)
   countLeadingZeros  (I# x#) = W# (clz# (int2Word# x#))
   countTrailingZeros (I# x#) = W# (ctz# (int2Word# x#))
   complement (I# x#)         = I# (notI# x#)

instance FiniteBits Int8 where
   type BitSize Int8           = 8
   zeroBits                    = 0
   oneBits                     = (-1)
   countLeadingZeros  (I8# x#) = W# (clz8# (int2Word# (int8ToInt# x#)))
   countTrailingZeros (I8# x#) = W# (ctz8# (int2Word# (int8ToInt# x#)))
   complement (I8# x#)         = I8# (intToInt8# (word2Int# (not# (int2Word# (int8ToInt# x#)))))

instance FiniteBits Int16 where
   type BitSize Int16           = 16
   zeroBits                     = 0
   oneBits                      = (-1)
   countLeadingZeros  (I16# x#) = W# (clz16# (int2Word# (int16ToInt# x#)))
   countTrailingZeros (I16# x#) = W# (ctz16# (int2Word# (int16ToInt# x#)))
   complement (I16# x#)         = I16# (intToInt16# (word2Int# (not# (int2Word# (int16ToInt# x#)))))

instance FiniteBits Int32 where
   type BitSize Int32           = 32
   zeroBits                     = 0
   oneBits                      = (-1)
   countLeadingZeros  (I32# x#) = W# (clz32# (int2Word# (int32ToInt# x#)))
   countTrailingZeros (I32# x#) = W# (ctz32# (int2Word# (int32ToInt# x#)))
   complement (I32# x#)         = I32# (intToInt32# (word2Int# (not# (int2Word# (int32ToInt# x#)))))

instance FiniteBits Int64 where
   type BitSize Int64           = 64
   zeroBits                     = 0
   oneBits                      = (-1)
   countLeadingZeros  (I64# x#) = W# (clz64# (int64ToWord64# x#))
   countTrailingZeros (I64# x#) = W# (ctz64# (int64ToWord64# x#))
   complement (I64# x#)         = I64# (word64ToInt64# (int64ToWord64# x# `xor64#` (int64ToWord64# (intToInt64# -1#))))
