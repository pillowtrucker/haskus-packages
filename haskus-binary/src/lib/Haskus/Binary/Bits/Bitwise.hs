{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

-- | Bitwise bit operations
module Haskus.Binary.Bits.Bitwise
   ( Bitwise (..)
   )
where

import Haskus.Number.Word
import Haskus.Number.Int
import GHC.Exts
import GHC.Num

#if MIN_VERSION_GLASGOW_HASKELL (9,0,0,0)
import GHC.Natural
import GHC.Integer
#endif

-- | Bitwise bit operations
class Bitwise a where
   -- | Bitwise "and"
   (.&.) :: a -> a -> a

   -- | Bitwise "or"
   (.|.) :: a -> a -> a

   -- | Bitwise "xor"
   xor :: a -> a -> a


instance Bitwise Word where
   (W# x#) .&.   (W# y#) = W# (x# `and#` y#)
   (W# x#) .|.   (W# y#) = W# (x# `or#` y#)
   (W# x#) `xor` (W# y#) = W# (x# `xor#` y#)

instance Bitwise Word8 where
   (W8# x#) .&.   (W8# y#) = W8# (wordToWord8# ((word8ToWord# x#) `and#` (word8ToWord# y#)))
   (W8# x#) .|.   (W8# y#) = W8# (wordToWord8# ((word8ToWord# x#) `or#` (word8ToWord# y#)))
   (W8# x#) `xor` (W8# y#) = W8# (wordToWord8# ((word8ToWord# x#) `xor#` (word8ToWord# y#)))

instance Bitwise Word16 where
   (W16# x#) .&.   (W16# y#) = W16# (wordToWord16# ((word16ToWord# x#) `and#` (word16ToWord# y#)))
   (W16# x#) .|.   (W16# y#) = W16# (wordToWord16# ((word16ToWord# x#) `or#` (word16ToWord# y#)))
   (W16# x#) `xor` (W16# y#) = W16# (wordToWord16# ((word16ToWord# x#) `xor#` (word16ToWord# y#)))

instance Bitwise Word32 where
   (W32# x#) .&.   (W32# y#) = W32# (wordToWord32# ((word32ToWord# x#) `and#` (word32ToWord# y#)))
   (W32# x#) .|.   (W32# y#) = W32# (wordToWord32# ((word32ToWord# x#) `or#` (word32ToWord# y#)))
   (W32# x#) `xor` (W32# y#) = W32# (wordToWord32# ((word32ToWord# x#) `xor#` (word32ToWord# y#)))

instance Bitwise Word64 where
   (W64# x#) .&.   (W64# y#) = W64# (x# `and64#` y#)
   (W64# x#) .|.   (W64# y#) = W64# (x# `or64#` y#)
   (W64# x#) `xor` (W64# y#) = W64# (x# `xor64#` y#)

instance Bitwise Int where
   (I# x#) .&.   (I# y#) = I# (x# `andI#` y#)
   (I# x#) .|.   (I# y#) = I# (x# `orI#` y#)
   (I# x#) `xor` (I# y#) = I# (x# `xorI#` y#)

instance Bitwise Int8 where
   (I8# x#) .&.   (I8# y#) = I8# (intToInt8# (word2Int# ((int2Word# (int8ToInt# x#)) `and#` (int2Word# (int8ToInt# y#)))))
   (I8# x#) .|.   (I8# y#) = I8# (intToInt8# (word2Int# ((int2Word# (int8ToInt# x#)) `or#`  (int2Word# (int8ToInt# y#)))))
   (I8# x#) `xor` (I8# y#) = I8# (intToInt8# (word2Int# ((int2Word# (int8ToInt# x#)) `xor#` (int2Word# (int8ToInt# y#)))))

instance Bitwise Int16 where
   (I16# x#) .&.   (I16# y#) = I16# (intToInt16# (word2Int# ((int2Word# (int16ToInt# x#)) `and#` (int2Word# (int16ToInt# y#)))))
   (I16# x#) .|.   (I16# y#) = I16# (intToInt16# (word2Int# ((int2Word# (int16ToInt# x#)) `or#`  (int2Word# (int16ToInt# y#)))))
   (I16# x#) `xor` (I16# y#) = I16# (intToInt16# (word2Int# ((int2Word# (int16ToInt# x#)) `xor#` (int2Word# (int16ToInt# y#)))))

instance Bitwise Int32 where
   (I32# x#) .&.   (I32# y#) = I32# (intToInt32# (word2Int# ((int2Word# (int32ToInt# x#)) `and#` (int2Word# (int32ToInt# y#)))))
   (I32# x#) .|.   (I32# y#) = I32# (intToInt32# (word2Int# ((int2Word# (int32ToInt# x#)) `or#`  (int2Word# (int32ToInt# y#)))))
   (I32# x#) `xor` (I32# y#) = I32# (intToInt32# (word2Int# ((int2Word# (int32ToInt# x#)) `xor#` (int2Word# (int32ToInt# y#)))))

instance Bitwise Int64 where
   (I64# x#) .&.   (I64# y#) = I64# (word64ToInt64# (int64ToWord64# x# `and64#` (int64ToWord64# y#)))
   (I64# x#) .|.   (I64# y#) = I64# (word64ToInt64# (int64ToWord64# x# `or64#` int64ToWord64# y#))
   (I64# x#) `xor` (I64# y#) = I64# (word64ToInt64# (int64ToWord64# x# `xor64#` int64ToWord64# y#))

instance Bitwise Integer where
   (.&.)      = andInteger
   (.|.)      = orInteger
   xor        = xorInteger

instance Bitwise Natural where
   (.&.)      = andNatural
   (.|.)      = orNatural
   xor        = xorNatural
