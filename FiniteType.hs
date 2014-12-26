{-# LANGUAGE DefaultSignatures #-}
{-
 This module provides a type-class FiniteType that provides a single method
 cardinality, which returns to number of values that inhabit the type.

 Although it is, of course, possible to name the cardinality of types with
 infinite values this is beyond the scope of this module which is only concerned
 with types that have finite values.

 For purposes of cardinality 'undefined' is not counted as value in the type.

 Instances of FiniteType are provided for Char, Bool, Unit, Ordering, Word, Word8
 Word16, Word32, Word64, Int, Int8, Int16, Int32 and Int64.

 Additionally instances for 2-, 3- and 4-tuples are provided.

 Two functions cardinalityFromEnum and cardinalityFromIntegral are provided to
 implement the class instance for types that implement Enum and Integral type
 classes.

 cardinalityFromEnum is provided as the default implementation of cardinality,
 so instances can be implemented by just doing:

   `instance FiniteType X`

 for types that implement the Enum type-class.

 Note: the cardinalityFromEnum function using the partial-function `fromEnum`.
 cardinalityFromEnum will only work if fromEnum works correctly for the minBound
 and maxBound of a type.
-}

module FiniteType (FiniteType, cardinality, cardinalityFromEnum, cardinalityFromIntegral) where

import Data.Proxy
import Data.Word
import Data.Int

--- Can I make these point free? I can't seem to?
maxBoundFromEnum :: (Bounded a, Enum a) => Proxy a -> Integer
maxBoundFromEnum t = (fromIntegral.fromEnum) (maxBound `asProxyTypeOf` t)

minBoundFromEnum :: (Bounded a, Enum a) => Proxy a -> Integer
minBoundFromEnum t = (fromIntegral.fromEnum) (minBound `asProxyTypeOf` t)

maxBoundIntegral :: (Bounded a, Integral a) => Proxy a -> Integer
maxBoundIntegral t = fromIntegral (maxBound `asProxyTypeOf` t)

minBoundIntegral :: (Bounded a, Integral a) => Proxy a -> Integer
minBoundIntegral t = fromIntegral (minBound `asProxyTypeOf` t)

cardinalityFromEnum :: (Bounded a, Enum a) => Proxy a -> Integer
cardinalityFromEnum t = maxBoundFromEnum t - minBoundFromEnum t + 1

cardinalityFromIntegral :: (Bounded a, Integral a) => Proxy a -> Integer
cardinalityFromIntegral t = maxBoundIntegral t - minBoundIntegral t + 1

class FiniteType a where
    cardinality :: Proxy a -> Integer
    default cardinality :: (Bounded a, Enum a) => Proxy a -> Integer
    cardinality = cardinalityFromEnum

-- Implementation the type-class for standard types
instance FiniteType ()
instance FiniteType Bool
instance FiniteType Ordering
instance FiniteType Char

-- Implementation of the type-class for Word types
instance FiniteType Word where { cardinality = cardinalityFromIntegral }
instance FiniteType Word8 where { cardinality = cardinalityFromIntegral }
instance FiniteType Word16 where { cardinality = cardinalityFromIntegral }
instance FiniteType Word32 where { cardinality = cardinalityFromIntegral }
instance FiniteType Word64 where { cardinality = cardinalityFromIntegral }

-- Implementation of the type-class for Int types
instance FiniteType Int
instance FiniteType Int8 where { cardinality = cardinalityFromIntegral }
instance FiniteType Int16 where { cardinality = cardinalityFromIntegral }
instance FiniteType Int32 where { cardinality = cardinalityFromIntegral }
instance FiniteType Int64 where { cardinality = cardinalityFromIntegral }

-- IMPROVE: Seems that there should be a better way of implementing these
instance (FiniteType a, FiniteType b) => FiniteType (a, b) where
    cardinality z =  let (q, r) = t2 z in cardinality q * cardinality r where t2 = (\Proxy -> (Proxy, Proxy)) :: Proxy (a, b) -> (Proxy a, Proxy b)

instance (FiniteType a, FiniteType b, FiniteType c) => FiniteType (a, b, c) where
    cardinality z = let (q, r, s) = t3 z in cardinality q * cardinality r * cardinality s where t3 = (\Proxy -> (Proxy, Proxy, Proxy)) :: Proxy (a, b, c) -> (Proxy a, Proxy b, Proxy c)

instance (FiniteType a, FiniteType b, FiniteType c, FiniteType d) => FiniteType (a, b, c, d) where
    cardinality z = let (q, r, s, t) = t4 z in cardinality q * cardinality r * cardinality s * cardinality t where t4 = (\Proxy -> (Proxy, Proxy, Proxy, Proxy)) :: Proxy (a, b, c, d) -> (Proxy a, Proxy b, Proxy c, Proxy d)


test = cardinality (Proxy :: Proxy ()) == 1 &&
       cardinality (Proxy :: Proxy Bool) == 2 &&
       cardinality (Proxy :: Proxy Ordering) == 3 &&
       cardinality (Proxy :: Proxy Word8) == 2 ^ 8 &&
       cardinality (Proxy :: Proxy Word16) == 2 ^ 16 &&
       cardinality (Proxy :: Proxy Word32) == 2 ^ 32 &&
       cardinality (Proxy :: Proxy Word64) == 2 ^ 64 &&
       cardinality (Proxy :: Proxy Int8) == 2 ^ 8 &&
       cardinality (Proxy :: Proxy Int16) == 2 ^ 16 &&
       cardinality (Proxy :: Proxy Int32) == 2 ^ 32 &&
       cardinality (Proxy :: Proxy Int64) == 2 ^ 64
