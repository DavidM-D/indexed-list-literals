{-# Language
  DataKinds,
  MultiParamTypeClasses,
  TypeInType,
  ConstraintKinds,
  FunctionalDependencies,
  FlexibleInstances,
  TupleSections
  #-}
module Data.IndexedListLiterals (
    IndexedListLiterals(..)
  , ILL
  , module Data.Tuple.OneTuple
  ) where

import GHC.TypeNats
import Data.Kind
import Data.Tuple.OneTuple
-- import Control.Monad

-- | An alias for IndexedListLiterals
type ILL = IndexedListLiterals

-- | A type class which allows you to write tuples which can be transformed into a list
--   the length of the list is also provided as a Nat
class IndexedListLiterals (input :: Type) (length :: Nat) (output :: Type) | output length -> input, input -> output length where
  toList :: input -> [output]

instance IndexedListLiterals (ZeroTuple a) 0 a where
  toList ZeroTuple = []

instance IndexedListLiterals (OneTuple a) 1 a where
  toList (OneTuple a) = [a]

-- | Intuitively the zero tuple is () or Void but this breaks the Functional Dependency "input -> output length" stopping reliable inference
data ZeroTuple a = ZeroTuple

-- all code generated below comes from this function
-- generate :: Int -- ^ up to N
--          -> String
-- generate n = unlines $ join $ map ("":) $ take n $ dropOneTuple res where
--   values = map ((:) 'a' . show) [1 :: Int ..]
--   types  = "a" : types
--   withCommas = scanl1 (\a b -> a++","++b)
--   className = "IndexedListLiterals"
--   template tys vals length =
--     ["instance " ++ className ++ " (" ++ tys ++ ") " ++ show length ++ " " ++ head types ++ " where"
--     ,"  toList (" ++ vals ++ ") = [" ++ vals ++ "]"]
--   res = zipWith3 template (withCommas types) (withCommas values) [0 :: Int ..]
--   dropOneTuple = tail

instance IndexedListLiterals (a,a) 2 a where
  toList (a1,a2) = [a1,a2]

instance IndexedListLiterals (a,a,a) 3 a where
  toList (a1,a2,a3) = [a1,a2,a3]

instance IndexedListLiterals (a,a,a,a) 4 a where
  toList (a1,a2,a3,a4) = [a1,a2,a3,a4]

instance IndexedListLiterals (a,a,a,a,a) 5 a where
  toList (a1,a2,a3,a4,a5) = [a1,a2,a3,a4,a5]

instance IndexedListLiterals (a,a,a,a,a,a) 6 a where
  toList (a1,a2,a3,a4,a5,a6) = [a1,a2,a3,a4,a5,a6]

instance IndexedListLiterals (a,a,a,a,a,a,a) 7 a where
  toList (a1,a2,a3,a4,a5,a6,a7) = [a1,a2,a3,a4,a5,a6,a7]

instance IndexedListLiterals (a,a,a,a,a,a,a,a) 8 a where
  toList (a1,a2,a3,a4,a5,a6,a7,a8) = [a1,a2,a3,a4,a5,a6,a7,a8]

instance IndexedListLiterals (a,a,a,a,a,a,a,a,a) 9 a where
  toList (a1,a2,a3,a4,a5,a6,a7,a8,a9) = [a1,a2,a3,a4,a5,a6,a7,a8,a9]

instance IndexedListLiterals (a,a,a,a,a,a,a,a,a,a) 10 a where
  toList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10]

instance IndexedListLiterals (a,a,a,a,a,a,a,a,a,a,a) 11 a where
  toList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11]

instance IndexedListLiterals (a,a,a,a,a,a,a,a,a,a,a,a) 12 a where
  toList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12]

instance IndexedListLiterals (a,a,a,a,a,a,a,a,a,a,a,a,a) 13 a where
  toList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13]

instance IndexedListLiterals (a,a,a,a,a,a,a,a,a,a,a,a,a,a) 14 a where
  toList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14]

instance IndexedListLiterals (a,a,a,a,a,a,a,a,a,a,a,a,a,a,a) 15 a where
  toList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15]

instance IndexedListLiterals (a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a) 16 a where
  toList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16]

instance IndexedListLiterals (a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a) 17 a where
  toList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17]

instance IndexedListLiterals (a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a) 18 a where
  toList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18]

instance IndexedListLiterals (a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a) 19 a where
  toList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19]

instance IndexedListLiterals (a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a) 20 a where
  toList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20]
