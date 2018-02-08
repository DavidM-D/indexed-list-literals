{-# Language
  DataKinds,
  MultiParamTypeClasses,
  TypeInType,
  ConstraintKinds,
  FunctionalDependencies,
  FlexibleInstances
  #-}
module Data.IndexedContainerLiteral (
    IndexedContainerLiteral(..)
  , ICL
  , module Data.Tuple.OneTuple
  ) where

import GHC.TypeNats
import Data.Kind
import Data.Tuple.OneTuple
--import Control.Monad

type ICL = IndexedContainerLiteral

class IndexedContainerLiteral (input :: Type) (index :: Nat) (output :: Type) | output index -> input, input -> output index where
  toList :: input -> [output]

instance IndexedContainerLiteral (OneTuple a) 1 a where
  toList (OneTuple a) = [a]

-- all code generated below comes from this function

-- generate :: Int -- ^ up to N
--          -> String
-- generate n = unlines $ tail $ join $ map ("":) $ take (n - 1) $ tail res where
--   values = map ((:) 'a' . show) [1 :: Int ..]
--   types  = "a" : types
--   withCommas = scanl1 (\a b -> a++","++b)
--   className = "IndexedContainerLiteral"
--   template tys vals index =
--     ["instance " ++ className ++ " (" ++ tys ++ ") " ++ show index ++ " " ++ head types ++ " where"
--     ,"  toList (" ++ vals ++ ") = [" ++ vals ++ "]"]
--   res = zipWith3 template (withCommas types) (withCommas values) [1 :: Int ..]

instance IndexedContainerLiteral (a,a) 2 a where
  toList (a1,a2) = [a1,a2]

instance IndexedContainerLiteral (a,a,a) 3 a where
  toList (a1,a2,a3) = [a1,a2,a3]

instance IndexedContainerLiteral (a,a,a,a) 4 a where
  toList (a1,a2,a3,a4) = [a1,a2,a3,a4]

instance IndexedContainerLiteral (a,a,a,a,a) 5 a where
  toList (a1,a2,a3,a4,a5) = [a1,a2,a3,a4,a5]

instance IndexedContainerLiteral (a,a,a,a,a,a) 6 a where
  toList (a1,a2,a3,a4,a5,a6) = [a1,a2,a3,a4,a5,a6]

instance IndexedContainerLiteral (a,a,a,a,a,a,a) 7 a where
  toList (a1,a2,a3,a4,a5,a6,a7) = [a1,a2,a3,a4,a5,a6,a7]

instance IndexedContainerLiteral (a,a,a,a,a,a,a,a) 8 a where
  toList (a1,a2,a3,a4,a5,a6,a7,a8) = [a1,a2,a3,a4,a5,a6,a7,a8]

instance IndexedContainerLiteral (a,a,a,a,a,a,a,a,a) 9 a where
  toList (a1,a2,a3,a4,a5,a6,a7,a8,a9) = [a1,a2,a3,a4,a5,a6,a7,a8,a9]

instance IndexedContainerLiteral (a,a,a,a,a,a,a,a,a,a) 10 a where
  toList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10]

instance IndexedContainerLiteral (a,a,a,a,a,a,a,a,a,a,a) 11 a where
  toList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11]

instance IndexedContainerLiteral (a,a,a,a,a,a,a,a,a,a,a,a) 12 a where
  toList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12]

instance IndexedContainerLiteral (a,a,a,a,a,a,a,a,a,a,a,a,a) 13 a where
  toList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13]

instance IndexedContainerLiteral (a,a,a,a,a,a,a,a,a,a,a,a,a,a) 14 a where
  toList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14]

instance IndexedContainerLiteral (a,a,a,a,a,a,a,a,a,a,a,a,a,a,a) 15 a where
  toList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15]

instance IndexedContainerLiteral (a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a) 16 a where
  toList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16]

instance IndexedContainerLiteral (a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a) 17 a where
  toList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17]

instance IndexedContainerLiteral (a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a) 18 a where
  toList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18]

instance IndexedContainerLiteral (a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a) 19 a where
  toList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19]

instance IndexedContainerLiteral (a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a) 20 a where
  toList (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20]
