{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Holumn.NameSpace where

import Control.Applicative (Applicative)
import Control.Monad.Free (Free, MonadFree, iter, liftF, wrap)
import Data.Foldable (Foldable)
import Data.Function (on)
import Data.List (groupBy, intercalate, sortBy)
import qualified Data.Map as M
import Data.Monoid (Monoid)
import Data.Ord (comparing)
import Data.Traversable(Traversable)

import Data.Holumn.Range (Range(Range))


-- | A namespace
--
-- a namespace is just the free monad on top of a bindings?
newtype NS a = NS { unNS :: Free (M.Map String) a }
             deriving (Functor, Applicative, Monad, Foldable, Traversable, MonadFree (M.Map String), Eq, Show)

single :: String -> a -> NS a
single k v = qualify k $ return v

qualify :: String -> NS a -> NS a
qualify pre ns = liftF (M.singleton pre ()) >> ns

-- | A namespace with a particular ordering
newtype Flat a = Flat { unFlat :: [([String], a)] }
               deriving (Eq, Show)

-- | Flatten a namespace into an ordered sequence of a's and their paths
--
-- The order need to be assigned consistently, with a simple understandable
-- scheme (e.g., not just whatever ordering Map decides to return the elements
-- in by default). At the moment I do auto-incrementing ids on the alphabetical
-- order of paths.
flatten :: NS a -> Flat a
flatten = Flat . iter flattenNode . unNS . fmap flattenLeaf
  where
    flattenLeaf a = [([], a)]
    flattenNode map = do
      (name, flat) <- M.toAscList map
      (path, a)    <- flat
      return $ (name : path, a)

-- | turn a flattened namespace back into a namespace again
--
-- Note that the Flat data type allows structures which are not valid flattened
-- namespaces. (e.g. Flat [([], 1), (["a"], 2])). This function does not attempt
-- to handle such invalid values. However, it should guarantee that
-- (namespace . flatten) == identity.
namespace :: Flat a -> NS a
namespace (Flat [([], a)]) = return a
namespace (Flat items) = wrap nodeToNS
  where
    nodeToNS = fmap namespace nodeToFlat
    nodeToFlat = fmap Flat $ M.fromListWith (++) $ map (fmap (:[])) nodeRemainderPairs
    nodeRemainderPairs = map (\((node:remainder), a) -> (node, (remainder, a))) items

-- Intended to be used with @=
ns :: [(String, NS a)] -> NS a
ns = wrap . M.fromList

-- Intended to be used with @=
flat :: [(String, a)] -> Flat a
flat = Flat . map (\(name,a) -> ([name], a))

-- Intended to be used with flat and ns
(@=) :: String -> a -> (String,a)
(@=) = (,)

range :: Flat a -> Range
range flat = Range 0 $ toInteger $ length (unFlat flat) - 1
