{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Holumn.NameSpace where

import Control.Applicative (Applicative)
import Control.Monad.Free (Free, MonadFree, iter, liftF, wrap)
import Data.Function (on)
import Data.List (groupBy, intercalate, sortBy)
import qualified Data.Map as M
import Data.Monoid (Monoid)
import Data.Ord (comparing)


-- | A namespace
--
-- a namespace is just the free monad on top of a bindings?
newtype NS a = NS { unNS :: Free (M.Map String) a }
             deriving (Functor, Applicative, Monad, MonadFree (M.Map String), Show)

(@=) :: String -> a -> NS a
(@=) = single

single :: String -> a -> NS a
single k v = qualify k $ return v

qualify :: String -> NS a -> NS a
qualify pre ns = liftF (M.singleton pre ()) >> ns

-- | Flatten a namespace into a sequence of a's, with full path, and a unique id
--
-- The id's need to be assigned consistently, with a simple understandable scheme
-- (e.g., not just whatever ordering Map decides to return the elements in by default).
-- At the moment I do auto-incrementing ids on the alphabetical order of paths.
flatten :: NS a -> [(Integer, [String], a)]
flatten =
  addIds . iter flattenNode . unNS . fmap flattenLeaf
    where
      flattenLeaf a = [([], a)]

      flattenNode map = do
        (name, flat) <- M.toAscList map
        (path, a)    <- flat
        return $ (name : path, a)

      addIds = zipWith (\id (path, a) -> (id, path, a)) [0..]
