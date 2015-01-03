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

flatten :: NS a -> [([String], a)]

