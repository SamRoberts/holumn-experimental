{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Holumn.NameSpace where

import Control.Applicative (Applicative)
import Control.Monad.Free (Free, MonadFree, iter, liftF, wrap)
import Data.Function (on)
import Data.Holumn.Binds (Binds, single)
import Data.List (groupBy, intercalate, sortBy)
import Data.Ord (comparing)


-- | A namespace
--
-- a namespace is just the free monad on top of a bindings?
newtype NS a = NS { unNS :: Free Binds a }
               deriving (Functor, Applicative, Monad, MonadFree Binds)

qualify :: String -> NS a -> NS a
qualify pre ns = liftF (single pre ()) >> ns
