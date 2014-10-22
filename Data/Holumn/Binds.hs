{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Holumn.Binds where

import qualified Data.Map as M

import Data.Monoid (Monoid)

-- | A flat mapping from names to values
newtype Binds a = Binds { unBinds :: M.Map String a }
                deriving (Show, Functor, Monoid)

size :: Binds a -> Integer
size = fromIntegral . M.size . unBinds

binds :: [Binds a] -> Binds a
binds = Binds
        . M.unionsWith (\_ _ -> error "name could refer to two or more values")
        . map unBinds

single :: String -> a -> Binds a
single name = Binds . M.singleton name

(@=) :: String -> a -> Binds a
(@=) = single

toList :: Binds a -> [(String, a)]
toList = M.toList . unBinds
