module Data.Holumn.Serial where

import Control.Applicative ((<|>), liftA2)
import Control.Monad (guard, mfilter)
import Data.List (foldl')

import Data.Holumn.Vals ((<->))
import qualified Data.Holumn.Vals as V

-- | An open data type for a schema representing a sequence of bits
--
-- Have a field for arbitrary meta data, as I'm currently thinking I need
-- meta data, but am not sure what it will look like.
--
-- The list of serials in Cat and Alt must be non-empty.
--
-- Currently using standard unsigned int format for all list lengths.
-- Might try some variable width format in the future.
--
-- Don't really support very large lists well: not without going back and
-- filling length in, which seems liks a bad thing. Ideally could read and write
-- format in one pass. Not so easy. Ignore for now.
--
-- One possible alternative for alternatives is to have items sorted by type
-- first, than order. This would allow you to filter by type and read just the
-- relevant info. On the other hand, it would also suck if you wanted to read
-- all values (because you'd be jumping around the place). Have chosen just to
-- have items in their natural order, so the alternative is already adequately
-- covered by using columns anyway.
data Serial a = Prim a V.Vals             -- ^ A primitive with given range
              | Cat  a [Serial a]         -- ^ A sequence of serials. Represented as the items in sequence.
              | Alt  a V.Vals [Serial a]  -- ^ A list of alternative types with given length range. Represented as length, than tag list, than items
              | List a V.Vals (Serial a)  -- ^ A list of one serial with given length range. Represented as length, than items

-- would it be convienient to make Serial be the free monad of SerialF? What would return mean?
data SerialF a b = PrimF a V.Vals
                 | CatF  a [b]
                 | AltF  a V.Vals [b]
                 | ListF a V.Vals b

fold :: (SerialF a b -> b) -> Serial a -> b
fold f (Prim m vals)   = f $ PrimF m      $ vals
fold f (Cat m ss)      = f $ CatF  m      $ map (fold f) ss
fold f (Alt m vals ss) = f $ AltF  m vals $ map (fold f) ss
fold f (List m vals s) = f $ ListF m vals $ fold f s

bits :: Serial a -> V.Vals
bits = fold f where
  f (PrimF _ vals) =
    V.constant $ V.bits vals

  f (CatF _ sizes) =
    foldl' (\x y -> (V.low x + V.low y) <-> (V.high x + V.high y)) (V.constant 0) sizes

  f (AltF _ lengths sizes) =
    (V.low lengths * minimum (map V.low sizes)) <-> (V.high lengths * maximum (map V.high sizes))

  f (ListF _ lengths size) =
    (V.low lengths * V.low size) <-> (V.high lengths * V.high size)

fixedBits :: Serial a -> Maybe Integer
fixedBits = toConstant . bits where
  toConstant len | V.low len == V.high len = Just (V.low len)
                 | otherwise   = Nothing
