module Data.Holumn.Layout where

import Control.Monad.Free (wrap)
import Data.Holumn.NameSpace (Flat, NS, (@=), flat, flatten, namespace, range, unFlat)
import Data.Holumn.Range (Range)
import qualified Data.Holumn.Type as T
import qualified Data.Map as M
import Data.Maybe (maybe)


-- | Logical representation of physical layout of Type
--
-- Three is no notion in the type of how to match up a tag with its corresponding
-- union, or a length with it's corresponding array. I could use tags in the
-- Stream type, as per Reader, but what about the overlying namespace layer?
--
-- If I can figure that out, maybe this can become the canonical representation,
-- and I can generate the "full" readers and writers from this type.
--
-- This type deliberately has no notion of how the streams are implemented. They
-- could be separate files, or blocks within the same file, or some in-memory
-- byte string, or something else entirely. That's a separate piece of metadata.
type Layout = NS Stream

data Stream = Val Range            -- ^ A primitive value, includes unit, bools, ints, chars, etc ...
              -- products
            | Struct (Flat Stream) -- ^ A product type
              -- sums
            | Union (Flat Stream)  -- ^ An *untagged* sum type
            | Tag Range            -- ^ A tag for one or more sum types
              -- lists
            | Array Stream         -- ^ An array items with unknown size
            | Length Range         -- ^ The length of one or more arrays
            deriving (Eq, Show)

layout :: T.Type -> Layout
layout = return . stream
  where stream (T.Prim r)     = Val r
        stream (T.Prod items) = Struct $ flatten $ fmap stream items
        stream (T.Sum  items) = let flatItems = flatten $ fmap stream items
                                in Struct $ flat [ "tag"   @= Tag (range flatItems)
                                                 , "union" @= Union flatItems
                                                 ]
        stream (T.List r typ) = Struct $ flat [ "length" @= Length r
                                              , "array"  @= Array (stream typ)
                                              ]

-- | Apply a tranformation that looks like a splitter to a layout, splitting as far as we can go
fullyApply :: (Stream -> Maybe Layout) -> Layout -> Layout
fullyApply splitter layout =
  layout >>= \stream -> maybe (return stream) (fullyApply splitter) (splitter stream)

-- | Push the namespace down, splitting stream up into separate streams
--
-- This does one level at a time. It will return Nothing if it can't split the
-- type, so the algorithm applying the split knows when to stop.
--
-- Not entirely happy with this reactive way of determining when to stop, would
-- prefer to Stream type to have enough information to pre-emptively tell us
-- what transformations are applicable. But I'll worry about that once I have
-- more transformations to consider.
splitStream :: Stream -> Maybe Layout
splitStream (Val r)        = Nothing
splitStream (Tag r)        = Nothing
splitStream (Length r)     = Nothing
splitStream (Struct items) = Just $ namespace items
splitStream (Union items)  = Just $ namespace items
splitStream (Array stream) = fmap (fmap Array) $ splitStream stream
