module Data.Holumn.Layout where

import Control.Applicative ((<$>))
import Data.Maybe (maybe)

import Data.Holumn.NameSpace (NS)
import Data.Holumn.Range (Range)


-- | Logical representation of physical layout of Type
--
-- Three is no notion in the type of how to match up a tag with its corresponding
-- union, or a length with it's corresponding array. I hope that as I implement
-- the corresponding reader and writer transformations, I can figure out how to
-- represent those details in this type. If I can figure that out, maybe this can
-- become the canonical representation, and I can generate the "full" readers and
-- writers from this type.
--
-- This type deliberately has no notion of how the streams are implemented. They
-- could be separate files, or blocks within the same file. That's a separate
-- piece of metadata.
type Layout = NS Stream

data Stream = Val Range          -- ^ A primitive value, includes unit, bools, ints, chars, etc ...
              -- products
            | Struct (NS Stream) -- ^ A product type
              -- sums
            | Union (NS Stream)  -- ^ An *untagged* sum type
            | Tag Range          -- ^ A tag for one or more sum types
              -- lists
            | Array Stream       -- ^ An array items with unknown size
            | Length Range       -- ^ The length of one or more arrays
            deriving (Eq, Show)

-- | Apply a tranformation that looks like a splitter to a layout, splitting as far as we can go
fullyApply :: (Stream -> Maybe Layout) -> Layout -> Layout
fullyApply splitter layout =
  layout >>= \stream -> maybe stream (fullyApply splitter) (splitter stream)


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
splitStream (Struct items) = Some items
splitStream (Union items)  = Some items
splitStream (Array stream) = Some $ Array <$> splitStream stream
