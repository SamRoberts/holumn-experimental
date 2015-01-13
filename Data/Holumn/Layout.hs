module Data.Holumn.Layout where

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

-- at the moment, we are only interested in:
-- distributing namespace over children
-- creating children in streams
