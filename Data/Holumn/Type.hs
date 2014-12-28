module Data.Holumn.Type where

import Data.Holumn.NameSpace (NS)


-- So how is this all meant to fit together?
--
-- Language specific API
-- Library of standard types (map, list, ieee double, decimal to various precisions, unsigned long, 32 bit int, dword, byte, unit, constants, etc)
-- Type
-- Repr/Reader/Writer before transformations
-- Repr/Reader/Writer after transformations

-- | A type
--
-- Holumn does columnar storage by applying rewriting rules to the type,
-- having the One True Serial representation for each type, and having the
-- ability to split that type into separate streams which parsers can read from selectively.
--
-- As a general rule of thumb, using a columnar layout is applying rewrite rules
-- that take List (MyType of A's) to MyTypeAsStruct of List A's.
data Type = Prim Range      -- ^ A primitive set of values, includes unit, bools, ints, chars, etc ...
          | Prod (NS Type)  -- ^ A product type
          | Sum  (NS Type)  -- ^ An tagged sum type
          | List Range Type -- ^ A list with a given range of lengths

type Range = Range Integer Integer

-- | Holumns internal unsfe C-like representation of a type
--
-- My reason for this representation is that there are no implicit elements included in the
-- type. Everything: sum tags, array lengths, etc, are part of the type and can be rewritten.
-- Of course any rewrites must preserve the safety of the layout (where "safety" means we can
-- define valid readers and writers).
--
-- On the other hand, useful information for humans (field names, sum tag names, etc)
-- is unnecesary.
--
-- Three is no notion in the type of how to match up a tag with its corresponding union, or a
-- length with it's corresponding array. I am punting on these details until I get to readers
-- and writers. I hope that as I implement the corresponding reader and writer transformations,
-- I can figure this out.
--
-- I'm not yet sure how I want to handle streams placed inside an array. Lots of separate
-- streams? Or use the same stream?
--
-- This type deliberately has no notion of how the streams are implemented. They could be
-- separate files, or blocks within the same file. That's a separate piece of metadata.
data Repr = Val Range     -- ^ A primitive value, includes unit, bools, ints, chars, etc ...
            -- products
          | Struct [Repr] -- ^ A product type
           -- sums
          | Union [Repr]  -- ^ An *untagged* sum type
          | Tag Range     -- ^ A tag for one or more sum types
            -- lists
          | Array Repr    -- ^ An array items with unknown size
          | Length Range  -- ^ The length of one or more arrays
            -- streams
          | Stream Repr   -- ^ The contents of the child Repr occur in a separate stream

-- at the moment, we are only interested in:
-- distributing array over children
-- creating children in streams

-- distributes array over child struct
-- we must give each new child array it's own stream, so we can pop values of each of them one-by-one
-- this is kinda dissapointing because the whole reason to use c-like in the first place was to get elegant rewrite rules
-- each new child array has the same length as the original
arrayStruct_structArray (Array (Struct xs)) = Struct $ map (Stream . Array) xs

arrayUnion_structArray  (Array (Union xs))  = Struct $ map (Stream . Array) xs -- length of new arrays should sum to length of old array
arrayArray_array        (Array (Array x))   = Array x               -- the concatenation of all elements, lengths will be handled elsewhere. this is actually a no-op because our arrays are just consecutive number of elements with no built-in metadata at the start or end of the structure


-- | describes a reader, I think. not sure if powerful enough
data Reader = Bits Range
              -- products
            | Sequence [Reader]
              -- branching
            | Choice Id [Reader]  -- assumes we have saved an tag for this id
            | Decision [Id] Range -- the branch to take in future choices
              -- looping
            | Loop Id Reader      -- assumes we have saved the number of times to repeat this parser
            | Counter [Id] Range  -- the number of times to repeat future loops
              -- different streams
            | Pop Id Reader       -- "pop" an item from a child stream and then continue reading

-- instead of uniq and uniqs, maybe add NS back into Repr, and use unique names to drive unique ids for everything
uniq = magical unique id generator in pure code
uniqs = magical stream of unique id generator in pure code

-- rules for readers, focussing on the observation that we our transformations ultimately boil
-- down to adding Pop constructors in (changing how items are laid out in memory without changing how we read them)
rdr_streamLoop_loopStream     (Pop m (Loop n x))    = Loop n   $ Pop m x              -- no-op, but allows other rules to fire
rdr_streamStruct_structStream (Pop _ (Sequence xs)) = Sequence $ zipWith Pop uniqs xs -- changes memory layout
rdr_streamUnion_unionStream   (Pop _ (Choice n xs)) = Choice   $ zipWith Pop uniqs xs -- changes memory layout
