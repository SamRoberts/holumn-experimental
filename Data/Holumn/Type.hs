module Data.Holumn.Type where

import Control.Applicative ((<*))
import Control.Monad.Trans.State (evalState, gets, modify)
import Data.Holumn.NameSpace (NS, flatten)


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
          deriving (Eq, Show)

data Range = Range Integer Integer deriving (Eq, Show)

-- | Holumns internal unsfe C-like representation of a type
--
-- My reason for this representation is that there are no implicit elements included in the
-- type. Everything: sum tags, array lengths, etc, are part of the type and can be rewritten.
-- Of course any rewrites must preserve the safety of the layout (where "safety" means we can
-- define valid readers and writers).
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
--
-- I'm wondering if one reason I've had so much trouble pinning down what I want to do with
-- Repr is that it doesn't clearly represent any particular thing. Maybe I should change it
-- to represent an abstract notion of layout of data on the file system. That would be:
--
-- Repr = NS Stream
-- Stream = all the constructors from current Repr, except Stream
--
-- But first I need to make NS more convienient to work with!
data Repr = Val Range     -- ^ A primitive value, includes unit, bools, ints, chars, etc ...
            -- products
          | Struct (NS Repr) -- ^ A product type
           -- sums
          | Union (NS Repr)  -- ^ An *untagged* sum type
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
--arrayStruct_structArray (Array (Struct xs)) = Struct $ map (Stream . Array) xs

--arrayUnion_structArray  (Array (Union xs))  = Struct $ map (Stream . Array) xs -- length of new arrays should sum to length of old array
--arrayArray_array        (Array (Array x))   = Array x               -- the concatenation of all elements, lengths will be handled elsewhere. this is actually a no-op because our arrays are just consecutive number of elements with no built-in metadata at the start or end of the structure


-- | describes a reader, I think. not sure if powerful enough
data Reader = Bits Range
              -- products
            | Sequence [(Id, Reader)]
              -- branching
            | Choice ChoiceId [(Id, Reader)] -- assumes we have saved an tag for this id
            | Decision [ChoiceId] [Id] -- the branch (id) to take in future choices
              -- looping
            | Loop LoopId Reader      -- assumes we have saved the number of times to repeat this parser
            | Counter [LoopId] Range  -- the number of times to repeat future loops
              -- different streams
            | Pop Id Reader       -- "pop" an item from a child stream and then continue reading
            deriving (Eq, Show)

-- | Represents a local id
--
-- A local id uniquely identifies items at the same place in some hierarchical structure.
-- It is not globally unique. As a result, whenever we change this stucture, we must take
-- care to ensure uniquess of each id at their new location in the new structure.
--
-- Local ids have two components, both locally unique and both referring to the same item.
-- One is a path, which may be useful for, for example, having a meaningful directory/file
-- naming scheme if splitting streams up into files. The other is a number in a string of
-- conseccutive ids starting at 0. These may be useful for coming up with ranges for tags and so forth.
data Id = Id { localId :: Integer
             , relPath :: [String]
             }
          deriving (Eq, Show)

addIds :: NS a -> [(Id, a)]
addIds = zipWith (\id (path, a) -> (Id id path, a)) [0..] . flatten

type ChoiceId = Integer
type LoopId   = Integer

-- instead of uniq and uniqs, maybe add NS back into Repr, and use unique names to drive unique ids for everything
--uniq = magical unique id generator in pure code
--uniqs = magical stream of unique id generator in pure code
--
-- rules for readers, focussing on the observation that we our transformations ultimately boil
-- down to adding Pop constructors in (changing how items are laid out in memory without changing how we read them)
--rdr_streamLoop_loopStream     (Pop m (Loop n x))    = Loop n   $ Pop m x              -- no-op, but allows other rules to fire
--rdr_streamStruct_structStream (Pop _ (Sequence xs)) = Sequence $ zipWith Pop uniqs xs -- changes memory layout
--rdr_streamUnion_unionStream   (Pop _ (Choice n xs)) = Choice   $ zipWith Pop uniqs xs -- changes memory layout
--
-- also, we want the ability to ignore the results of a read, and then recognize when we are ignoring the result of some Pop, and remove that read entirely
-- which isn't quite as straightforward as it sounds, as in order for this to be safe we must know that that Pop Id is unique ... hrmm ...

reader :: Type -> Reader
reader typ = evalState (go typ) (0,0)
  where
    -- need to learn zippers ...
    getLoopId   = gets fst <* modify (\(c,l) -> (c,l+1))
    getChoiceId = gets snd <* modify (\(c,l) -> (c+1,l))

    go (Prim range) =
      return $ Bits range

    go (Prod items) =
      return $ Sequence $ addIds $ fmap reader items

    go (Sum  items) =
      let choices = addIds $ fmap reader items
          ids     = map fst choices
      in do
        choiceId <- getChoiceId
        return $ Sequence [ (Id 0 ["decision"], Decision [choiceId] ids)
                          , (Id 1 ["choice"]  , Choice   choiceId   choices)
                          ]

    go (List range typ) =
      let rdr = reader typ
      in do
        loopId <- getLoopId
        return $ Sequence [ (Id 0 ["counter"], Counter [loopId] range)
                          , (Id 1 ["loop"]   , Loop    loopId   rdr)
                          ]
