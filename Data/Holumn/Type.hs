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
