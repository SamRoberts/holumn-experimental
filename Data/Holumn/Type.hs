module Data.Holumn.Type where

import Data.Holumn.NameSpace (NS)
import Data.Holumn.Range (Range)

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
