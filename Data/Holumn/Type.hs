module Data.Holumn.Type where

import Data.Holumn.Binds (Binds, (@=), binds)
import qualified Data.Holumn.Binds as B
import Data.Holumn.NameSpace (NS)
import Data.Monoid ((<>))

-- | Structured types
--
-- Simple version with limited support for recursive types.
-- For a given data type in user land, would expect to have a conversion
-- from user type <-> Type <-> Serial.
--
-- Extra tag parameter for now, because I think I want to make it easy to insert extra
-- information (e.g. conversion info, frequencies, etc).
--
-- "Infinite" types restricted to lists for now. If you need trees, we should make
-- it easy to represent as a list of zipper movements. Also, they have a range of
-- possible lengths so they are not really infinite (16 bytes should be enough for anyone!).
-- Just running with this idea of length ranges for now and seeing what it looks like.
--
-- The base definition does not have such simple concepts as
-- chars, ints, maps, sets, etc ... These should be libraries.
data Type a = Range  a Integer Integer      -- ^ used for characters, ints, unit, etc
            | Struct a (Binds Type)         -- ^ products, structs, tuples, etc
            | Union  a (Binds Type)         -- ^ tagged unions, sum types, etc
            | List   a Integer Integer Type -- ^ lists with min and max length

data TypeF a b = RangeF  a Integer Integer
               | StructF a (Binds b)
               | UnionF  a (Binds b)
               | ListF   a Integer Integer b

fold :: (TypeF a b -> b) -> Type a -> b
fold f (Range m mn mx)  = f $ RangeF m mn mx
fold f (Struct m xs)    = f $ StructF m $ fmap (fold f) xs
fold f (Union m xs)     = f $ UnionF  m $ fmap (fold f) xs
fold f (List m mn mx x) = f $ ListF   m mn mx $ fold f x

-- | Assuming top type term is list, distribute it over list elem type, turning list elem type into cols
--
-- Not sure what to do with meta data yet, probably need particular type of meta data.
-- Make it unit for now
--
-- Note that we might not always want to distribute ... serial can handle everything,
-- Need some meta data info to help us decide. For now this function just assumes
-- we want to distribute over anything remotely possible and shows what that might look like.
-- Realistically, distributing a list over a list seems to have no value at all
-- compared to having lengths inline
--
-- Not sure how we'll want the artificial magic columns to look eventually ...
-- certainly have better names!
distributeList :: Type () -> NS (Type ())
distributeList (List _ mn mx (Struct _ xs)) =
  liftF $ "struct" @= fmap (List () mn mx) xs
distributeList (List _ mn mx (Union _ xs))  =
  liftF $ ("union" @= fmap (List () mn mx) xs) <> ("tag" @= (List () mn mx $ Range () 0 (B.size xs)))
distributeList (List _ mn mx (List _ mn2 mx2 x)) =
  liftF $ ("values" @= List () (mn*mn2) (mx*mx2) x) <> ("lengths" @= List () mn mx (Range () mn2 mx2))
distributeList (List _ mn mx (Range _ mn2 mx2)) =
  return $ List () mn mx (Range () mx2 mx2)

int :: Integer -> Type
int n = Type $ Range (-mid) (mid-1)
  where mid = 2^(n-1)

uint :: Integer -> Type
uint n = Type $ Range 0 (2^n - 1)

unit :: Type
unit = Type $ Range 0 0

byte :: Type
byte = uint 8

uint32 :: Type
uint32 = uint 32

uint64 :: Type
uint64 = uint 64

int32 :: Type
int32 = int 32

int64 :: Type
int64 = int 64

union :: [Binds Type] -> Type
union = Type . Union . binds

struct :: [Binds Type] -> Type
struct = Type . Struct . binds

list :: Type -> Type
list = Type . List

btree :: Type -> Type
btree = list . btreeCursor
  -- the idea here is that we represent a tree as a list of commands for building a tree
  -- a node is assumed to be finished when we go up from that node, no revisiting
  -- the list of commands finishes when we go up from root
  -- it is up to the person constructing this type to build a valid btree
  -- e.g. [ elem "root", left, elem "left", up, right, elem "right", right, elem "bottom", up, up, up ]
  -- as opposed to an invalid tree:
  -- e.g. [ elem "foo", left, elem "bar", up, left, elem "overwrite", up, up ]
  where btreeCursor a = union [ "elem"  @= a
                              , "right" @= unit
                              , "left"  @= unit
                              , "up"    @= unit ]
