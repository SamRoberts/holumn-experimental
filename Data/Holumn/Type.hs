module Data.Holumn.Type where

import Data.Holumn.NameSpace (NS)

-- | A type
--
-- Holumn does columnar storage by applying rewriting rules to the type,
-- having the One True Serial representation for each type, and having the
-- ability to record offsets into that serial representation and split over files.
--
-- As a general rule of thumb, using a columnar layout is applying rewrite rules
-- that take List (MyType of A's) to MyTypeAsStruct of List A's.
data Type = Prim Min Max      -- ^ A primitive set of values, includes unit, bools, ints, chars, etc ...
          | Prod (NS Type)    -- ^ A product type
          | Sum  (NS Type)    -- ^ An tagged sum type
          | List Min Max Type -- ^ A list with a given range of lengths

type Range = Range Integer Integer | LazyULong

-- Do we need Repr? Why not Writer and Reader pair?
data Repr m = Val m Range         -- ^ A primitive value, includes unit, bools, ints, chars, etc ...
              -- products
            | Struct m [Repr m]   -- ^ A product type
              -- sums
            | Union m Id [Repr m] -- ^ An *untagged* sum type
            | Tag m Id Range      -- ^ A tag for a sum type
              -- lists
            | Array m Id (Repr m) -- ^ An array items with unknown size
            | Length m [Id] Range -- ^ The length of one or more arrays

data Reader m = Bits m Range
                -- products
              | Sequence m [Reader m]
                -- branching
              | Choice m Id [Reader m]  -- assumes we have saved an tag for this id
              | Decision m Id Range     -- the branch to take in a future choice
                -- looping
              | Loop m Id (Reader m)    -- assumes we have saved the number of times to repeat this parser
              | Counter m Id Range      -- the number of times to repeat a future loop, and where the array starts
                -- different streams
              | Offset m Id Range Range -- record the offset and size of a child stream
              | Pop m Id (Reader m)     -- "pop" an item from a child stream and then continue reading

-- is it possible that we can use same type for readers and writer?
-- one consumes stream of data, one produces stream of data, after all!
-- writing is going to be very inefficient! But then, maybe it is possible to do transforms on the type which preserve all data and transform the writer to be more efficient too?
-- one difference is that we may leave space for a counter or offset, than do some looping, then go back and fill in space with the number of times we looped or the number of bytes we used
-- this is getting a little more arbitrary than I had intended ...
-- do we ever write after a child stream? or do we always have (root stream, child1 stream, child2 stream, nothing after last child stream)
data Writer m = WrBits m Range
                -- products
              | WrSequence m [Writer m]
                -- branching
              | WrChoice m Id [Writer m] -- assumes we have writen the decision for this branch, and so we know which branch we are writing now
              | WrDecision m Id Range    -- writes down the branch to take in a future choice
                -- looping
              | WrLoop m Id (Writer m)   -- repeats the given reader a number of times, assumes we have written down the number of times earlier
              | WrCounter m Id Range     -- writes down the number of times to repeat a future loop (BUT WHAT IF WE DON'T KNOW UNTIL AFTER LOOPING?)
                -- different streams
              | WrOffset m Id Range Rante -- record the offset and size of a child stream (BUT WHAT IF WE DON't KNOW YET?)
              | WrPush m Id (Writer m)    -- "push" an item onto a child stream and then continue writing (WHAT HAPPENS IF WE RUN INTO A CHILD STREAM? SKIP SPACE? ERROR?)




List 0 255 (Prod ["foo" := Prim 0 255, "bar" := Prim 0 1])

Struct [Length [0] 0 255, Array 0 (Struct [Val 0 255, Val 0 1])]
Sequence [Counter 0 0 255, Loop 0 (Sequence [Bits 0 255, Bits 0 1])]

Struct [Length [0,1] 0 255, Array 0 (Val 0 255), Array 1 (Val 0 1)]
Sequence [Counter 0 0 255, Offset 0 0 0, Offset 1 0 255, Loop 0 (Sequence [Pop 0 (Bits 0 255), Pop 1 (Bits 0 1)])]





-- rewrites (meta variable assumed to be unit and not shown)
--
-- Struct [Length 0 0 100, Array 0 (Struct [Val 0 7, Val 0 127])]  ||   Sequence [Counter 0 0 100, Loop 0 (Sequence [Bits 0 7, Bits 0 127])]
-- Struct [Length 0 0 100, Array 0 (Val 0 7), Array 0 (Val 0 127)] ||   Sequence [Length 0 0 100, Loop 0 (Sequence [Stream 1 (Bits 0 7), Stream 2 (Bits 0 127)])] // if on top level ...

type Min = Integer
type Max = Integer
type Id  = Integer
type NodeId = Integer

meta :: Repr a -> a
meta (Val a _ _)      = a
meta (Tag a _ _)      = a
meta (Length a _ _ _) = a
meta (Struct a _)     = a
meta (Union a _ _)    = a
meta (Array a _ _)    = a

repr :: Type -> Repr ()
repr = run 0 . go
  where go (Prim min max) = return $ Val () min max
        go (Prod ts)      = Struct () <$> mapM go (list ts)
        go (Sum ts)       = do
                              id <- getId
                              rs <- mapM go (list ts)
                              return $ Struct () [Tag () id (length (list ts)), Union () id rs]
        go (List min max ts) = do
                              id <- getId
                              rs <- go ts
                              return $ Struct () [Length () id min max, Array () id rs]

label :: Repr () -> Repr NodeId
label = run 0 . go
  where go (Val () min max)       = Val    <$> getId <*> pure min <*> pure max
        go (Tag () id len)        = Tag    <$> getId <*> pure id  <*> pure len
        go (Length () id min max) = Length <$> getId <*> pure id  <*> pure min <*> pure max
        go (Struct () rs)         = Struct <$> getId <*> mapM go rs
        go (Union () id rs)       = Union  <$> getId <*> pure id  <*> mapM go rs
        go (List () min max r)    = List   <$> getId <*> pure min <*> pure max <*> go r

newtype State s a = State (s -> (s, a))

run :: s -> State s a -> a
run s (State r) = snd . r s

getId :: State Integer Integer
getId = State (\s -> (s+1, s))

instance Functor (State s) where
  fmap f (State r) = State ((\(s, a) -> (s, f a)) . r)

instance Monad (State s) where
  return a = State (\s -> (s, a))
  State ra >>= fsb =
    State (\s1 -> let (s2, a)  = ra s1
                      State rb = fsb a
                      (s3, b)  = rb s2
                  in  (s3, b) )

instance Applicative (State s) where
  pure = return
  (<*>) = ap
