module Data.Holumn.Reader where

import Control.Applicative ((<$>), (<*))
import Control.Monad.Trans.State (evalState, gets, modify)
import Data.Holumn.NameSpace (NS, Flat, (@=), flat, flatten, range)
import Data.Holumn.Range (Range)
import qualified Data.Holumn.Type as T
import Data.Traversable (traverse)

-- | describes a reader, I think. not sure if powerful enough
data Reader = Bits Range
              -- products
            | Sequence (Flat Reader)
              -- branching
            | Choice ChoiceId (Flat Reader) -- assumes we have saved an tag for this id
            | Decision [ChoiceId] Range     -- the branch (id) to take in future choices
              -- looping
            | Loop LoopId Reader            -- assumes we have saved the number of times to repeat this parser
            | Counter [LoopId] Range        -- the number of times to repeat future loops
            --   -- different streams
            -- | Pop SomeKindOfId Reader       -- "pop" an item from a child stream and then continue reading
            deriving (Eq, Show)

type ChoiceId = Integer
type LoopId   = Integer

reader :: T.Type -> Reader
reader typ = evalState (go typ) (0,0)
  where
    -- need to learn zippers ...
    getLoopId   = gets snd <* modify (\(c,l) -> (c,l+1))
    getChoiceId = gets fst <* modify (\(c,l) -> (c+1,l))

    go (T.Prim range) =
      return $ Bits range

    go (T.Prod items) =
      (Sequence . flatten) <$> traverse go items

    go (T.Sum  items) = do
      choices  <- flatten <$> traverse go items
      choiceId <- getChoiceId
      return $ Sequence $ flat [ "decision" @= Decision [choiceId] (range choices)
                               , "choice"   @= Choice   choiceId   choices
                               ]

    go (T.List range typ) = do
      loopId <- getLoopId
      rdr    <- go typ
      return $ Sequence $ flat [ "counter" @= Counter [loopId] range
                               , "loop"    @= Loop    loopId   rdr
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
