module Data.Holumn.Serial where

import Control.Applicative ((<|>), liftA2)
import Control.Monad (guard, mfilter)
import qualified Data.Holumn.Vals as V

-- | An open data type for a schema representing a sequence of bits
--
-- Open for now as I think I want to make it easy to insert extra info
-- e.g. conversion info, frequency info, etc.
--
-- The list of serials in Cat and Alt must be non-empty.
--
-- Currently using standard unsigned int format for all list lengths. Might try some variable width format in the future.
data SerialOpen a = Prim V.Vals    -- ^ A primitive with given range
                  | Cat [a]        -- ^ A sequence of serials. Represented as the items in sequence.
                  | Alt V.Vals [a] -- ^ A list of alternative types with given range of lengths. Represented as the number of items, followed by a block of control data, followed by the items
                  | List V.Vals a  -- ^ A list of one serial with given range of lengths. Represented as the number of items, followed by the items

-- | The actual serial type with no additional information
newtype Serial = Serial (SerialOpen Serial)

fold :: (SerialOpen a -> a) -> Serial -> a
fold f (Serial (Prim vals))   = f $ Prim      $ vals
fold f (Serial (Cat ss))      = f $ Cat       $ map (fold f) ss
fold f (Serial (Alt vals ss)) = f $ Alt vals  $ map (fold f) ss
fold f (Serial (List vals s)) = f $ List vals $ fold f s

fixedBits :: Serial -> Maybe Integer
fixedBits = fold f
  where
    f (Prim vals) =
      Just $ V.bits vals

    f (Cat sizes) =
      fmap sum $ sequence sizes

    f (Alt vals sizes) =
      let len = mfilter (V.low vals ==) $ Just (V.high vals)
          size = do
            ss <- sequence sizes
            guard $ all (head ss ==) (tail ss)
            return $ head ss
      in mfilter (0 ==) len
         <|> mfilter (0 ==) size
         <|> liftA2 (*) len size

    f (List vals size) =
      let len = mfilter (V.low vals ==) $ Just (V.high vals)
      in mfilter (0 ==) len
         <|> mfilter (0 ==) size
         <|> liftA2 (*) len size
