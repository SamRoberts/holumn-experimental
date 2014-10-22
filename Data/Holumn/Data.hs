module Data.Holumn.Block where

import Data.Holumn.NameSpace (NS)
import Data.Holumn.Type (Type)
import Data.Holumn.Serial (Serial)

-- How this is going to be represented on disk?
-- We need to store the type of data stored on disk, and it's serialised format.
--
-- We (will) have conversions from return dataTyoe to some final NS Type.
-- At the moment, that final NS Type directly corresponds to the NS Serial.
-- We define a round trip from Type to Serial back to Type that is identity.
--
-- When the user queries, they specify conversions from source Type to returned
-- Type. For example, select fields in a struct, only look at rows with certain
-- types in an alternative, "ungroup" struct with list (turn (a, b, [c]) into
-- [(a, b, c)], etc. We need to be able to take dataConversions and modify the
-- conversions so that they apply to the returned Type.
--
-- Currently assuming that each serial has only one block of data, and all
-- blocks of data are stored in the same file.

newtype Pos = Offset Integer

data Data = Data
             { dataType        :: Type
             , dataConversions :: List[NS Type -> NS Type] -- TODO need some serialized format for these. Probably need to be Iso.
             , dataFormat      :: NS (Serial, Pos)         -- represents the actual schema of serialised bytes.
             , dataBytes       :: Vector Word8             -- represents actual payload of file
             }
