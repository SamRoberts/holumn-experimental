module Data.Holumn.Block where

import Data.Holumn.NameSpace (NS)
import Data.Holumn.Type (Type)
import Data.Holumn.Serial (Serial)

data Data = Data
             { dataType :: Type
             , dataBytes :: NS (Serial, Vector Word8)
             }
