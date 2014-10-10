module Data.Holumn.Vals where

-- | a range of number values with fixed bit size
data Vals = Range Integer Integer

low :: Vals -> Integer
low (Range l _) = l

high :: Vals -> Integer
high (Range _ h) = h

numVals :: Vals -> Integer
numVals (Range l h) = h - l + 1

bits :: Vals -> Integer
bits = ceiling . logBase 2.0 . numValsDbl
  where numValsDbl :: Vals -> Double
        numValsDbl = fromIntegral . numVals

(<->) :: Integer -> Integer -> Vals
l <-> h = if 0 <= l && l <= h
          then Range l h
          else error $ "cannot create a range from " ++ show l ++ " to " ++ show h

num :: Integer -> Vals
num n = 0 <-> (n-1)

constant :: Integer -> Vals
constant n = n <-> n

unit :: Vals
unit  = constant 0

uint :: Integer -> Vals
uint = num . (2^)

int :: Integer -> Vals
int n = let mag = 2^(n-1) in (-mag) <-> (mag-1)

uint8 :: Vals
uint8 = uint 8

uint16 :: Vals
uint16 = uint 16

uint32 :: Vals
uint32 = uint 32

uint64 :: Vals
uint64 = uint 64

int32 :: Vals
int32 = int 32

int64 :: Vals
int64 = int 64
