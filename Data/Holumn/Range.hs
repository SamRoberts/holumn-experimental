module Data.Holumn.Range where

data Range = Range Integer Integer deriving (Eq, Show)

size :: Integer -> Range
size n = Range 0 $ n - 1

bits :: Integer -> Range
bits n = size $ 2^n

unit :: Range
unit = bits 0

bool :: Range
bool = bits 1

byte :: Range
byte = bits 8

uint32 :: Range
uint32 = bits 32

uint64 :: Range
uint64 = bits 64

-- what about a Num class? we might want to merge ranges together .. we'll see ...
