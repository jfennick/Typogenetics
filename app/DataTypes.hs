module DataTypes where

data BasePair = A | C | G | T deriving (Eq, Ord, Read, Show) -- Ord for Set
type Strand = [BasePair]

data Code = Cut | Delete | Switch
  | MoveRight | MoveLeft | CopyOn | CopyOff
  | InsertA | InsertC | InsertG | InsertT
  | FindPyRight | FindPuRight | FindPyLeft | FindPuLeft deriving (Eq, Ord, Read, Show)

--These two types are isomorphic to Bool, but we use them to avoid Boolean Blindness.
data SC = Stop | Continue deriving (Eq, Read, Show) -- shortcircuit
data CopyMode = On | Off deriving (Eq, Read, Show)

type ListZipper a = ([a],[a])
-- Represent an RNA Strand as a single ListZipper and a DNA Strand as two ListZippers.
-- (see http://learnyouahaskell.com/zippers for general info on zippers)
-- This will help when implementing move left/right, copy, cut, and in particular switch.
-- By convention, the current position is the head of the right list.
-- To avoid doubling the number of pattern matches, we will assume that each half is the same length, and use Nothing to represent blank spaces.
-- Thus, we consider DS (l1,[]) (l2,[]) and DS (l1,Nothing:r1) (l2,[y:r2) to be illegal states
-- and we do not allow any function to return these states (unless it is time to Stop).
-- Note that the left list of each zipper is "reversed", but that is precisely what we want.
type MBP = Maybe BasePair
data DS = DS (ListZipper MBP) (ListZipper MBP) deriving (Eq, Ord, Read, Show)
