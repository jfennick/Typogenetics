module TertiaryStructure where

import DataTypes

data RelativeDirection = Straight | RelLeft | RelRight-- deriving (Eq, Read, Show)
data AbsoluteDirection = AbsUp | AbsDown | AbsLeft | AbsRight-- deriving (Eq, Read, Show)

getCode :: (BasePair, BasePair) -> (Code, RelativeDirection)
getCode (A, A) = undefined
getCode (A, C) = (Cut, Straight)
getCode (A, G) = (Delete, Straight)
getCode (A, T) = (Switch, RelRight)
getCode (C, A) = (MoveRight, Straight)
getCode (C, C) = (MoveLeft, Straight)
getCode (C, G) = (CopyOn, RelRight)
getCode (C, T) = (CopyOff, RelLeft)
getCode (G, A) = (InsertA, Straight)
getCode (G, C) = (InsertC, RelRight)
getCode (G, G) = (InsertG, RelRight)
getCode (G, T) = (InsertT, RelLeft)
getCode (T, A) = (FindPyRight, RelRight)
getCode (T, C) = (FindPuRight, RelLeft)
getCode (T, G) = (FindPyLeft, RelLeft)
getCode (T, T) = (FindPuLeft, RelLeft)

getRelDir :: Code -> RelativeDirection
getRelDir Cut = Straight
getRelDir Delete = Straight
getRelDir Switch = RelRight
getRelDir MoveRight = Straight
getRelDir MoveLeft = Straight
getRelDir CopyOn = RelRight
getRelDir CopyOff = RelLeft
getRelDir InsertA = Straight
getRelDir InsertC = RelRight
getRelDir InsertG = RelRight
getRelDir InsertT = RelLeft
getRelDir FindPyRight = RelRight
getRelDir FindPuRight = RelLeft
getRelDir FindPyLeft = RelLeft
getRelDir FindPuLeft = RelLeft

foldDirection :: AbsoluteDirection -> RelativeDirection -> AbsoluteDirection
foldDirection AbsUp   RelLeft  = AbsLeft
foldDirection AbsUp   RelRight = AbsRight
foldDirection AbsDown RelLeft  = AbsRight
foldDirection AbsDown RelRight = AbsLeft
foldDirection AbsLeft RelLeft  = AbsDown
foldDirection AbsLeft RelRight = AbsUp
foldDirection AbsRight RelLeft  = AbsUp
foldDirection AbsRight RelRight = AbsDown
foldDirection absdir Straight = absdir

getInitialDirection :: RelativeDirection -> AbsoluteDirection
getInitialDirection Straight = AbsRight
getInitialDirection RelLeft  = AbsDown
getInitialDirection RelRight = AbsUp

getTertiaryStructure :: [Code] -> [AbsoluteDirection]
getTertiaryStructure [] = []
-- AbsoluteDirection of one code is undefined, so choose AbsRight arbitrarily
getTertiaryStructure [code] = [AbsRight]
getTertiaryStructure (code:codes) = result where
  initDir = getInitialDirection $ getRelDir code
  tertstr = scanl foldDirection initDir $ map getRelDir (code:codes)
--Remove first and last AbsoluteDirection. init and tail are safe here due to 2nd pattern match
  result = init $ tail $ tertstr

getBindingLetter :: AbsoluteDirection -> BasePair
-- Argument is the absolute direction of the last segment of the tertiary structure
getBindingLetter AbsRight = A
getBindingLetter AbsUp    = C
getBindingLetter AbsDown  = G
getBindingLetter AbsLeft  = T
