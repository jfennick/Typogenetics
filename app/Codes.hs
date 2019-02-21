module Codes where

import qualified Data.Maybe as DM

import DataTypes
import Utility

cut :: DS -> (DS, SC)
cut (DS (l1,[]) (l2,[])) = error "already at right end cut"
cut (DS (l1,Nothing:r1) (l2,y:r2)) = error "current location contains Nothing"
cut (DS (l1,x:[]) (l2,y:[])) = (DS (l1,x:[]) (l2,y:[]), Stop) --if already at right end, nothing to cut
-- Implement cutting by inserting double Nothings, and then actually cut later in cutDS.
-- This allows all 15 Codes to have a uniform return type which makes it possible
-- to define transcribe using foldl.
cut (DS (l1,x:r1) (l2,y:r2)) = ((DS (l1,x:Nothing:r1) (l2,y:Nothing:r2)), Stop)

-- NOTE: Rather than simply delete and move right, we want to detect if deleting will
-- cause the strand to become effectively cut (and thus Stop). Need to look at 3 adjacent bases.
del :: DS -> (DS, SC)
del (DS (l1,[]) (l2,[])) = error "already at right end del"
del (DS (l1,Nothing:r1) (l2,y:r2)) = error "current location contains Nothing"
--consider lengths 1 and 2 first, so we can guarantee the pattern matches of length 3+ succeed
del (DS ([],[x]) ([],[y])) = (DS ([],Nothing:[]) ([],y:[]), Stop) -- delete, but cannot move right
del (DS ([],[x,x2]) ([],[y,y2])) = result where
  done = if (x2 == Nothing || y2 == Nothing) then Stop else Continue --next pos Nothing or diagonal
  -- Insert double Nothings after x and y not needed for length 2 cases
  result = (DS ([Nothing],[x2]) ([y],[y2]), done)
del (DS ([x1],[x]) ([y1], [y])) = (DS ([x1],[Nothing]) ([y1],[y]), Stop)-- delete, but cannot move right
--length 3+ left end. r1,r2 shouldn't be [] due to the above length 2 pattern matches
del (DS ([],x:Nothing:r1) ([], y:y2:r2)) = (DS ([],Nothing:Nothing:r1) ([],y:y2:r2), Stop) -- delete, but cannot move right
del (DS ([],x:x2:r1) ([], y:y2:r2)) = (DS ([Nothing],x2:r1) ([y],y2:r2), Continue)
--length 3+ middle.  Note l1,r1,l2,r2 may be [], but that is okay.
del (DS (x1:l1,x:x2:r1) (y1:l2,y:y2:r2)) = calcresult x1 l1 x x2 r1 y1 l2 y y2 r2 where
  calcresult x1 l1 x x2 r1 y1 l2 y y2 r2
    | (not(y==Nothing) && (y1==Nothing || y2==Nothing)) = -- L diagrams
      (DS (x1:l1,Nothing:Nothing:x2:r1) (y1:l2,y:Nothing:y2:r2), Stop) -- diagonal=CUT, skip move right
-- Insert double Nothings after x and y and cut later
    | (not (y==Nothing)) && (x2==Nothing) =               -- T diagram
      (DS (x1:l1,Nothing:x2:r1) (y1:l2,y:y2:r2), Stop) -- next pos Nothing, skip move right
-- The next case requires an assumption.  Do we simply delete x and concatenate the two double strands
-- , or does deleting x essentially amount to a cut?  Let's do the former for now, but due to the
-- guards (|), the latter can be easily accommodated.
    | (all (not . (== Nothing)) [x1,y1,x2,y2] ) && (y == Nothing) =
      (DS (x1:l1,x2:r1) (y1:l2,y2:r2), Continue) -- delete x, concatenate, and move right
    | (x2==Nothing) = (DS (x1:l1,Nothing:x2:r1) (y1:l2,y:y2:r2), Stop) -- skip move right
    | otherwise = (DS (x1:l1,x2:r1) (y1:l2,y2:r2), Continue) -- delete x, concatenate, and move right
--length 3+ right end.
del (DS (l1,[x]) (l2, [y])) = (DS (l1,[Nothing]) (l2,[y]), Stop)--TODO: doublecheck this pattern match
del ds = error $ "nonexhaustive patterns in del " ++ show ds

copy :: DS -> (DS, SC)
copy (DS (l1,[]) (l2,[])) = error "already at right end copy"
copy (DS (l1,Nothing:r1) (l2,y:r2)) = error "current location contains Nothing"
copy (DS (l1,x:r1) (l2,Nothing:r2)) = (DS (l1,x:r1) (l2,comp:r2), Continue) where
  complement :: BasePair -> BasePair
  complement A = T
  complement C = G
  complement G = C
  complement T = A
  comp = Just $ complement $ DM.fromJust x
copy (DS (l1,x:r1) (l2,y:r2)) = (DS (l1,x:r1) (l2,y:r2), Continue) -- if base already present, nothing to copy

moveRight :: DS -> (DS, SC)
moveRight (DS (l1,[]) (l2,[])) = error "already at right end moveR"
moveRight (DS (l1,Nothing:r1) (l2,r2)) = error "current location contains Nothing"
moveRight (DS (l1,x:Nothing:r1) (l2,r2)) = (DS (l1, x:Nothing:r1) (l2,r2), Stop)
moveRight (DS (l1,x:[]) (l2,r2)) = (DS (l1, x:[]) (l2,r2), Stop)
-- cannot move to a location that contains Nothing or is [], so do nothing and Stop
moveRight (DS (l1,x:r1) (l2,y:r2)) = (DS (x:l1, r1) (y:l2,r2), Continue)

moveLeft :: DS -> (DS, SC)
moveLeft (DS (l1,[]) (l2,[])) = error "already at right end moveL"
moveLeft (DS ([],r1) ([],r2)) = ((DS ([],r1) ([],r2)), Stop) -- if already at left end, do nothing
moveLeft (DS (Nothing:l1,r1) (y:l2,r2)) = (DS (Nothing:l1,r1) (y:l2,r2),Stop)
-- cannot move to a location that contains Nothing, so do nothing and Stop
moveLeft (DS (x:l1,r1) (y:l2,r2)) = (DS (l1, x:r1) (l2,y:r2), Continue)

switch :: DS -> (DS, SC)
switch (DS (l1,[]) (l2,[])) = error "already at right end switch"
switch (DS (l1,Nothing:r1) (l2,y:r2)) = error "current location contains Nothing"
switch (DS (l1,x:r1) (l2,Nothing:r2)) = ((DS (l1,x:r1) (l2,Nothing:r2)), Stop)
switch (DS (l1,x:r1) (l2,y:r2)) = (DS (r2,y:l2) (r1,x:l1), Continue)
--Note: Due to our use of zippers, we simply need to exchange l1 & r2, and l2 & r1!
-- (and move left due to our current position convention.) Do not use moveLeft here
-- i.e. (moveLeft $ DS (y:r2,l2) (x:r1,l1), Continue) because when r1 & r2 == []
-- the intermediate state is illegal.

insert :: MBP -> DS -> (DS, SC) --(DS (l1,[maybebase]) (l2,[Nothing]), Continue)
insert maybebase (DS (l1,[]) (l2,[])) = error "already at right end insert"
insert maybebase (DS (l1,x:r1) (l2,y:r2)) = (DS (x:l1,maybebase:r1) (y:l2,Nothing:r2), Continue)

copyfn :: (CopyMode, (DS, SC)) -> (CopyMode, (DS, SC))
copyfn (mode, (ds, mb)) = if (mode == On) then (mode, (fst $ copy ds, mb)) else (mode, (ds, mb))

search :: (DS -> (DS, SC)) -> (BasePair -> Bool) -> CopyMode -> DS -> (DS, SC)
search movefn pred mode (DS (l1,r1) (l2,r2)) = result where
  fst3 (a,b,c) = a
  bothCont (a,b,c) = (b == Continue) && (c == Continue)
  --x:r1 pattern match should not fail due to moveLeft/Right definition
  predfn (DS (l1,x:r1) (l2,r2), movesc) = (DS (l1,x:r1) (l2,r2), movesc, predsc) where
    predsc = if (pred $ DM.fromJust x) then Stop else Continue
  moves = iterate (predfn . snd . (\x -> copyfn (mode, x)) . movefn . fst3)
                  (DS (l1,r1) (l2,r2), Continue, Continue)
  (ds,mb,pb) = last $ takeWhileInclusive bothCont moves
  result = (ds,if (mb == Continue) then Continue else Stop)
  --Stop if end of strand reached without finding base

pyrimidinepred :: BasePair -> Bool
pyrimidinepred T = True
pyrimidinepred C = True
pyrimidinepred A = False
pyrimidinepred G = False

purinepred :: BasePair -> Bool
purinepred A = True
purinepred G = True
purinepred T = False
purinepred C = False

searchPyRight :: CopyMode -> DS -> (DS, SC)
searchPyRight = search moveRight pyrimidinepred
searchPuRight :: CopyMode -> DS -> (DS, SC)
searchPuRight = search moveRight purinepred
searchPyLeft :: CopyMode -> DS -> (DS, SC)
searchPyLeft = search moveLeft pyrimidinepred
searchPuLeft :: CopyMode -> DS -> (DS, SC)
searchPuLeft = search moveLeft purinepred
