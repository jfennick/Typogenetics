module Main where

--from the 'base' package (preinstalled with the compiler)
import qualified Data.List as DL
import qualified Data.Maybe as DM
import qualified Control.Monad as CM

--from the 'containers' package http://hackage.haskell.org/package/containers-0.6.0.1
import qualified Data.Set as DS
import qualified Data.Tree as DT

--from the 'combinat' package http://hackage.haskell.org/package/combinat-0.2.9.0
import qualified Math.Combinat.Sets as CS
import qualified Math.Combinat.Tuples as CT

import Codes
import DataTypes
import TertiaryStructure
import Utility

main :: IO ()
main = CM.forM_ [1..10] $ (\i -> do
         putStrLn $ "Search depth " ++ show i
         let checkfn diag = CM.zipWithM_ (CM.zipWithM_ checkLevelsForSuccess) (allStrands ()) diag
         checkfn $ take i $ takeGenerations 4 $ map (map DT.levels) $ typoGeneticsTreeAll ())

--Note the IO () type signature: This is the only impure function (besides main).
checkLevelsForSuccess :: Strand -> [[Gen]] -> IO ()
checkLevelsForSuccess strand levels =
  CM.forM_ levels $ (\level ->
  CM.forM_ level  $ (\gen -> do
    let ((strands, sc), prevgens) = gen
    if (sc == Success)
      then do
        putStrLn $ "Success! "
        putStrLn $ "Strand " ++ show strand
        putStrLn $ "Previous generations "
        putStrLn $ show prevgens
      else return ()))

-- This is an infinite list whose element at index n is a list of all Strands of length n
-- i.e. take 2 $ allStrands () = [ [[A],[C],[G],[T]],
-- [[A,A],[C,A],[G,A],[T,A],[A,C],[C,C],[G,C],[T,C],[A,G],[C,G],[G,G],[T,G],[A,T],[C,T],[G,T],[T,T]] ]
allStrands :: () -> [[Strand]]
allStrands () = tail $ iterate prependBases [[]]  where
    prependBases :: [Strand] -> [Strand]
    prependBases strands = concat [map (: str) [A,C,G,T] | str <- strands] --each str is finite

-- Initialize the top strand with Nothings so that the lengths are equal.
makeDblStrand :: Strand -> DS
makeDblStrand botstrand = DS ([], map Just botstrand) ([], replicate (length botstrand) Nothing)

-- This function allows us to define transcribe using foldl
transcribeCode :: (CopyMode, (DS, SC)) -> Code -> (CopyMode, (DS, SC))
transcribeCode (mode, (ds, _)) Cut         = (mode, cut ds)
transcribeCode (mode, (ds, _)) Delete      = (mode, del ds)
transcribeCode (mode, (ds, _)) Switch      = (mode, switch ds)
transcribeCode (mode, (ds, _)) MoveRight   = copyfn (mode, moveRight ds)
transcribeCode (mode, (ds, _)) MoveLeft    = copyfn (mode, moveLeft ds)
transcribeCode (mode, (ds, _)) CopyOn      = copyfn (On , (ds, Continue))
transcribeCode (mode, (ds, _)) CopyOff     = (Off, (ds, Continue))
transcribeCode (mode, (ds, _)) InsertA     = copyfn (mode, (insert (Just A) ds))
transcribeCode (mode, (ds, _)) InsertC     = copyfn (mode, (insert (Just C) ds))
transcribeCode (mode, (ds, _)) InsertG     = copyfn (mode, (insert (Just G) ds))
transcribeCode (mode, (ds, _)) InsertT     = copyfn (mode, (insert (Just T) ds))
transcribeCode (mode, (ds, _)) FindPyRight = (mode, searchPyRight mode ds)--copyfn called in search
transcribeCode (mode, (ds, _)) FindPuRight = (mode, searchPuRight mode ds)--copyfn called in search
transcribeCode (mode, (ds, _)) FindPyLeft  = (mode, searchPyLeft mode ds)--copyfn called in search
transcribeCode (mode, (ds, _)) FindPuLeft  = (mode, searchPuLeft mode ds)--copyfn called in search

moveToBindingSite :: DS -> Int -> DS
moveToBindingSite ds n
  | (n < 0) = error $ "Invalid binding index " ++ show n
  | otherwise = fst . head $ drop n $ iterate (moveRight . fst) (ds, Continue)

--Transcribe for all possible binding sites.
transcribe :: Strand -> [Code] -> [DS]
transcribe strand codes = results where
  ds = makeDblStrand strand
  base = getBindingLetter $ last $ getTertiaryStructure codes
  indices = DL.elemIndices base strand
  initialpositions = map (moveToBindingSite ds) indices
  shortcircuitfn (mode, (ds, sc)) code =
    if (sc == Stop) then (mode, (ds, sc)) else transcribeCode (mode, (ds, sc)) code
  results = map (\pos -> fst $ snd $ foldl shortcircuitfn (Off, (pos, Continue)) codes)
                initialpositions

-- transcribes strand starting at each binding site of the enzyme represented by codes on strand
-- each element of list is from a different binding site, but each binding site may yield a list of strands
transcribeAllSites :: Strand -> [Code] -> [[Strand]]
transcribeAllSites strand codes = map (cutDS . unzipDS) $ transcribe strand codes

transcribeSet :: Strand -> [Code] -> DS.Set Strand
transcribeSet strand codes = DS.fromList $ concat $ map (cutDS . unzipDS) $ transcribe strand codes

isAA :: (BasePair, BasePair) -> Bool
isAA (A, A) = True
isAA (_, _) = False

translate :: Strand -> [[Code]]
translate strand = result where --Split a strand into genes when AA
  listlistpairs = filter (not . (== [])) $ splitByPred isAA $ listToPairs strand
  codefn listofpairs = map (fst . getCode) listofpairs
  result = map codefn listlistpairs

-- For nontermination heuristics
-- In evolve, we only want to transcribe some strand with a *single* gene from another strand. To help,
-- this function flattens the codes from all genes in a set of strands into a single set.
-- It is also used for nontermination heuristics
translateAllGenes :: DS.Set Strand -> DS.Set [Code]
translateAllGenes strands = DS.fromList $ concat $ map translate $ DS.toList strands

translateAll :: DS.Set Strand -> DS.Set Code
translateAll strands = DS.fromList $ concat $ map (concat . translate) $ DS.toList strands

unzipDS :: DS -> ([MBP],[MBP])
unzipDS ds = (r1,r2) where
  moveFarLeft :: (DS, Bool) -> (DS, Bool) -- like moveLeft, but ignores Nothing
  moveFarLeft ((DS ([],r1) ([],r2)),_) = (DS ([],r1) ([],r2), False) -- if already at left end, do nothing
  moveFarLeft ((DS (x:l1,r1) (y:l2,r2)), _) = (DS (l1, x:r1) (l2,y:r2), True)
  (DS (_,r1) (_,r2), _) = last $ takeWhileInclusive snd $ iterate moveFarLeft (ds, True)

-- This function actually performs the cutting (which we deferred in the cut function).
cutDS :: ([MBP],[MBP]) -> [Strand]
cutDS (bot,top) = result where
  listlistpairs :: [[(MBP, MBP)]]
  listlistpairs = filter (not . (== [])) $ splitByPred (== (Nothing,Nothing)) $ zip bot top
  revtop (bot,top) = [bot, reverse top]
  maybes :: [[MBP]]
  maybes = concat $ map revtop $ map unzip listlistpairs
  removeN strand = map DM.fromJust $ filter (not . (== Nothing)) strand
  result = filter (not . (== [])) $ map removeN maybes -- filter (not . (== [])) necessary?

--Need at least 3 choices here because if not KeepGoing, we need to know whether Success or not
data GSC = Fail | KeepGoing | Success | NonTerm deriving (Eq, Read, Show)

type Gen = ((DS.Set Strand, GSC),  -- Current generation and shortcircuit
            DS.Set (DS.Set Strand))-- All previous generations

-- This function has been superceded by evolveTree.  See comments below.
evolveList :: Strand -> Gen -> Gen
evolveList origstrand ((strands, sc), prevgens) =
  if (strands == DS.empty) then ((strands, Fail), prevgens) else (result, newgens) where
-- Note: Taking unions in gen12 loses binding site info!  This can cause some of the nontermination
-- heuristics to fail. (i.e. on input [A,G,A])  Also, if strand2 codes for more than one gene,
-- we are incorrectly transcribing strand1 by more than one gene. evolveTree solves both of these issues.
  gens = [let gen12 = DS.unions [transcribeSet strand1 code | code <- translate strand2]
-- check done here (rather than after taking unions in result) because otherwise, if origstrand
-- is in strands, it would be impossible to determine if a new copy is made
              done = if (not (strand1 == origstrand) && DS.member origstrand gen12)
                     then Success else KeepGoing --DS.member is O(log n)
          in (gen12, done)
         | strand1 <- DS.toList strands, strand2 <- DS.toList strands]
  newstrands = DS.unions $ map fst gens
  allcodes = translateAll newstrands
--nonterm via size and codes, i.e. each str in newstrands >= origstrand and insert but no delete or cut
  sizenonterm = (DS.filter (\str-> length str <= length origstrand) newstrands == DS.empty)
                && (any (\icode -> DS.member icode allcodes) [InsertA, InsertT, InsertG, InsertC])
                && (DS.notMember Delete allcodes) && (DS.notMember Cut allcodes)
  newgens = DS.insert newstrands prevgens
  result = (newstrands, if (any (== Success) $ map snd gens)
                        then Success else --try some heuristics to detect nontermination
                          if ((newstrands == strands)-- via idempotency with immediate predecessor.
                             || (DS.member newstrands prevgens)
-- We can check for nontermination cycles of arbitrarily large finite length by simply testing for membership
-- This will require much more memory, but will eliminate a large class of nontermination.
                             || sizenonterm)
                          then NonTerm else KeepGoing)

typoGeneticsList :: Strand -> [(DS.Set Strand, GSC)]
typoGeneticsList strand = map fst $ takeWhileInclusive ((== KeepGoing) . snd . fst)--Did we fail or succeed?
                          $ iterate (evolveList strand) ((DS.fromList [strand], KeepGoing), DS.empty)

typoGeneticsListAll :: () -> [[[(DS.Set Strand, GSC)]]]
typoGeneticsListAll () = map (map typoGeneticsList) $ allStrands ()

--For use with unfoldTree :: (b -> (a, [b])) -> b -> Tree a      where a=b=Gen
evolveTree :: Strand -> Gen -> (Gen, [Gen])
evolveTree origstrand ((strands, sc), prevgens) =
{- The following code is a bit tricky.  We want to perform all possible transcriptions without losing any
information. Simply flattening the lists (as in evolveList) loses information, so we use CT.tuples'
to get indices to the data we want before flattening at the end. Note that the use of bindings here is
likely causing a space leak (allcombos and allgens in particular).-}
  let
  allcodesthisgen :: DS.Set Strand -> DS.Set [Code]
  allcodesthisgen strands = translateAllGenes strands
  
  codecombos :: DS.Set [Code] -> [[[Code]]] --each listofenzymes in codecombos is of length (length strands)
  codecombos allcodesthisgen = CS.choose (length strands) $ DS.toList allcodesthisgen

  allcombos :: [[[Code]]] -> [[[[Strand]]]] --enzymecombo -> strand -> binding site -> list of strands
  allcombos codecombos = [zipWith (\strand enzyme -> transcribeAllSites strand enzyme)
                                  (DS.toList strands) listofenzymes
                         | listofenzymes <- codecombos]
                         
  allcombos' strands = allcombos $ codecombos $ allcodesthisgen $ strands
               
  -- CT.tuples' :: [Int] -> [[Int]]
  -- i.e. tuples' [2,3] = [[0,0],[0,1],[0,2],[0,3],[1,0],[1,1],[1,2],[1,3],[2,0],[2,1],[2,2],[2,3]]
  allindices :: [[[[Strand]]]] -> [[[Int]]] --enzymecombo -> choice of binding site -> indices
  allindices allcombos = map (CT.tuples' . (map (\x -> length x - 1))) allcombos

  getSites :: [Int] -> [[[a]]] -> [a]
  getSites indices bindingsites = concat $ zipWith (!!) bindingsites indices

                                         --enzymecombo -> combo of strand & binding site -> list of strands
  allgens :: [[[Int]]] -> [[[[Strand]]]] -> [[[Strand]]]
  allgens allindices allcombos = zipWith (\lstindices sitecombo ->
             {-no concat!-} map (\indices -> getSites indices sitecombo) lstindices) allindices allcombos

  donefn :: [Strand] -> Gen
  donefn gen =  let madecopy = (>1) $ length $ filter (==origstrand) gen
                    genset = DS.fromList gen
                    done = if madecopy then Success
                           else --try some heuristics to detect nontermination
                             if ((genset == strands)-- via idempotency with immediate predecessor.
                                || (DS.member genset prevgens)
-- We can check for nontermination cycles of arbitrarily large finite length by simply testing for membership
-- This will require much more memory, but will eliminate a large class of nontermination.
                                || sizenonterm)
                             then NonTerm else KeepGoing
                    allgenes = DS.fromList $ concat $ concat $ map translate $ DS.toList strands
                    allgenesnextgen = DS.fromList $ concat $ concat $ map translate gen
 --nonterm via size and codes, i.e. each str in newstrands >= origstrand and insert but no delete or cut
                    sizenonterm = (DS.filter (\str-> length str <= length origstrand) genset == DS.empty)
                      && (any (\icode -> DS.member icode allgenesnextgen) [InsertA, InsertT, InsertG, InsertC])
                      && (DS.notMember Delete allgenes) && (DS.notMember Cut allgenes)
                    newgens = DS.insert genset prevgens
                in ((genset, done), newgens)
--Only flatten at the very end
  in if (strands == DS.empty || sc == Fail)
       then (((strands, Fail), prevgens), [])
       else (((strands, sc), prevgens),
    [donefn gensite | genenz <- allgens (allindices $ allcombos' strands) (allcombos' strands)
                           , gensite <- genenz]) --allcombos' evaluated twice, but no space leak

--These functions build an infinite tree of all possible combinations using breadth first search.
allStrandsTree :: () -> DT.Tree Strand
allStrandsTree () = DT.unfoldTree (\strand -> (strand, map (: strand) [A,C,G,T])) []
-- i.e. take 3 $ DT.levels $ treeStrands = [[[]],[[A],[C],[G],[T]],
-- [[A,A],[C,A],[G,A],[T,A],[A,C],[C,C],[G,C],[T,C],[A,G],[C,G],[G,G],[T,G],[A,T],[C,T],[G,T],[T,T]]]

typoGeneticsTree :: Strand -> DT.Tree Gen
typoGeneticsTree strand = DT.unfoldTree (evolveTree strand) ((DS.fromList [strand], KeepGoing), DS.empty)

typoGeneticsTreeAll :: () -> [[DT.Tree Gen]]
typoGeneticsTreeAll () = map (map typoGeneticsTree) $ allStrands ()

pg510Strand = [T,A,G,A,T,C,C,A,G,T,C,C,A,C,A,T,C,G,A]
pg510Codes = translate pg510Strand
pg510TertStructs = map getTertiaryStructure pg510Codes
pg510BindingLetters = map (getBindingLetter . last) pg510TertStructs
--Note: This is pg508Codes!
--[FindPyRight,InsertA,FindPuRight,MoveRight,InsertT,MoveLeft,Cut,Switch,CopyOn]

pg508Strand = [T,A,G,A,T,C,C,A,G,T,C,C,A,T,C,G,A]
pg508Codes = translate pg508Strand
pg508TertStructs = map getTertiaryStructure pg508Codes
pg508BindingLetters = map (getBindingLetter . last) pg508TertStructs
--[FindPuRight, InsertC, CopyOn, MoveRight, MoveLeft, Switch, FindPuLeft, InsertT]

pg506Strand = [C,A,A,A,G,A,G,A,A,T,C,C,T,C,T,T,T,G,A,T]
pg506Codes = translate pg506Strand
pg506TertStructs = map getTertiaryStructure pg506Codes
pg506BindingLetters = map (getBindingLetter . last) pg506TertStructs
--[FindPyRight, CopyOn, FindPuRight, Cut]

pg506example = transcribeSet pg506Strand [FindPyRight, CopyOn, FindPuRight, Cut]
pg508example = transcribeSet pg508Strand [FindPuRight, InsertC, CopyOn, MoveRight, MoveLeft, Switch, FindPuLeft, InsertT]
-- getBindingLetter $ last $ getTertiaryStructure [FindPuRight, InsertC, CopyOn, MoveRight, MoveLeft, Switch, FindPuLeft, InsertT] = T, but the claim on pg 509 is that it is G...?  Errata?
