{-
http://www.reddit.com/r/dailyprogrammer/comments/2yx8b8/20150313_challenge_205_hard_dna_and_protein/

[2015-03-13] Challenge #205 [Hard] DNA and Protein Sequence Alignment
submitted 3 days ago * by jnazario2 0
Description

If you are studying a particular pair of genes or proteins, an important question is to what extent the two sequences are similar. To quantify similarity, it is necessary to align the two sequences, and then you can calculate a similarity score based on the alignment.
There are two types of alignment in general. A global alignment is an alignment of the full length of two sequences, for example, of two protein sequences or of two DNA sequences. A local alignment is an alignment of part of one sequence to part of another sequence.
Alignment treats the two inputs as a linear sequence to be lined up as much as possible, with optional gaps and conversions allowed. The goal is to minimize these differences.
The first step in computing a sequence alignment is to decide on a scoring system. For this exercise, we'll simplify this and give a score of +2 to a match and a penalty of -1 to a mismatch, and a penalty of -2 to a gap.
Here's a small example. Our two DNA sequences to align:
CTCTAGCATTAG
GTGCACCCA
One alignment might look like this:
CTCTAGCATTAG
GT---GCACCCA
But that one adds three gaps. We can do a bit better with only one gap added (and a small shift in starting position):
CTCTAGCATTAG
  GT-GCACCCA
While not an exact match, it now minimizes the conversion penalties between the two and aligns them as best we can.
For more information and how to do this using an R package, see the chapter "Pairwise Sequence Alignment", or this set of lecture notes from George Washington University. The key algorithm is Needleman-Wunsch.
For this challenge your task is to write a program that accepts two sequences and globally aligns them. If you want to make this harder and integrate the BLOSUM matrices, you may.
Input Description

You'll be given two sequences on two lines, one line per sequence. They'll be the same type of input, DNA or protein.
Output Description

Your program should emit the aligned sequences with gaps introduced represented by dashed ("-").
Input

DNA example
GACGTACGTACGTACGTACGTACGTACGTACGTACGTACGTACGTACGTAC
ACGTACGTACGTACGTACGTACGTACGTACGTACGTACGTACGTAC
Protein example
    MTNRTLSREEIRKLDRDLRILVATNGTLTRVLNVVANEEIVVDIINQQLLDVAPKIPELENLKIGRILQRDILLKGQKSGILFVAAESLIVIDLLPTAITTYLTKTHHPIGEIMAASRIETYKEDAQVWIGDLPCWLADYGYWDLPKRAVGRRYRIIAGGQPVIITTEYFLRSVFQDTPREELDRCQYSNDIDTRSGDRFVLHGRVFKN
    MLAVLPEKREMTECHLSDEEIRKLNRDLRILIATNGTLTRILNVLANDEIVVEIVKQQIQDAAPEMDGCDHSSIGRVLRRDIVLKGRRSGIPFVAAESFIAIDLLPPEIVASLLETHRPIGEVMAASCIETFKEEAKVWAGESPAWLELDRRRNLPPKVVGRQYRVIAEGRPVIIITEYFLRSVFEDNSREEPIRHQRSVGTSARSGRSICT
Output

DNA example
GACGTACGTACGTACGTACGTACGTACGTACGTACGTACGTACGTACGTAC
 ACGTACGTACGTACGTACGTACGTACGTACGTACGTACGTACGTAC
Protein example
          MTNRTLSREEIRKLDRDLRILVATNGTLTRVLNVVANEEIVVDIINQQLLDVAPKIPELENLKIGRILQRDILLKGQKSGILFVAAESLIVIDLLPTAITTYLTKTHHPIGEIMAASRIETYKEDAQVWIGDLPCWLADYGYWDLPKRAVGRRYRIIAGGQPVIITTEYFLRSVFQDTPREELDRCQYSNDIDTRSGDRFVLHGRVFKN
MLAVLPEKREMTECHLSDEEIRKLNRDLRILIATNGTLTRILNVLANDEIVVEIVKQQIQDAAPEMDGCDHSSIGRVLRRDIVLKGRRSGIPFVAAESFIAIDLLPPEIVASLLETHRPIGEVMAASCIETFKEEAKVWAGESPAWLELDRRRNLPPKVVGRQYRVIAEGRPVIIITEYFLRSVFEDNSREEPIRHQRS--VGT-SA-R---SGRSICT
Notes

Once you have a simple NW algorithm implemented, you can alter the cost matrices. In the bioinformatics field, the PAM and BLOSUM matrices are the standards. You can find them here: ftp://ftp.ncbi.nih.gov/blast/matrices/
Have a cool challenge idea? Post it to /r/DailyProgrammer_Ideas!
-}

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Par
import Control.Parallel.Strategies
import Data.Foldable as F
import Data.Monoid
import Data.Ord
import Data.Sequence ((><), (<|), (|>))
import qualified Data.Sequence as S

{-
--Needed for older ghc/base
instance Applicative S.Seq where
    pure = S.singleton
    fs <*> xs = foldl' add S.empty fs
        where add ys f = ys >< fmap f xs
-}

parsearch :: NFData solution
      => Int                             -- depth
      -> ( partial -> Maybe solution )   -- finished?
      -> ( partial -> [ partial ] )      -- refine a solution
      -> partial                         -- initial solution
      -> [solution]
parsearch maxdepth finished refine emptysoln
  = runPar $ generate 0 emptysoln
  where
    generate d partial | d >= maxdepth
       = return (search finished refine partial)
    generate d partial
       | Just soln <- finished partial = return [soln]
       | otherwise  = do
           solnss <- parMapM (generate (d+1)) (refine partial)
           return (F.concat solnss)

search :: ( partial -> Maybe solution )
       -> ( partial -> [ partial ] )
       -> partial
       -> [solution]
search finished refine emptysoln = generate emptysoln
  where
    generate partial
       | Just soln <- finished partial = [soln]
       | otherwise  = F.concat (fmap generate (refine partial))

aligns :: String -> String -> Bool -> Bool -> S.Seq (S.Seq (Char, Char))
aligns []  []  _ _ = S.empty
aligns [x] [y] _ _ = S.singleton $ S.singleton (x, y)
aligns xs  []  _ _ = S.singleton $ S.fromList $ zip xs (repeat ' ')
aligns []  ys  _ _ = S.singleton $ S.fromList $ zip (repeat ' ') ys
aligns (x:xs) (y:ys) xStart yStart = Prelude.foldr1 (><)
    [ (<|) <$> pure (x, y)      <*> aligns xs ys     True   True
    , (<|) <$> pure (blankX, y) <*> aligns (x:xs) ys xStart True
    , (<|) <$> pure (x, blankY) <*> aligns xs (y:ys) True   yStart ]
    where 
      blankX = if xStart then '-' else ' '
      blankY = if yStart then '-' else ' '

score :: (Char, Char) -> Int
score (a,b) | a == b               = 2
            | a == '-' || b == '-' = (-2)
            | a == ' ' || b == ' ' = 0
            | otherwise            = (-1)

best :: String -> String -> (String, String)
best a b = unzip . toList $ maximumBy (comparing rating) (parAligns a b)
    where
        parAligns a b = withStrategy (parBuffer 64 rdeepseq) (alignsStart a b)
        rating = F.sum . fmap score
        alignsStart a b = toList $ aligns a b False False

main = interact $ \input ->
    let [a, b] = lines input
        (bestA, bestB) = best a b
    in unlines [bestA, bestB]
