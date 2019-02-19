{-# Language DeriveGeneric     #-}
{-# Language OverloadedStrings #-}
{-# Language DeriveFunctor     #-}
{-# Language DeriveFoldable    #-}
{-# Language RecordWildCards   #-}

module Lib where

import           Control.Arrow                  ( (&&&) )
import           Data.Csv
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Vector                   as V
import           Data.Either                    ( fromRight )
import           GHC.Generics
import           Statistics.Sample
import           System.IO.Unsafe
import           Data.Foldable                  ( toList
                                                , foldl'
                                                )
import           Data.List                      ( transpose )

-- The definition of a spambase entry.
-- It is SpamEntry a as that allows me to be able to map over the data easily.
type SpamEntry = SpamE Double -- Alias for nicer type signatures
data SpamE a =
  S {
    _make        ::  !a
  , _address     ::  !a
  , _all         ::  !a
  , _3d          ::  !a
  , _our         ::  !a
  , _over        ::  !a
  , _remove      ::  !a
  , _internet    ::  !a
  , _order       ::  !a
  , _mail        ::  !a
  , _receive     ::  !a
  , _will        ::  !a
  , _people      ::  !a
  , _report      ::  !a
  , _addresses   ::  !a
  , _free        ::  !a
  , _business    ::  !a
  , _email       ::  !a
  , _you         ::  !a
  , _credit      ::  !a
  , _your        ::  !a
  , _font        ::  !a
  , _000         ::  !a
  , _money       ::  !a
  , _hp          ::  !a
  , _hpl         ::  !a
  , _george      ::  !a
  , _650         ::  !a
  , _lab         ::  !a
  , _labs        ::  !a
  , _telnet      ::  !a
  , _857         ::  !a
  , _data        ::  !a
  , _415         ::  !a
  , _85          ::  !a
  , _technology  ::  !a
  , _1999        ::  !a
  , _parts       ::  !a
  , _pm          ::  !a
  , _direct      ::  !a
  , _cs          ::  !a
  , _meeting     ::  !a
  , _original    ::  !a
  , _project     ::  !a
  , _re          ::  !a
  , _edu         ::  !a
  , _table       ::  !a
  , _conference  ::  !a
  , _semicolon   ::  !a
  , _lparen      ::  !a
  , _pipe        ::  !a
  , _bang        ::  !a
  , _dollarsign  ::  !a
  , _hash        ::  !a
  , _caps_avg    ::  !a
  , _caps_max    ::  !a
  , _caps_total  ::  !a
  , _spam        ::  !Int
} deriving (Generic, Show, Functor, Foldable)
  -- Gives me mapping, toList, folding, decoding, etc., for free.

-- CSV decoding instanes for free because of Generic
instance FromField a => FromRecord (SpamE a)
instance ToField   a => ToRecord   (SpamE a)

--
-- This is written in a semi unidiomatic haskell style where I don't really
-- have a main function that assembles everything together.  Instead, I use
-- unsafePerformIO to grab the data and treat it as a pure value so I can just
-- hardcode the assignment here. It allows for faster iteratino time.
--

-- Decodes the file into a vector of spambase entries
{-# NOINLINE spamEntries #-}
spamEntries :: V.Vector SpamEntry
spamEntries = fromRight undefined . decode NoHeader $ mydata
 where
  -- Load the data from a file. Bit of a hacky way to do it, but it makes the
  -- rest of the code easier to incrementally write.
  mydata :: BL.ByteString
  mydata = unsafePerformIO $ BL.readFile "spambase.data"
  {-# NOINLINE mydata #-} -- if you inline this function, bad shit happens

-- This works by indexing the vector of SpamEntry entries
-- Then the vector is split into two vectors, one with all the even indices,
-- one with all odd (this effectively splits the data sets equally)
--
-- Then the extra indices are stripped from both vectors
test, train :: V.Vector SpamEntry
(test, train) = splitData spamEntries
 where
  splitData
    :: V.Vector SpamEntry
    -> (V.Vector SpamEntry, V.Vector SpamEntry)
  splitData e = both (snd <$>) $ V.partition (even . fst) (V.indexed e)

--
-- Probabilities of the prior
--
probSpam :: V.Vector SpamEntry -> Double
probSpam s = V.length (fst $ splitSpam s) // V.length s

pSpamTrain, pSpamTest :: Double
(pSpamTrain, pSpamTest) = both probSpam (train, test)

-- Takes the spam entries and collects the (mean, standard deviation) of all 57
-- features
stats :: Foldable t => V.Vector (t Double) -> [(Double, Double)]
stats entries = (mean &&& stdDev) <$> collectedStats
 where
  --  This is just data shuffling to get all 57 features from the different
  --  spamEntries into a list of feature-vectors
  collectedStats :: [V.Vector Double]
  collectedStats = V.fromList . normalize <$$> transpose $ toList <$> V.toList entries
  -- Normalize is a safety precaution to prevent zeroes
  normalize       = fmap (\x -> if x < 0.0001 then 0.0001 else x)

-- Splits into a tuple of (spam, non spam)
splitSpam :: V.Vector SpamEntry -> (V.Vector SpamEntry, V.Vector SpamEntry)
splitSpam = V.partition ((== 1) . _spam)

-- Just some globals to use the functions just defined above
trainSpamStat, trainNonStat, testSpamStat, testNonStat :: [(Double, Double)]
(trainSpamStat, trainNonStat) = both stats $ splitSpam train
(testSpamStat, testNonStat) = both stats $ splitSpam test

--  This is the N(xi;μ,σ) function from the text
normal :: Floating a => (a, a) -> a -> a
normal (μ, σ) x = 1 / root * exp (-(x - μ) ^ 2 / (2 * σ ^ 2))
  where root = sqrt (2 * pi) * σ

--
-- The meat of the library. This classifies things as spam or nonSpam
--
data Class = Spam | NonSpam deriving (Show)

classify :: SpamEntry -> Class
classify e = if testSpamLogs > testNonLogs then Spam else NonSpam
 where
  testSpamLogs = log pSpamTrain + sum (logify testSpamStat)
  testNonLogs  = log (1 - pSpamTrain) + sum (logify testNonStat)

  logify :: [(Double, Double)] -> [Double]
  logify stat = log <$> zipWith ($) (normal <$> stat) (toList e)

classify' :: SpamEntry -> Accuracy
classify' e = case (guess, actual) of
  (Spam   , Spam   ) -> TruePos
  (NonSpam, NonSpam) -> TrueNeg
  (Spam   , NonSpam) -> FalsePos
  (NonSpam, Spam   ) -> FalseNeg
 where
  guess  = classify e
  actual = if _spam e == 1 then Spam else NonSpam

--
-- Accuracy and analysis section
--
data Accuracy = TruePos | TrueNeg | FalsePos | FalseNeg deriving (Show)

-- The higher-ordered data structure is, again, so I can map over the data
-- easily.  foldl' (+) accuracyData 0 will sum up all of the items in this data
-- structure, for example.
type AccuracyData = ACount Int
data ACount a =
  ACount {
    _truePos  :: !a
  , _trueNeg  :: !a
  , _falsePos :: !a
  , _falseNeg :: !a
} deriving (Show, Functor, Foldable)

-- Default starting point
accuracyData :: AccuracyData
accuracyData = ACount 0 0 0 0

-- Updater functions: succ a <=> a+1
truePos :: AccuracyData -> AccuracyData
truePos a@ACount {..} = a { _truePos = succ _truePos }

trueNeg :: AccuracyData -> AccuracyData
trueNeg a@ACount {..} = a { _trueNeg = succ _trueNeg }

falsePos :: AccuracyData -> AccuracyData
falsePos a@ACount {..} = a { _falsePos = succ _falsePos }

falseNeg :: AccuracyData -> AccuracyData
falseNeg a@ACount {..} = a { _falseNeg = succ _falseNeg }

-- Go over all of the classified data; categorize it into the confusion matrix,
-- then tally up all of the results.
tally :: AccuracyData
tally = foldl' accum accuracyData classes
 where
  classes = classify' <$> test
  accum a e = case e of
    TruePos  -> truePos a
    TrueNeg  -> trueNeg a
    FalsePos -> falsePos a
    FalseNeg -> falseNeg a

--
-- The statistics:
--
accuracy :: Double
accuracy = allPos // total
 where
  allPos = _truePos tally + _trueNeg tally
  total  = foldl' (+) 0 tally

recall :: Double
recall = tp // (tp + _falseNeg tally) where tp = _truePos tally

precision :: Double
precision = tp // allPos
 where
  tp     = _truePos tally
  allPos = tp + _falsePos tally


--
-- Main
--
main = do
  putStrLn $ "The accuracy is: "         ++ show accuracy
  putStrLn $ "The recall is: "           ++ show recall
  putStrLn $ "The precision is: "        ++ show precision
  putStrLn $ "The confusion matrix is: " ++ show tally

--
-- Helper functions
--

-- Apply a function to both sides of a tuple at once
both :: (t -> b) -> (t, t) -> (b, b)
both f (a, b) = (f a, f b)

-- Nested mapping
infixl 5 <$$>
(<$$>) :: (Functor f2, Functor f1) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap fmap fmap

-- Division with implicit number conversion
(//) :: (Integral a1, Integral a, Fractional a2) => a -> a1 -> a2
a // b = fromIntegral a / fromIntegral b
