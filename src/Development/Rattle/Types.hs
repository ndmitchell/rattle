{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving, DeriveAnyClass, DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Development.Rattle.Types(
    Trace(..), Touch(..), fsaTrace, normalizeTouch,
    Cmd(..),
    RunIndex, runIndex0, nextRunIndex,
    ) where

import Data.Hashable
import Data.List.Extra
import System.Directory
import System.Info.Extra
import Control.Monad
import Development.Shake.Command
import Data.Semigroup
import qualified Data.HashSet as Set
import GHC.Generics
import Prelude
import System.Time.Extra
import Data.Serialize
import General.FileName

data Cmd = Cmd [CmdOption] [String]
    deriving (Show, Eq, Generic, Hashable)

instance Serialize Cmd
deriving instance Serialize CmdOption
deriving instance Generic CmdOption
deriving instance Hashable CmdOption

data Trace a = Trace
    {tRun :: !RunIndex
    ,tStart :: Seconds
    ,tStop :: Seconds
    ,tTouch :: Touch a
    } deriving (Show, Functor, Foldable, Traversable, Eq, Generic)

instance Serialize a => Serialize (Trace a)

data Touch a = Touch
    {tRead :: [a]
    ,tWrite :: [a]
    } deriving (Show, Functor, Foldable, Traversable, Eq, Generic)

instance Serialize a => Serialize (Touch a)

instance Semigroup (Touch a) where
    Touch r1 w1 <> Touch r2 w2 = Touch (r1++r2) (w1++w2)

instance Monoid (Touch a) where
    mempty = Touch [] []
    mappend = (<>)

instance Hashable a => Hashable (Trace a) where
    hashWithSalt s (Trace a b c d) = hashWithSalt s (a,b,c,d)

instance Hashable a => Hashable (Touch a) where
    hashWithSalt s (Touch r w) = hashWithSalt s (r,w)

fsaTrace :: [FSATrace] -> IO (Touch FileName)
-- We want to get normalized traces. On Linux, things come out normalized, and we just want to dedupe them
-- On Windows things come out as C:\windows\system32\KERNELBASE.dll instead of C:\Windows\System32\KernelBase.dll
-- so important to call (expensive) normalizeTouch
fsaTrace fs
    | isWindows =
        -- normalize twice because normalisation is cheap, but canonicalisation might be expensive
        fmap (normalizeTouch . fmap fileNameFromString) $ canonicalizeTouch $ normalizeTouch $ mconcatMap f fs
    | otherwise =
        return $ normalizeTouch $ fmap fileNameFromString $ mconcatMap f fs
    where
        f (FSAWrite x) = Touch [] [x]
        f (FSARead x) = Touch [x] []
        f (FSADelete x) = Touch [] [x]
        f (FSAMove x y) = Touch [] [x,y]
        f (FSAQuery x) = Touch [x] []
        f (FSATouch x) = Touch [] [x]

normalizeTouch :: (Ord a, Hashable a) => Touch a -> Touch a
-- added 'sort' because HashSet uses the ordering of the hashes, which is confusing
normalizeTouch (Touch a b) = Touch (sort $ Set.toList $ a2 `Set.difference` b2) (sort $ Set.toList b2)
    where a2 = Set.fromList a
          b2 = Set.fromList b

canonicalizeTouch :: Touch FilePath -> IO (Touch FilePath)
canonicalizeTouch (Touch a b) = Touch <$> mapM g a <*> mapM g b
    where g x = do
            -- TEMPORARY EXPERIMENT
            y <- canonicalizePath x
            when (x /= y) $ print ("canonicalizePath CHANGED THINGS", x, y)
            return y


-- | Which run we are in, monotonically increasing
newtype RunIndex = RunIndex Int
    deriving (Eq,Ord,Show,Generic)

instance Serialize RunIndex

instance Hashable RunIndex where
    hashWithSalt s (RunIndex i) = hashWithSalt s i

runIndex0 :: RunIndex
runIndex0 = RunIndex 0

nextRunIndex :: RunIndex -> RunIndex
nextRunIndex (RunIndex i) = RunIndex $ i + 1
