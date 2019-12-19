{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving, DeriveAnyClass, DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Development.Rattle.Types(
    Trace(..), Touch(..), fsaTrace,
    Cmd(..),
    T, t0,
    RunIndex, runIndex0, nextRunIndex,
    ) where

import Data.Hashable
import Data.List.Extra
import System.Directory
import Development.Shake.Command
import Data.Semigroup
import qualified Data.HashSet as Set
import GHC.Generics
import Prelude
import System.Time.Extra


data Cmd = Cmd [CmdOption] [String]
    deriving (Show, Read, Eq, Generic, Hashable)

deriving instance Read CmdOption
deriving instance Generic CmdOption
deriving instance Hashable CmdOption

data Trace a = Trace
    {tRun :: !RunIndex
    ,tTime :: Seconds
    ,tTouch :: Touch a
    } deriving (Show, Read, Functor, Foldable, Traversable, Eq)

data Touch a = Touch
    {tRead :: [a]
    ,tWrite :: [a]
    } deriving (Show, Read, Functor, Foldable, Traversable, Eq)

instance Semigroup (Touch a) where
    Touch r1 w1 <> Touch r2 w2 = Touch (r1++r2) (w1++w2)

instance Monoid (Touch a) where
    mempty = Touch [] []
    mappend = (<>)

instance Hashable a => Hashable (Trace a) where
    hashWithSalt s (Trace tt rr tr) = hashWithSalt s (tt,rr,tr)

instance Hashable a => Hashable (Touch a) where
    hashWithSalt s (Touch r w) = hashWithSalt s (r,w)

fsaTrace :: RunIndex -> Seconds -> [FSATrace] -> IO (Trace FilePath)
-- normalize twice because normalisation is cheap, but canonicalisation might be expensive
fsaTrace rr t fs = fmap (Trace rr t . normalizeTouch) $ canonicalizeTouch $ normalizeTouch $ mconcat $ map f fs
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
canonicalizeTouch (Touch a b) = Touch <$> mapM canonicalizePath a <*> mapM canonicalizePath b


-- Which run we are in, monotonically increasing
newtype RunIndex = RunIndex Int
    deriving (Eq,Ord,Show,Read)

instance Hashable RunIndex where
  hashWithSalt s (RunIndex i) = hashWithSalt s i

runIndex0 :: RunIndex
runIndex0 = RunIndex 0

nextRunIndex :: RunIndex -> RunIndex
nextRunIndex (RunIndex i) = RunIndex $ i + 1


newtype T = T Int -- timestamps
    deriving (Enum,Eq,Ord,Show,Read)

instance Hashable T where
  hashWithSalt s (T i) = hashWithSalt s i

t0 :: T
t0 = T 0
