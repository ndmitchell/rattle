{-# LANGUAGE TupleSections, GeneralizedNewtypeDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving, DeriveAnyClass, DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Development.Rattle.Types(
    Trace(..), fsaTrace,
    Cmd(..),
    T, t0,
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


data Cmd = Cmd [CmdOption] String [String]
    deriving (Show, Read, Eq, Generic, Hashable)

deriving instance Read CmdOption
deriving instance Generic CmdOption
deriving instance Hashable CmdOption

data Trace a = Trace
    {tTime :: Seconds
    ,tRun :: !T
    ,tRead :: [a]
    ,tWrite :: [a]
    } deriving (Show, Read, Functor, Foldable, Traversable, Eq)

instance Semigroup (Trace a) where
    Trace t1 tr1 r1 w1 <> Trace t2 tr2 r2 w2 = Trace (max t1 t2) (max tr1 tr2) (r1++r2) (w1++w2)

instance Monoid (Trace a) where
    mempty = Trace 0.0 (T (-1)) [] []
    mappend = (<>)

instance Hashable a => Hashable (Trace a) where
  hashWithSalt s (Trace tt rr tr tw) = hashWithSalt s (tt,rr,tr,tw)

fsaTrace :: Seconds -> T -> [FSATrace] -> IO (Trace FilePath)
fsaTrace t rr [] = return $ Trace t rr [] []
-- normalize twice because normalisation is cheap, but canonicalisation might be expensive
fsaTrace t rr fs = fmap normTrace $ canonicalizeTrace $ normTrace $ mconcat $ map f fs
    where
        g = Trace t rr
        f (FSAWrite x) = g [] [x]
        f (FSARead x) = g [x] []
        f (FSADelete x) = g [] [x]
        f (FSAMove x y) = g [] [x,y]
        f (FSAQuery x) = g [x] []
        f (FSATouch x) = g [] [x]

normTrace :: (Ord a, Hashable a) => Trace a -> Trace a
-- added 'sort' because HashSet uses the ordering of the hashes, which is confusing
normTrace (Trace t r a b) = Trace t r (sort $ Set.toList $ a2 `Set.difference` b2) (sort $ Set.toList b2)
    where a2 = Set.fromList a
          b2 = Set.fromList b

canonicalizeTrace :: Trace FilePath -> IO (Trace FilePath)
canonicalizeTrace (Trace t rr r w) = Trace t rr <$> mapM canonicalizePath r <*> mapM canonicalizePath w


newtype T = T Int -- timestamps
    deriving (Enum,Eq,Ord,Show,Read)

instance Hashable T where
  hashWithSalt s (T i) = hashWithSalt s i

t0 :: T
t0 = T 0
