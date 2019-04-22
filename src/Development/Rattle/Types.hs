{-# LANGUAGE TupleSections, GeneralizedNewtypeDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Development.Rattle.Types(
    Trace(..), fsaTrace,
    Cmd(..),
    T, t0,
    ) where

import Data.Hashable
import Data.List.Extra
import Development.Shake.Command
import Data.Semigroup
import Prelude
import System.Time.Extra

newtype Cmd = Cmd [String]
    deriving (Show, Read, Eq, Hashable)

data Trace a = Trace
    {tTime :: Seconds
    ,tRead :: [(FilePath, a)]
    ,tWrite :: [(FilePath, a)]
    } deriving (Show, Read, Functor, Foldable, Traversable, Eq)

instance Semigroup (Trace a) where
    Trace t1 r1 w1 <> Trace t2 r2 w2 = Trace (max t2 t2) (r1++r2) (w1++w2)

instance Monoid (Trace a) where
    mempty = Trace 0.0 [] []
    mappend = (<>)

instance Hashable a => Hashable (Trace a) where
  hashWithSalt s (Trace tt tr tw) =
    s`hashWithSalt`
    tt `hashWithSalt`
    tr `hashWithSalt` tw


fsaTrace :: Seconds -> [FSATrace] -> Trace ()
fsaTrace t [] = Trace t [] []
fsaTrace t fs = nubTrace . mconcat $ map f fs
    where
        g r w = Trace t (map (,()) r) (map (,()) w)
        f (FSAWrite x) = g [] [x]
        f (FSARead x) = g [x] []
        f (FSADelete x) = g [] [x]
        f (FSAMove x y) = g [] [x,y]
        f (FSAQuery x) = g [x] []
        f (FSATouch x) = g [] [x]

nubTrace :: Ord a => Trace a -> Trace a
nubTrace (Trace t a b) = Trace t (nubOrd a \\ b) (nubOrd b)


newtype T = T Int -- timestamps
    deriving (Enum,Eq,Ord,Show)

t0 :: T
t0 = T 0
