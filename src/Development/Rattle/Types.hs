{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Development.Rattle.Types(
    Trace(..), Touch(..), fsaTrace, normalizeTouch,
    TouchSet, tsRead, tsWrite, newTouchSet, addTouchSet,
    Cmd(..), mkCmd,
    RunIndex, runIndex0, nextRunIndex,
    ) where

import Data.Hashable
import Data.List.Extra
import System.Directory
import System.Info.Extra
import Control.Monad
import General.Binary
import Data.Word
import Development.Shake.Command
import Data.Semigroup
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString as BS
import qualified Data.HashSet as Set
import GHC.Generics
import Prelude
import System.Time.Extra
import General.FileName

-- record the hash as the first field
data Cmd = Cmd Int [CmdOption] [String]
    deriving Eq

instance Show Cmd where
    show (Cmd _ a b) = "Cmd " ++ show a ++ " " ++ show b

instance Hashable Cmd where
    hashWithSalt _ = hash
    hash (Cmd x _ _) = x

mkCmd :: [CmdOption] -> [String] -> Cmd
mkCmd a b = Cmd (hash (a,b)) a b

instance BinaryEx Cmd where
    getEx x = mkCmd (getEx a) (getEx b)
        where (a,b) = getExPair x
    putEx (Cmd _ a b) = putExPair (putEx a) (putEx b)

-- The common values for CmdOption are [], [Shell] and a few others - optimise those
instance BinaryEx [CmdOption] where
    getEx x
        | BS.null x = []
        | BS.length x == 1 = case getEx x :: Word8 of
            0 -> [Shell]
            1 -> [EchoStderr False]
            2 -> [Shell,EchoStderr False]
        | otherwise = map read $ getEx x

    putEx [] = mempty
    putEx [Shell] = putEx (0 :: Word8)
    putEx [EchoStderr False] = putEx (1 :: Word8)
    putEx [Shell,EchoStderr False] = putEx (2 :: Word8)
    putEx xs = putEx $ map show xs

deriving instance Generic CmdOption
deriving instance Read CmdOption
instance Hashable CmdOption

data Trace a = Trace
    {tRun :: {-# UNPACK #-} !RunIndex
    ,tStart :: {-# UNPACK #-} !Seconds
    ,tStop :: {-# UNPACK #-} !Seconds
    ,tTouch :: Touch a
    } deriving (Show, Functor, Foldable, Traversable, Eq)

instance BinaryEx a => BinaryEx (Trace a) where
    getEx x = Trace a b c $ getEx d
        where (a,b,c,d) = binarySplit3 x
    putEx (Trace a b c d) = putExStorable a <> putExStorable b <> putExStorable c <> putEx d

data Touch a = Touch
    {tRead :: [a]
    ,tWrite :: [a]
    } deriving (Show, Functor, Foldable, Traversable, Eq)

instance BinaryEx a => BinaryEx (Touch a) where
    getEx x = Touch (map getEx $ getExList a) (map getEx $ getExList b)
        where [a,b] = getExList x
    putEx (Touch a b) = putExList [putExList $ map putEx a, putExList $ map putEx b]

instance Semigroup (Touch a) where
    Touch r1 w1 <> Touch r2 w2 = Touch (r1++r2) (w1++w2)

instance Monoid (Touch a) where
    mempty = Touch [] []
    mappend = (<>)
    mconcat xs = Touch (concatMap tRead xs) (concatMap tWrite xs)

instance Hashable a => Hashable (Trace a) where
    hashWithSalt s (Trace a b c d) = hashWithSalt s (a,b,c,d)

instance Hashable a => Hashable (Touch a) where
    hashWithSalt s (Touch r w) = hashWithSalt s (r,w)

fsaTrace :: [FSATrace BS.ByteString] -> IO (Touch FileName)
-- We want to get normalized traces. On Linux, things come out normalized, and we just want to dedupe them
-- On Windows things come out as C:\windows\system32\KERNELBASE.dll instead of C:\Windows\System32\KernelBase.dll
-- so important to call (expensive) normalizeTouch
fsaTrace fs
    | isWindows =
        -- normalize twice because normalisation is cheap, but canonicalisation might be expensive
        fmap (normalizeTouch . fmap (byteStringToFileName . UTF8.fromString)) $
        canonicalizeTouch $
        fmap UTF8.toString $ normalizeTouch $ mconcatMap f fs
    | otherwise =
        -- We know the file names are already normalized from Shake so avoid a redundant conversion
        pure $ normalizeTouch $ byteStringToFileName <$> mconcatMap f fs
    where
        f (FSAWrite x) = Touch [] [x]
        f (FSARead x) = Touch [x] []
        f (FSADelete x) = Touch [] [x]
        f (FSAMove x y) = Touch [] [x,y]
        f (FSAQuery x) = Touch [x] []
        f (FSATouch x) = Touch [] [x]

normalizeTouch :: (Ord a, Hashable a) => Touch a -> Touch a
-- added 'sort' because HashSet uses the ordering of the hashes, which is confusing
-- and since we are sorting, try and avoid doing too much hash manipulation of the reads
normalizeTouch (Touch a b) = Touch (f $ sort a) (sort $ Set.toList b2)
    where
        b2 = Set.fromList b
        f (x1:x2:xs) | x1 == x2 = f (x1:xs)
        f (x:xs) | x `Set.member` b2 = f xs
                 | otherwise = x : f xs
        f [] = []

canonicalizeTouch :: Touch FilePath -> IO (Touch FilePath)
canonicalizeTouch (Touch a b) = Touch <$> mapM canonicalizePath a <*> mapM canonicalizePath b


-- For sets, Set.fromList is fastest if there are no dupes
-- Otherwise a Set.member/Set.insert is fastest
data TouchSet = TouchSet {tsRead :: Set.HashSet FileName, tsWrite :: Set.HashSet FileName}

newTouchSet :: [Touch FileName] -> TouchSet
newTouchSet [] = TouchSet Set.empty Set.empty
newTouchSet (Touch{..}:xs) = foldl' addTouchSet (TouchSet (Set.fromList tRead) (Set.fromList tWrite)) xs

addTouchSet :: TouchSet -> Touch FileName -> TouchSet
addTouchSet TouchSet{..} Touch{..} = TouchSet (f tsRead tRead) (f tsWrite tWrite)
    where f = foldl' (\mp k -> if Set.member k mp then mp else Set.insert k mp)


-- | Which run we are in, monotonically increasing
newtype RunIndex = RunIndex Int
    deriving (Eq,Ord,Show,Storable,BinaryEx,Hashable)

runIndex0 :: RunIndex
runIndex0 = RunIndex 0

nextRunIndex :: RunIndex -> RunIndex
nextRunIndex (RunIndex i) = RunIndex $ i + 1
