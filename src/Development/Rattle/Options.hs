{-# LANGUAGE TupleSections #-}

module Development.Rattle.Options(
    RattleOptions(..), RattleUI(..), rattleOptions,
    rattleOptionsExplicit, shorten, expand
    ) where

import Control.Monad.Extra
import General.Extra
import Data.Ord
import System.FilePath
import System.Directory
import Development.Rattle.UI
import qualified Data.HashMap.Strict as Map
import qualified Development.Shake.Command as C
import Data.Maybe
import Data.List.Extra
import General.FileName
import qualified Data.ByteString.Char8 as BSC
import Data.Monoid
import Prelude


-- | Basic options for configuring rattle.
data RattleOptions = RattleOptions
    {rattleFiles :: FilePath -- ^ Where all my shared files go
    ,rattleSpeculate :: Maybe String -- ^ Should I speculate? Under which key?
    ,rattleMachine :: String -- ^ Key to store run#
    ,rattleShare :: Bool -- ^ Should I share files from the cache
    ,rattleProcesses :: Int -- ^ Number of simulateous processes
    ,rattleCmdOptions :: [C.CmdOption] -- ^ Extra options added to every command line
    ,rattleNamedDirs :: [(String, FilePath)] -- ^ Named directories, e.g. (PWD, .)
    ,rattleUI :: Maybe RattleUI -- ^ Nothing for auto detect
    ,rattleForward :: Bool -- ^ Support forward style stuff
    } deriving Show


-- | Default 'RattleOptions' value.
rattleOptions :: RattleOptions
rattleOptions = RattleOptions ".rattle" (Just "") "m1" True 0 [] [("PWD",".")] Nothing False


rattleOptionsExplicit :: RattleOptions -> IO RattleOptions
rattleOptionsExplicit = fixProcessorCount >=> fixNamedDirs
    where
        fixProcessorCount o
            | rattleProcesses o /= 0 = pure o
            | otherwise = do p <- getProcessorCount; pure o{rattleProcesses=p}

        fixNamedDirs o = do
            xs <- sequence [(a,) . addTrailingPathSeparator <$> canonicalizePath b | (a,b) <- rattleNamedDirs o]
            -- sort so all prefixes come last, so we get the most specific match
            pure o{rattleNamedDirs = sortOn (Down . snd) xs}


shorten :: [(String, String)] -> FileName -> FileName
shorten [] = id
shorten named = \x -> fromMaybe x $ firstJust (f x) named2
    where
        named2 = [(BSC.pack $ "$" ++ a ++ [pathSeparator], BSC.pack $ addTrailingPathSeparator b) | (a,b) <- named]
        f x (name,dir) = do rest <- BSC.stripPrefix dir $ fileNameToByteString x; pure $ byteStringToFileName $ name <> rest

expand :: [(String, String)] -> FileName -> FileName
expand [] = id
expand named = \x -> case BSC.uncons $ fileNameToByteString x of
    Just ('$', x)
        | (x1, x2) <- BSC.break isPathSeparator x
        , not $ BSC.null x2
        , Just v <- Map.lookup x1 named2
        -> byteStringToFileName $ v <> x2
    _ -> x
    where
        named2 = Map.fromList [(BSC.pack a, BSC.pack b) | (a,b) <- named]
