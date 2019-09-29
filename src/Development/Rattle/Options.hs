{-# LANGUAGE TupleSections #-}

module Development.Rattle.Options(
    RattleOptions(..), rattleOptions,
    rattleOptionsExplicit, shorten, expand
    ) where

import Control.Monad.Extra
import General.Extra
import Data.Ord
import System.FilePath
import System.FilePattern
import System.Directory
import qualified Development.Shake.Command as C
import Data.Maybe
import Data.List.Extra


-- | Basic options for configuring rattle.
data RattleOptions = RattleOptions
    {rattleFiles :: FilePath -- ^ Where all my shared files go
    ,rattleSpeculate :: Maybe String -- ^ Should I speculate? Under which key?
    ,rattleMachine :: String -- ^ Key to store run#
    ,rattleShare :: Bool -- ^ Should I share files from the cache
    ,rattleProcesses :: Int -- ^ Number of simulateous processes
    ,rattleCmdOptions :: [C.CmdOption] -- ^ Extra options added to every command line
    ,rattleNamedDirs :: [(String, FilePath)] -- ^ Named directories
    ,rattleIgnore :: [FilePattern]
    ,rattleFancyUI :: Maybe Bool -- ^ True for Yes, Nothing for auto detect, False for no
    } deriving Show


-- | Default 'RattleOptions' value.
rattleOptions :: RattleOptions
rattleOptions = RattleOptions ".rattle" (Just "") "m1" True 0 [] [("PWD",".")] [] Nothing


rattleOptionsExplicit :: RattleOptions -> IO RattleOptions
rattleOptionsExplicit = fixProcessorCount >=> fixNamedDirs
    where
        fixProcessorCount o
            | rattleProcesses o /= 0 = return o
            | otherwise = do p <- getProcessorCount; return o{rattleProcesses=p}

        fixNamedDirs o = do
            xs <- sequence [(a,) . addTrailingPathSeparator <$> canonicalizePath b | (a,b) <- rattleNamedDirs o]
            -- sort so all prefixes come last, so we get the most specific match
            return o{rattleNamedDirs = sortOn (Down . snd) xs}


shorten :: [(String, FilePath)] -> FilePath -> FilePath
shorten named x = fromMaybe x $ firstJust f named
    where f (name,dir) = do rest <- stripPrefix dir x; return $ "$" ++ name </> rest

expand :: [(String, FilePath)] -> FilePath -> FilePath
expand named ('$':x)
    | (x1, _:x2) <- break isPathSeparator x
    , Just y <- lookup x1 named
    = y ++ x2
expand _ x = x
