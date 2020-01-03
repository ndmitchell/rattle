{-# LANGUAGE TupleSections #-}

module Development.Rattle.Options(
    RattleOptions(..), rattleOptions,
    rattleOptionsExplicit, shorten, expand
    ) where

import Control.Monad.Extra
import General.Extra
import Data.Ord
import System.FilePath
import System.Directory
import qualified Development.Shake.Command as C
import Data.Maybe
import Data.List.Extra
import General.FileName
import qualified Data.ByteString.Char8 as BSC

-- | Basic options for configuring rattle.
data RattleOptions = RattleOptions
    {rattleFiles :: FilePath -- ^ Where all my shared files go
    ,rattleSpeculate :: Maybe String -- ^ Should I speculate? Under which key?
    ,rattleMachine :: String -- ^ Key to store run#
    ,rattleShare :: Bool -- ^ Should I share files from the cache
    ,rattleProcesses :: Int -- ^ Number of simulateous processes
    ,rattleCmdOptions :: [C.CmdOption] -- ^ Extra options added to every command line
    ,rattleNamedDirs :: [(BSC.ByteString, FilePath)] -- ^ Named directories
    ,rattleFancyUI :: Maybe Bool -- ^ True for Yes, Nothing for auto detect, False for no
    } deriving Show


-- | Default 'RattleOptions' value.
rattleOptions :: RattleOptions
rattleOptions = RattleOptions ".rattle" (Just "") "m1" True 0 [] [(BSC.pack "PWD",".")] Nothing


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


shorten :: [(BSC.ByteString, FilePath)] -> FileName -> FileName
shorten named x = fromMaybe x $ firstJust f named
    where f (name,dir) = do rest <- stripPrefix dir $ fileNameToString x; return $ fileNameFromString $ "$" ++ (BSC.unpack name) </> rest

expand :: [(BSC.ByteString, FilePath)] -> FileName -> FileName
expand named f = g $ fileNameToByteString f
  where g bs
          | ('$' == BSC.head bs) =
            let (x1, x2) = BSC.break isPathSeparator $ BSC.tail bs in
              if BSC.null x2 then f
              else case lookup x1 named of
                     (Just y) -> fileNameFromByteString $ BSC.append (BSC.pack y) x2
                     Nothing  -> f
          | otherwise = f
