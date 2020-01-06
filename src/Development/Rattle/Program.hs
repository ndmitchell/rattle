{-# LANGUAGE RecordWildCards #-}

module Development.Rattle.Program(
    Program, newProgram, runProgram
    ) where

import Development.Rattle.Hash
import Development.Rattle.Server
import Development.Rattle.Derived
import Development.Shake.Command
import System.IO.Unsafe
import System.FilePath
import Language.Haskell.TH
import qualified Data.ByteString.Char8 as BS

-- | A program that can be run externally.
data Program a = Program
    {programDisplay :: a -> String
    ,programContents :: String
    ,programHash :: Hash
    }

-- | Create a new program which is based on a TH splice.
newProgram :: (Show a, Read a) => (a -> String) -> Q (TExp (a -> IO ())) -> Program a
newProgram display expr = Program display contents (hashString contents)
    where contents = unlines $ generate $ unsafePerformIO $ runQ expr

-- | Run a program.
runProgram :: Show a => Program a -> a -> Run ()
runProgram Program{..} x = do
    let Hash unhash = programHash
    let file = ".rattle/program/" </> BS.unpack unhash <.> "hs"
    cmdWriteFile file programContents
    cmd "runhaskell" file [show x]

generate :: TExp (a -> IO ()) -> [String]
generate expr =
    ["import System.Environment"
    ,"import System.IO"
    ,"import System.Directory"
    ,"import Development.Shake.Command"
    ,"import System.FilePath.Windows"
    ,"import Data.List as Data.OldList"
    ,"import System.IO.Extra"
    ,"import Data.Foldable"
    ,"import Data.Functor"
    ,"import System.Directory.Extra"
    ,"import GHC.List"
    ,"import Development.Shake.Command as Development.Shake.Internal.CmdOption"
    ,"import Data.List.Extra"
    ,"import GHC.Base"
    ,"import GHC.Classes"
    ,"main :: IO ()"
    ,"main = do [x] <- System.Environment.getArgs; body (read x)"
    ,"body = " ++ pprint (unType expr)
    ]
