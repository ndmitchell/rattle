

-- | Extra Cmd options as specified by Rattle.
--   They are stashed in AddEnv "#!RATTLE" options to get the nice interface
module Development.Rattle.CmdOption(
    CmdOption2(..),
    toCmdOption, fromCmdOption
    ) where

import Development.Shake.Command
import System.FilePattern

-- | A data type for additional rattle options
data CmdOption2
    = Ignored [FilePattern] -- Files that are ignored
    | Read [FilePath] -- Files that are required
    | Write [FilePath]
    | HashNonDeterministic [FilePattern] -- Files that have non-det outputs, so hash the inputs
    | WriteFile FilePath
      deriving (Read, Show)


instance IsCmdArgument CmdOption2 where
    toCmdArgument = toCmdArgument . toCmdOption

-- | Convert a new option into a standard one.
toCmdOption :: CmdOption2 -> CmdOption
toCmdOption = AddEnv "#!RATTLE" . show

-- | Convert a normal option into potentially a rattle one.
fromCmdOption :: CmdOption -> Either CmdOption CmdOption2
fromCmdOption (AddEnv "#!RATTLE" x) = Right $ read x
fromCmdOption x = Left x
