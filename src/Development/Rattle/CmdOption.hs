

-- | Extra Cmd options as specified by Rattle.
--   They are stashed in AddEnv "#!RATTLE" options to get the nice interface
module Development.Rattle.CmdOption(
    CmdOption2(..),
    toCmdOption, fromCmdOption
    ) where

import Development.Shake.Command
import System.FilePattern

data CmdOption2
    = Ignored [FilePattern] -- Files that are ignored
    | HashNonDeterministic [FilePattern] -- Files that have non-det outputs, so hash the inputs
    | WriteFile FilePath
      deriving (Read, Show)


instance IsCmdArgument CmdOption2 where
    toCmdArgument = toCmdArgument . toCmdOption

toCmdOption :: CmdOption2 -> CmdOption
toCmdOption = AddEnv "#!RATTLE" . show

fromCmdOption :: CmdOption -> Either CmdOption CmdOption2
fromCmdOption (AddEnv "#!RATTLE" x) = Right $ read x
fromCmdOption x = Left x
