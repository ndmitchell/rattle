
-- | General rules for writing consistent rattle build systems:
--
-- * Never write to the same file twice. Never read then write.
--
-- * Don't delete files that have been produced. Each command should make
--   new files, not delete old files.
module Development.Rattle(
    rattleRun, Run,
    rattleDump,
    Hazard,
    RattleOptions(..), rattleOptions, RattleUI(..),
    cmd, CmdOption(..), CmdOption2(..), toCmdOption,
    module Development.Rattle.Derived,
    module Development.Rattle.Program,
    liftIO, writeProfile, graphData
    ) where

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Development.Shake.Command
import Development.Rattle.Server
import Development.Rattle.Derived
import Development.Rattle.Options
import Development.Rattle.CmdOption
import Development.Rattle.Shared
import Development.Rattle.Hazards
import Development.Rattle.Profile
import Development.Rattle.Program


-- | Given an Action to run, and a list of previous commands that got run, run it again
rattleRun :: RattleOptions -> Run a -> IO a
rattleRun opts (Run act) = withRattle opts $ \r ->
    runReaderT act r

-- | Dunmp the contents of a shared cache
rattleDump :: (String -> IO ()) -> FilePath -> IO ()
rattleDump = dump
