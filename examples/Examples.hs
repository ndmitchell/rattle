
module Examples(main) where

import System.Environment
import System.Directory
import Development.Shake.Command
import System.IO.Extra
import Development.Rattle
import System.FilePath
import Control.Monad

import qualified Builder.FSATrace


projects =
    [f "fsatrace" Builder.FSATrace.run "https://github.com/jacereda/fsatrace"
    ]
    where f a b c = (a,(b,c))

main :: IO ()
main = do
    args <- getArgs
    case args of
        [x] | Just (run,_) <- lookup x projects ->
            rattle rattleOptions run
        ["local",x] | Just (run,_) <- lookup x projects ->
            withCurrentDirectory (".." </> x) $ rattle rattleOptions run
        ["test"] -> do
            forM_ projects $ \(name,(run,url)) -> do
                withTempDir $ \dir -> do
                    cmd_ (Cwd dir) "git clone" url name
                    withCurrentDirectory (dir </> name) $
                        rattle rattleOptions run
