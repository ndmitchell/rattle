
module BuildScript(main) where

import Development.Rattle
import Data.List.Extra
import System.Environment
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import System.FilePath
import Data.Maybe

-- Takes as arguments a file and the number of threads to run with
-- File is of the following format

-- dir: directory-following-command-executes-from
-- cmd (cmd is terminated by a newline if the line does not end with '\')

-- ( this might need to be changed for windows)


localOptions :: Int -> FilePath -> RattleOptions
localOptions j f = RattleOptions ".rattle" (Just "") "m1" True j [] [(BS.pack "PWD", ".")] Nothing $ Just f

shcCmd :: String -> FilePath -> Run ()
shcCmd c d = cmd (Cwd d) ["sh", "-c", c]

-- bare bones parsing
toCmds :: String -> [(String, FilePath)]
toCmds = f . lines
  where f [] = []
        f xs = let (p,ys) = getCmd xs in
                 p : f ys
        h [] = ([],[])
        -- get cmd line
        h (x:xs)
          | isSuffixOf "\\" $ trim x =
              let (ls1,r) = h xs in
                (x:ls1,r)
          | otherwise = ([trim x],xs)
        getCmd [] = (("", "."),[])
        getCmd (x:xs) =
          -- first get dir; then get cmd
          let (y, zs) = h xs in
            ((stripDashC $ unlines y, getDir x), zs)
        getDir str = case stripPrefix "dir: " str of
          Nothing -> error $ "Expected line beginning with 'dir: ' got: " ++ str
          Just d -> d
        unlines [] = []
        unlines [x] = x
        unlines (x:xs) = x ++ '\n' : unlines xs
        stripDashC str = fromMaybe str $ stripPrefix "-c" str

-- takes a single file as an argument and a number of threads
main :: IO ()
main = do
  [fileN,j] <- getArgs
  file <- readFile fileN
  let cmds = toCmds file
  rattleRun (localOptions (read j) $ fileN <.> "debug") $
    forM_ cmds $ uncurry shcCmd
