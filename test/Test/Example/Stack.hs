
-- | Try performing a Stack-like install.
module Test.Example.Stack(main) where

import Development.Rattle
import Development.Shake.FilePath
import System.IO.Extra
import qualified Data.ByteString as BS
import Data.Maybe
import Control.Monad.Extra
import System.Environment
import System.Directory
import Data.List.Extra
import qualified Data.HashMap.Strict as Map

-- Cabal packages
import Distribution.PackageDescription.Parsec
import Distribution.Types.CondTree
import Distribution.Types.GenericPackageDescription
import Distribution.Types.Dependency
import Distribution.Types.PackageName(unPackageName)


type PackageName = String
type PackageVersion = String

main :: IO ()
main = do
    args <- getArgs
    tdir <- canonicalizePath =<< getTemporaryDirectory
    let ignore = ["**/hackage-security-lock", "**/package.cache.lock", tdir ++ "/**"]
    rattle rattleOptions{rattleIgnore=ignore} $ stack "nightly-2019-05-15" $ args ++ ["cereal" | null args]


installPackage :: (PackageName -> Run (Maybe PackageVersion)) -> FilePath -> PackageName -> PackageVersion -> Run ()
installPackage dep config name version = do
    let dir = name ++ "-" ++ version
    cmd "cabal unpack" dir
    depends <- liftIO $ cabalDepends $ dir </> name <.> "cabal"
    dependsVer <- forP depends dep
    cmd (Cwd dir) "cabal v1-configure"
        "--disable-library-profiling --disable-optimisation"
        ["--package-db=../" ++ n ++ "-" ++ v ++ "/dist/package.conf.inplace" | (n, Just v) <- zip depends dependsVer]
    cmd (Cwd dir) "cabal v1-build" ("lib:" ++ name)


stack :: String -> [PackageName] -> Run ()
stack resolver packages = do
    let config = resolver <.> "config"
    -- Shell below is to hack around an fsatrace issue
    cmd Shell "curl -sSL" ("https://www.stackage.org/" ++ resolver ++ "/cabal.config") "-o" config
    versions <- liftIO $ readResolver config
    let askVersion x = fromMaybe (error $ "Don't know version for " ++ show x) $ Map.lookup x versions
    needPkg <- memoRec $ \needPkg name -> do
        let v = askVersion name
        whenJust v $ installPackage needPkg config name
        return v
    forP_ packages needPkg


readResolver :: FilePath -> IO (Map.HashMap PackageName (Maybe PackageVersion))
readResolver file = do
    src <- words <$> readFileUTF8' file
    return $ Map.fromList $ ("integer-simple",Nothing) : f src
    where
        f (x:('=':'=':y):zs) = (x,Just $ dropWhileEnd (== ',') y) : f zs
        f (x:"installed,":zs) = (x,Nothing) : f zs
        f (_:xs) = f xs
        f [] = []


cabalDepends :: FilePath -> IO [PackageName]
cabalDepends file = do
    bs <- BS.readFile file
    return $ case parseGenericPackageDescriptionMaybe bs of
        Just x | Just x <- condLibrary x -> map (unPackageName . depPkgName) $ treeConstraints x
        _ -> return []

treeConstraints :: CondTree a [b] c -> [b]
treeConstraints (CondNode _ b cs) = b ++ concatMap branch  cs
    where branch (CondBranch _ t f) = treeConstraints t ++ maybe [] treeConstraints f
