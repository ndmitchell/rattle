{-# LANGUAGE TemplateHaskell #-}

-- | Try performing a Stack-like install.
module Test.Example.Stack(main) where

import Development.Rattle
import Development.Shake.FilePath
import System.IO.Extra
import Language.Haskell.TH
import qualified Data.ByteString as BS
import qualified Development.Shake.Command as C
import Data.Maybe
import Control.Monad.Extra
import System.Environment
import System.Directory.Extra
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

haskell :: (Read a, Show a) => String -> Q (TExp (a -> IO ())) -> (a -> Run ())
haskell name act v = do
    e <- liftIO $ runQ act
    let file = name <.> "hs"
    liftIO $ unlessM (doesFileExist file) $
        liftIO $ writeFile (name <.> "hs") $ unlines
            ["import System.Environment"
            ,"import System.IO"
            ,"import System.Directory"
            ,"import Development.Shake.Command"
            ,"import System.FilePath.Windows"
            ,"import System.FilePath.Posix"
            ,"import Data.List as Data.OldList"
            ,"import System.IO.Extra"
            ,"import Data.Foldable"
            ,"import Data.Functor"
            ,"import System.Directory.Extra"
            ,"import GHC.List"
            ,"import Development.Shake.Command as Development.Shake.Internal.CmdOption"
            ,"import System.Directory.Extra as System.Directory.Internal.Common"
            ,"import Data.List.Extra"
            ,"import GHC.Base"
            ,"import GHC.Classes"
            ,"main :: IO ()"
            ,"main = do [x] <- System.Environment.getArgs; body (read x)"
            ,"body = " ++ pprint (unType e)
            ]
    cmd "runhaskell" file (show v)


main :: IO ()
main = do
    args <- getArgs
    unsetEnv "GHC_PACKAGE_PATH"
    tdir <- canonicalizePath =<< getTemporaryDirectory
    let ignore = ["**/hackage-security-lock", "**/package.cache.lock", tdir ++ "/**"]
    rattleRun rattleOptions{rattleCmdOptions=[toCmdOption $ Ignored ignore]} $
        stack "nightly-2019-09-29" $ args ++ ["cereal" | null args]

{-
haskell :: a -> Q (TExp (a -> IO ())) -> Run ()
haskell arg act = do
-}

cabalUnpack = haskell "unpack" [|| \dir -> do
    removePathForcibly dir
    C.cmd "cabal unpack" dir
    ||]

cabalBuild = haskell "build" [|| \(dir, name) -> do
    C.cmd_ (Cwd dir) "cabal v1-build" ("lib:" ++ name)
    xs <- filter (\x -> takeExtension x == ".conf") <$> listFiles (dir </> "dist/package.conf.inplace")
    pwd <- getCurrentDirectory
    forM_ xs $ \x -> do
        src <- readFileUTF8' x
        writeFileUTF8 x $ replace (addTrailingPathSeparator pwd) "" src
    C.cmd "ghc-pkg recache" ("--package-db=" ++ dir </> "dist/package.conf.inplace")
    ||]


installPackage :: (PackageName -> Run (Maybe PackageVersion)) -> FilePath -> [String] -> PackageName -> PackageVersion -> Run ()
installPackage dep config flags name version = do
    let dir = name ++ "-" ++ version

    cabalUnpack dir

    -- cmd "pipeline rm -rf" dir "&& cabal unpack" dir

    depends <- liftIO $ cabalDepends $ dir </> name <.> "cabal"
    depends <- return $ delete name $ nubSort depends
    dependsVer <- forP depends dep
    cmd (Cwd dir) "cabal v1-configure" flags
        "--disable-library-profiling --disable-optimisation"
        ["--package-db=../" ++ n ++ "-" ++ v ++ "/dist/package.conf.inplace" | (n, Just v) <- zip depends dependsVer]

    cabalBuild (dir, name)
    -- cmd (Cwd dir) "cabal v1-build" ("lib:" ++ name)


extraConfigureFlags :: IO [String]
extraConfigureFlags = do
    -- Cabal create dist/setup-config during configure
    -- That serialises a LocalBuildInfo, which with withPrograms contains ConfiguredProgram
    -- The ConfiguredProgram contains programMonitorFiles which ends up containing a list of all directories the binary
    -- _might_ be in. Unfortunately, Cabal puts the current directory in that list.
    -- However, if we pass --with-hscolour=... then it doesn't bother looking for the program, so programMonitorFiles is
    -- empty and thus doesn't change between computers.
    let progs = ["hscolour","alex","happy","ghc","cpphs","doctest"]
    flip mapMaybeM progs $ \prog -> do
        location <- findExecutable prog
        -- WARNING: If this returns Nothing (because you don't have doctest installed, for instance)
        --          your cache will not be stable.
        return $ fmap (\x -> "--with-" ++ prog ++ "=" ++ x) location


stack :: String -> [PackageName] -> Run ()
stack resolver packages = do
    let config = resolver <.> "config"
    -- Shell below is to hack around an fsatrace issue
    cmd Shell "curl -sSL" ("https://www.stackage.org/" ++ resolver ++ "/cabal.config") "-o" config
    versions <- liftIO $ readResolver config
    flags <- liftIO extraConfigureFlags
    let askVersion x = fromMaybe (error $ "Don't know version for " ++ show x) $ Map.lookup x versions
    needPkg <- memoRec $ \needPkg name -> do
        let v = askVersion name
        whenJust v $ installPackage needPkg config flags name
        return v
    forP_ packages needPkg


readResolver :: FilePath -> IO (Map.HashMap PackageName (Maybe PackageVersion))
readResolver file = do
    src <- words <$> readFileUTF8' file
    -- base and bytestring are here because of https://github.com/commercialhaskell/stackage/issues/4862
    return $ Map.fromList $ ("integer-simple",Nothing) : ("base",Nothing) : ("bytestring",Nothing) : f src
    where
        f (x:('=':'=':y):zs) = (x,Just $ dropWhileEnd (== ',') y) : f zs
        f (x:"installed,":zs) = (x,Nothing) : f zs
        f (_:xs) = f xs
        f [] = []


cabalDepends :: FilePath -> IO [PackageName]
cabalDepends file = do
    bs <- BS.readFile file
    return $ case parseGenericPackageDescriptionMaybe bs of
        Just x -> map (unPackageName . depPkgName) $ concatMap treeConstraints $
            -- we only build the library, but configure requires all the executables to
            -- have their dependencies available
            maybeToList (void <$> condLibrary x) ++ map (void . snd) (condExecutables x)
        _ -> return []

treeConstraints :: CondTree a [b] c -> [b]
treeConstraints (CondNode _ b cs) = b ++ concatMap branch  cs
    where branch (CondBranch _ t f) = treeConstraints t ++ maybe [] treeConstraints f
