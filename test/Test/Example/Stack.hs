
-- | Try performing a Stack-like install.
module Test.Example.Stack(main) where

import Development.Rattle
import Development.Shake.FilePath
import System.IO.Extra
import qualified Data.ByteString as BS
import Distribution.PackageDescription.Parsec
import Distribution.Types.CondTree
import Distribution.Types.GenericPackageDescription
import Distribution.Types.Dependency
import Distribution.Types.PackageName(unPackageName)
import Data.Maybe
import Control.Monad.Extra
import Data.List.Extra
import qualified Data.HashMap.Strict as Map


type PackageName = String
type PackageVersion = String

main :: IO ()
main = rattle rattleOptions $ stack "nightly-2019-05-15" ["aeson","shake"]


installPackage :: (PackageName -> Run ()) -> FilePath -> PackageName -> PackageVersion -> Run ()
installPackage dep config name version = do
    cmd "cabal unpack" (name ++ "-" ++ version)
    depends <- liftIO $ cabalDepends $ name ++ "-" ++ version </> name <.> "cabal"
    forP_ depends dep
    cmd "cabal configure"
    cmd "cabal build"


stack :: String -> [PackageName] -> Run ()
stack resolver packages = when False $ withIgnore ["**/hackage-security-lock"] $ do
    let config = resolver <.> "config"
    cmd "curl" ("https://www.stackage.org/" ++ resolver ++ "/cabal.config") "-o" config
    versions <- liftIO $ readResolver config
    let askVersion x = fromMaybe (error $ "Don't know version for " ++ show x) $ Map.lookup x versions
    needPkg <- memoRec $ \needPkg name -> whenJust (askVersion name) $ installPackage needPkg config name
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
