-- This tests that reflex-dom re-exports all of reflex-dom-core's modules.
-- Without this test they easily drift.
module Main where

import Control.Monad (when)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import qualified Data.Set as Set
import Distribution.Types.PackageName (mkPackageName)
import Distribution.Compiler (CompilerFlavor (GHC))
import Distribution.ModuleName (ModuleName, components)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
import qualified Distribution.Parsec.Common as Dist
import Distribution.Parsec.ParseResult (runParseResult)
import qualified Distribution.System as Dist
import Distribution.Types.BuildInfo (buildable, defaultExtensions, defaultLanguage, hsSourceDirs, options)
import Distribution.Types.CondTree (simplifyCondTree)
import Distribution.Types.GenericPackageDescription (ConfVar (Arch, Impl, OS), condLibrary)
import Distribution.Types.Library (exposedModules, libBuildInfo, reexportedModules)
import Distribution.Types.ModuleReexport (ModuleReexport, moduleReexportOriginalName, moduleReexportOriginalPackage)
import Distribution.Utils.Generic (toUTF8BS, readUTF8File)
import System.Environment (getArgs)
import qualified System.Info

main :: IO ()
main = do
  [reflexDomFile, reflexDomCoreFile] <- getArgs
  (_, reflexDomReexports) <- parseCabalExports reflexDomFile
  (reflexDomCoreExports', _) <- parseCabalExports reflexDomCoreFile

  let
    reflexDomCoreExports = Set.fromList reflexDomCoreExports'
    reflexDomCorePackageName = mkPackageName "reflex-dom-core"
    reflexDomReexportsFromCore
      = Set.fromList
      $ mapMaybe (\x -> if let origPackage = moduleReexportOriginalPackage x
                            in isNothing origPackage || origPackage == Just reflexDomCorePackageName
                    then Just $ moduleReexportOriginalName x
                    else Nothing
                 )
      reflexDomReexports

  when (reflexDomCoreExports /= reflexDomReexportsFromCore) $ do
    error $ intercalate "\n\t"
      $ "reflex-dom does not re-export the following modules from reflex-dom-core:"
      : map
          (intercalate "." . components)
          (Set.toAscList $ reflexDomCoreExports `Set.difference` reflexDomReexportsFromCore)

  putStrLn "Test passed."

parseCabalExports :: FilePath -> IO ([ModuleName], [ModuleReexport])
parseCabalExports file = do
  contents <- readUTF8File file
  let
    (warnings, result) = runParseResult $ parseGenericPackageDescription $ toUTF8BS contents
    osConfVar = case System.Info.os of
      "linux" -> Just Dist.Linux
      "darwin" -> Just Dist.OSX
      _ -> error "Unrecognized System.Info.os"
    archConfVar = Just Dist.X86_64
    evalConfVar v = Right $ case v of
      OS osVar -> Just osVar == osConfVar
      Arch archVar -> Just archVar == archConfVar
      Impl GHC _ -> True
      _ -> False
  pure $ case condLibrary <$> result of
    Right (Just condLib) ->
      let (_, lib) = simplifyCondTree evalConfVar condLib
      in (exposedModules lib, reexportedModules lib)
    Right Nothing -> error $ "Haskell package has no library component: " <> file
    Left (_, errors) -> error $ "Failed to parse " <> file <> ":\n" <> unlines (map show errors)
