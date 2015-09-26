{-# LANGUAGE RecordWildCards, NamedFieldPuns, ViewPatterns    #-}
{-# OPTIONS_GHC -O0 -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}

-- | One of the challenges of using the GHC API for external tooling
-- is handling integration with Cabal. This library provides a simple
-- interface for configuring GHC's 'DynFlags' as Cabal would have,
-- allowing seamless tooling use on Cabal projects.
--
-- A typical usage might look like,
--
-- > import GHC
-- > import qualified GHC.Paths
-- > import qualified Distribution.Verbosity as Verbosity
-- > import GHC.Cabal
-- >
-- > main = runGhc (Just GHC.Paths.libdir) $ do
-- >     dflags <- GHC.getSessionDynFlags
-- >     -- Use default DynFlags if we aren't in a Cabal project
-- >     dflags' <- fromMaybe dflags <$> liftIO (initCabalDynFlags Verbosity.normal dflags)
-- >     GHC.setSessionDynFlags dflags'
-- >
-- >     -- Standard GHC API usage goes here
--

module SimplePlugins.Cabal where 
import SimplePlugins.Etc

import Control.Monad (guard, msum, mzero)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.Class

import qualified Distribution.Verbosity as Verbosity
import Distribution.Simple.Utils (warn, debug, findPackageDesc)
import Distribution.Simple.Program (defaultProgramConfiguration)
import qualified Distribution.Simple.Setup as Setup
import qualified Distribution.PackageDescription as PD
import qualified Distribution.PackageDescription.Parse as PD
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import qualified Distribution.Simple.LocalBuildInfo as LBI
import qualified Distribution.Simple.Configure as Configure
import qualified Distribution.Simple.Compiler as Compiler
import qualified Distribution.Simple.GHC      as CGHC
import qualified Distribution.Simple.Program.GHC as CGHC
import qualified SrcLoc
import Distribution.System (Platform(..))
import Distribution.Package (Dependency) 
import Distribution.PackageDescription.Parse (readPackageDescription) 
import Distribution.Verbosity (Verbosity) 
import Distribution.PackageDescription (GenericPackageDescription, PackageDescription, FlagAssignment)
import qualified Distribution.Simple.LocalBuildInfo as LocalBuildInfo
import Distribution.Simple.Compiler (CompilerFlavor(..), compilerInfo) 

import DynFlags (DynFlags, parseDynamicFlagsCmdLine)



-- | Modify a set of 'DynFlags' to match what Cabal would produce.
initCabalDynFlags :: Verbosity -> DynFlags -> IO (Maybe DynFlags)
initCabalDynFlags verbosity dflags0 = runMaybeT $ do
    let warnNoCabal _err = lift (warn verbosity "Couldn't find cabal file") >> mzero
    pdfile <- either warnNoCabal pure =<< lift (findPackageDesc ".")
    gpkg_descr <- lift $ PD.readPackageDescription verbosity pdfile
    lbi <- lift $ Configure.getPersistBuildConfig Setup.defaultDistPref

    let _programsConfig = defaultProgramConfiguration
    (comp', compPlatform, _programsConfig') <- lift $
        Configure.configCompilerEx (Just Compiler.GHC) Nothing Nothing
                                   (LBI.withPrograms lbi) (Verbosity.lessVerbose verbosity)

    -- TODO: is any of this correct?
    let pkg_descr = case finalizePackageDescription
                             [] (const True) compPlatform (Compiler.compilerInfo comp')
                             [] gpkg_descr of
                        Right (pd,_) -> pd
                        -- This shouldn't happen since we claim dependencies can always be satisfied
                        Left _err     -> error "missing dependencies"
    let comp :: Maybe (PD.BuildInfo, LBI.ComponentLocalBuildInfo)
        comp = msum [libraryComp, executableComp]

        libraryComp = do
            lib <- PD.library pkg_descr
            let bi = PD.libBuildInfo lib
            guard $ PD.buildable bi
            return (bi, LBI.getComponentLocalBuildInfo lbi LBI.CLibName)

        executableComp = msum $ flip map (PD.executables pkg_descr) $ \exec->do
            let bi = PD.buildInfo exec
            guard $ PD.buildable bi
            return (bi, LBI.getComponentLocalBuildInfo lbi (LBI.CExeName $ PD.exeName exec))

    case comp of
      Just (bi, clbi) -> lift $ initCabalDynFlags' verbosity lbi bi clbi dflags0
      Nothing         -> do
          lift $ warn verbosity $ "Found no buildable components in "++pdfile
          mzero

initCabalDynFlags' :: Verbosity -> LBI.LocalBuildInfo
                   -> PD.BuildInfo -> LBI.ComponentLocalBuildInfo
                   -> DynFlags -> IO DynFlags
initCabalDynFlags' verbosity lbi bi clbi dflags0 = do
    debug verbosity $ "initCabalDynFlags': Flags = "++show rendered
    (dflags, _leftovers, warnings) <- DynFlags.parseDynamicFlagsCmdLine dflags0 (map SrcLoc.noLoc rendered)
    putStrLn $ unlines $ map SrcLoc.unLoc warnings
    return dflags
  where
    baseOpts = CGHC.componentGhcOptions verbosity lbi bi clbi (LBI.buildDir lbi)
    rendered = CGHC.renderGhcOptions (LBI.compiler lbi) baseOpts


data CabalEnv = CabalEnv
 { _cabalFlags        :: FlagAssignment
 , _cabalSatisfiable  :: (Dependency -> Bool)
 , _cabalPlatform     :: Platform 
 , _cabalCompiler     :: Compiler.CompilerInfo
 , _cabalConstraints  :: [Dependency]
 } 

runPackageDescriptionIO :: Verbosity -> FilePath -> IO (Maybe PackageDescription) 
runPackageDescriptionIO verbosity filepath
 = runPackageDescription <$> readPackageDescription verbosity filepath <*> getCabalEnv verbosity filepath 

runPackageDescription :: GenericPackageDescription -> CabalEnv -> Maybe PackageDescription
runPackageDescription description CabalEnv{..} = description'
 where 
 description' = (fmap fst . either2maybe) (finalizePackageDescription _cabalFlags _cabalSatisfiable _cabalPlatform _cabalCompiler _cabalConstraints description) 

getCabalEnv :: Verbosity -> FilePath -> IO CabalEnv -- TODO
getCabalEnv verbosity _filepath = do 
 local_build_info <- Configure.getPersistBuildConfig Setup.defaultDistPref

 (  compilerInfo -> _cabalCompiler
  , _cabalPlatform
  , _
  ) <- Configure.configCompilerEx (Just GHC) Nothing Nothing (LocalBuildInfo.withPrograms local_build_info) verbosity 

 let _cabalFlags = [] 
 let _cabalSatisfiable = (const True)
 let _cabalConstraints = [] 

 return CabalEnv{..} 

