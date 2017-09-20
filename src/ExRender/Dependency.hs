-- Copyright © 2015 Mykola Orliuk <virkony@gmail.com>
-- Distributed under the terms of the GNU General Public License v2

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ViewPatterns, PatternSynonyms, LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module ExRender.Dependency (CabalDependency(..), DepScope(..), DepType(..), Flag(..), toRawDep) where

import Control.Lens
import Control.Monad.ST
import Data.Default
import Data.List
import Data.Maybe
import Data.STRef
import Distribution.Text
import Distribution.Package
import Distribution.Version

import ExRender.Base


-- |Extended cabal dependencies, maintaining all
-- the info we need.
data CabalDependency = CabalDep DepScope Flag Dependency

data DepScope = LibS  DepType
              | ExecS DepType
              | TestS DepType
              | SetupS   -- custom-setup -> setup-depends

data DepType = BuildRunT   -- build-depends (haskell only)
             | BuildtoolT  -- build-tools (haskell only)
             | PkgT        -- pkgconfig-depends
             | ExtraLibsT  -- extra-libraries

data Flag = Flag String
          | None


getDepType :: CabalDependency -> Maybe DepType
getDepType (CabalDep (LibS d) _ _)  = Just d
getDepType (CabalDep (ExecS d) _ _) = Just d
getDepType (CabalDep (TestS d) _ _) = Just d
getDepType _                        = Nothing

pattern HasDepType :: CabalDependency -> CabalDependency
pattern HasDepType d <- ((\x -> (isJust (getDepType x), x)) -> (True, d))


toRawDep :: CabalDependency -> Dependency
toRawDep (CabalDep _ _ d) = d


-- |Maps the cabal dependency structure into a format more close
-- to exheres.
data ExDepTree = ExDepTree {
    _flagsDeps :: [FlagDeps]
  , _exDeps :: ExDeps
  }
  deriving (Show)

instance Default ExDepTree where
  def = ExDepTree [] def


data FlagDeps = FlagDeps {
    _flagName :: String
  , _flagDep :: [ExDeps]
  }
  deriving (Show)


data ExDeps = ExDeps {
    _haskellLib  :: [Dependency] -- haskell_lib_dependencies(), build+run
  , _haskellBin  :: [Dependency] -- haskell_bin_dependencies(), build+run
  , _haskellTest :: [Dependency] -- haskell_test_dependencies(), test
  , _build       :: BuildDeps    -- build deps
  , _test        :: TestDepsSys  -- other test deps (system)
  }
  deriving (Show)

instance Default ExDeps where
  def = ExDeps [] [] [] def def


-- |Build-only dependencies.
data BuildDeps = Build {
    _bPkg    :: [Dependency] -- system
  , _bExtraL :: [Dependency] -- system
  , _bBuildT :: [Dependency] -- haskell
  , _bSetupD :: [Dependency] -- haskell
  }
  deriving (Show)

instance Default BuildDeps where
  def = Build [] [] [] []


-- |Test dependencies that are not haskell ones.
data TestDepsSys = TDS {
    _tPkg    :: [Dependency]
  , _tExtra  :: [Dependency]
  }
  deriving (Show)

instance Default TestDepsSys where
  def = TDS [] []


makeLenses ''ExDepTree
makeLenses ''FlagDeps
makeLenses ''ExDeps
makeLenses ''BuildDeps
makeLenses ''TestDepsSys


toExDependencies :: [CabalDependency] -> ExDepTree
toExDependencies cs = go cs def
  where
    go :: [CabalDependency] -> ExDepTree -> ExDepTree
    go [] exd = exd

    go ((CabalDep (LibS BuildRunT) None d):csd) exd
      = go csd (over (exDeps . haskellLib) (d:) $ exd)
    go ((CabalDep (ExecS BuildRunT) None d):csd) exd
      = go csd (over (exDeps . haskellBin) (d:) $ exd)
    go ((CabalDep (TestS BuildRunT) None d):csd) exd
      = go csd (over (exDeps . haskellTest) (d:) $ exd)

    go ((CabalDep (LibS BuildtoolT) None d):csd) exd
      = go csd (over (exDeps . build . bBuildT) (d:) $ exd)
    go ((CabalDep (ExecS BuildtoolT) None d):csd) exd
      = go csd (over (exDeps . build . bBuildT) (d:) $ exd)
    go ((CabalDep (TestS BuildtoolT) None d):csd) exd
      = go csd (over (exDeps . haskellTest) (d:) $ exd)

    go ((CabalDep (LibS PkgT) None d):csd) exd
      = go csd (over (exDeps . build . bPkg) (d:) $ exd)
    go ((CabalDep (ExecS PkgT) None d):csd) exd
      = go csd (over (exDeps . build . bPkg) (d:) $ exd)
    go ((CabalDep (TestS PkgT) None d):csd) exd
      = go csd (over (exDeps . test . tPkg) (d:) $ exd)

    go ((CabalDep (LibS ExtraLibsT) None d):csd) exd
      = go csd (over (exDeps . build . bExtraL) (d:) $ exd)
    go ((CabalDep (ExecS ExtraLibsT) None d):csd) exd
      = go csd (over (exDeps . build . bExtraL) (d:) $ exd)
    go ((CabalDep (TestS ExtraLibsT) None d):csd) exd
      = go csd (over (exDeps . test . tExtra) (d:) $ exd)

    go ((CabalDep SetupS None d):csd) exd
      = go csd (over (exDeps . build . bSetupD) (d:) $ exd)

    go (_:csd) exd = go csd exd

    {- addDep :: CabalDependency -> ExDepTree -> ExDepTree -}
    {- addDep (CabalDep (ExecS BuildtoolT) None d) exd -}
      {- = over (exDeps . haskellLib) (d:) $ exd -}
    {- addDep (CabalDep (ExecS BuildtoolT) (Flag name) d) exd -}
      {- = case existingFlag = find (\(FlagDeps n _) n == name) of -}
             {- Nothing -> -}
             {- Just f -}
        {- in over (exDeps . haskellLib) (d:) $ exd -}

    addToFlag :: [FlagDeps] -> String -> ExDeps -> [FlagDeps]
    addToFlag flags fname' deps' = runST $ do
      ref <- newSTRef False -- whether we inserted successfully
      go flags fname' deps' ref
      where
        go :: [FlagDeps] -> String -> ExDeps -> STRef s Bool -> ST s [FlagDeps]
        go [] fname deps ref = do
          v <- readSTRef ref
          if v then return []
               else return [FlagDeps fname [deps]]
        go (p@(FlagDeps n ds):fs) fname deps ref
          | n == fname = do
              writeSTRef ref True
              return $ (FlagDeps n (deps:ds)):fs
          | otherwise  = fmap (p:) $ go fs fname deps ref


instance ExRender LowerBound where
    exDisp (LowerBound v InclusiveBound) = ">=" <> disp v
    exDisp (LowerBound v ExclusiveBound) = ">" <> disp v

instance ExRender UpperBound where
    exDisp (UpperBound v InclusiveBound) = "<=" <> disp v
    exDisp (UpperBound v ExclusiveBound) = "<" <> disp v
    exDisp x = error $ "Unsupported UpperBound: " ++ show x

-- | Render some of VersionInterval's that can be represented with a single condition and thus suitable for using in disjunction list.
--
-- >>> map maybeExVersion $ asVersionIntervals (fromJust $ simpleParse ">=1.0 || ==0.1.*" :: VersionRange)
-- [Just =0.1*,Just >=1.0]
maybeExVersion :: VersionInterval -> Maybe Doc
maybeExVersion = \case
    -- >=x && <=x
    (LowerBound a InclusiveBound, UpperBound b InclusiveBound)
        | a == b -> Just $ char '=' <> disp a

    -- <x, <=x
    (LowerBound (Version [0] []) InclusiveBound, ub) -> Just $ exDisp ub

    -- >=x, >x
    (lb, NoUpperBound) -> Just $ exDisp lb

    (LowerBound (Version [] _) _, _) -> Nothing
    (_, UpperBound (Version [] _) _) -> Nothing
    -- >=x.y && <x.y'
    (LowerBound v@(Version a []) InclusiveBound, UpperBound (Version b []) ExclusiveBound)
        | init a == init b && succ (last a) == last b ->
            Just $ char '=' <> disp v <> char '*'

    (LowerBound (Version [_] _) _, _) -> Nothing
    -- >=x.y.z && <x.y'
    (LowerBound v@(Version a []) InclusiveBound, UpperBound (Version b []) ExclusiveBound)
        | init a' == init b && succ (last a') == last b ->
                Just $ text "~>" <> disp v
            where a' = init a

    _ -> Nothing

-- | Transform VersionInterval in a sequence of disjunctions
--
-- >>> map exVersions $ asVersionIntervals (fromJust $ simpleParse ">=1.0" :: VersionRange)
-- [[>=1.0]]
-- >>> map exVersions $ asVersionIntervals (fromJust $ simpleParse ">=1.0 && <1.3" :: VersionRange)
-- [[=1.0*,=1.1*,=1.2*]]
-- >>> map exVersions $ asVersionIntervals (fromJust $ simpleParse ">=1.0 && <=1.3" :: VersionRange)
-- [[=1.0*,=1.1*,=1.2*,=1.3]]
-- >>> map exVersions $ asVersionIntervals (fromJust $ simpleParse ">=1 && <=1.3" :: VersionRange)
-- [[=1,=1.0*,=1.1*,=1.2*,=1.3]]
-- >>> map exVersions $ asVersionIntervals (fromJust $ simpleParse ">=1 && <=1.0.3" :: VersionRange)
-- [[=1,=1.0,=1.0.0*,=1.0.1*,=1.0.2*,=1.0.3]]
exVersions :: VersionInterval -> [Doc]
exVersions = \case
    (maybeExVersion -> Just x) -> [x]

    -- ... && <=x.b
    (lb, UpperBound v InclusiveBound) ->
        exVersions (lb, UpperBound v ExclusiveBound) ++ [char '=' <> disp v]

    -- >=x.a && <x.b
    (LowerBound va@(Version a _) InclusiveBound, ub@(UpperBound (Version b _) ExclusiveBound))
        | init a == init b -> do
            c <- [init a ++ [i] | i <- [last a .. last b - 1]]
            return $ char '=' <> disp (Version c []) <> char '*'
        | length a < length b ->
            char '=' <> disp va : exVersions (LowerBound (Version (a ++ [0]) []) InclusiveBound, ub)
    _ -> []

instance ExRender VersionInterval where
    exDisp (LowerBound (Version [0] []) InclusiveBound, NoUpperBound) = empty
    exDisp (maybeExVersion -> Just exVi) = exVi
    exDisp (lb, ub) = exDisp lb <> char '&' <> exDisp ub

instance ExRender VersionRange where
    exDisp vr = case asVersionIntervals vr of
        [vi] -> nbrackets $ exDisp vi
        (concatMap exVersions -> exVis) | not $ null exVis -> nbrackets . hcat $ punctuate (char '|') exVis
        _ -> error $ "Unsupported version range: " ++ display vr

{- instance ExRender ExDependency where -}
    {- exDisp (ExDep _ System _ (Dependency n vr)) -}
      {- = maybe ("??? " <> disp n) (\(_, x) -> text x <> exDisp vr) -}
         {- $ find (\(x, _) -> x == unPackageName n) pkgConfigDepMap -}
    {- exDisp (ExDep _ _ _ (Dependency n vr)) -}
      {- = "dev-haskell/" <> disp n <> exDisp vr -}


-- | Map a pkg-config dependency package name to an exherbo package name.
pkgConfigDepMap :: [(String, String)]
pkgConfigDepMap = [
  ("gtk+-2.0", "x11-libs/gtk+:2"),
  ("gthread-2.0", "dev-libs/glib")
  ]


-- $setup
--
-- >>> import Data.Maybe
--
-- doctest examples:
--
-- >>> exDisp (fromJust $ simpleParse ">=1.0 && <1.3" :: VersionRange)
-- [>=1.0&<1.3]
-- >>> exDisp (fromJust $ simpleParse ">=1.1 && <2" :: VersionRange)
-- [~>1.1]
-- >>> exDisp (fromJust $ simpleParse "==1.* || ==3.*" :: VersionRange)
-- [=1*|=3*]
-- >>> exDisp (fromJust $ simpleParse "==1.1.* || ==1.0.* || ==0.11.*" :: VersionRange)
-- [=0.11*|=1.0*|=1.1*]
