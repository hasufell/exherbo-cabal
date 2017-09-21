-- Copyright © 2015 Mykola Orliuk <virkony@gmail.com>
-- Distributed under the terms of the GNU General Public License v2

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ViewPatterns, PatternSynonyms, LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module ExRender.Dependency where

import Control.Lens
import Data.Default
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Distribution.Text
import Distribution.Package
import Distribution.Version

import ExRender.Base


-- |Extended cabal dependencies, maintaining all
-- the info we need.
-- A dependency has a scope (e.g. library, binary, ...)
-- and might depend on multiple cabal flags.
data CabalDependency = CD DepScope (Set Flag) Dependency

-- |A scope is the "section" where the dependency is defined
-- (e.g. library, binary...).
data DepScope = Lib  DepType
              | Exec DepType
              | Test DepType
              | Setup   -- custom-setup -> setup-depends

-- |A dependency can be of a specific type.
data DepType = BuildRun   -- build-depends (haskell only)
             | Buildtool  -- build-tools (haskell only)
             | Pkg        -- pkgconfig-depends
             | ExtraLibs  -- extra-libraries

-- |A cabal flag.
newtype Flag = Flag String
  deriving (Show)


toRawDep :: CabalDependency -> Dependency
toRawDep (CD _ _ d) = d


-- |Maps the cabal dependency structure into a format more close
-- to exheres. We structure the format in two parts. First, dependencies
-- that are not behind any sort of flag, second, a tree of flags with
-- potential dependencies at any depth.
data ExDepTree = EDT {
    _exDeps :: ExDeps
  , _flagsDeps :: [FlagsTree]
  }
  deriving (Show)

instance Default ExDepTree where
  def = EDT mempty []


data FlagsTree = FT {
    _flag :: Flag
  , _flagDep :: ExDeps
  , _subFlags :: [FlagsTree]
  }
  deriving (Show)


data ExDeps = ED {
    _haskellLib  :: [Dependency] -- haskell_lib_dependencies(), build+run
  , _haskellBin  :: [Dependency] -- haskell_bin_dependencies(), build+run
  , _haskellTest :: [Dependency] -- haskell_test_dependencies(), test
  , _build       :: BuildDeps    -- build deps
  , _test        :: TestDepsSys  -- other test deps (system)
  }
  deriving (Show)

instance Monoid ExDeps where
  mempty = ED [] [] [] mempty mempty
  mappend (ED hl hb ht b t) (ED hl' hb' ht' b' t')
    = ED (hl ++ hl') (hb ++ hb') (ht ++ ht') (mappend b b') (mappend t t')


-- |Build-only dependencies.
data BuildDeps = BD {
    _bPkg    :: [Dependency] -- system
  , _bExtraL :: [Dependency] -- system
  , _bBuildT :: [Dependency] -- haskell
  , _bSetupD :: [Dependency] -- haskell
  }
  deriving (Show)

instance Monoid BuildDeps where
  mempty = BD [] [] [] []
  mappend (BD p e b s) (BD p' e' b' s')
    = BD (p ++ p') (e ++ e') (b ++ b') (s ++ s')


-- |Test dependencies that are not haskell ones.
data TestDepsSys = TDS {
    _tPkg    :: [Dependency]
  , _tExtra  :: [Dependency]
  }
  deriving (Show)

instance Monoid TestDepsSys where
  mempty = TDS [] []
  mappend (TDS p e) (TDS p' e') = TDS (p ++ p') (e ++ e')


makeLenses ''ExDepTree
makeLenses ''FlagsTree
makeLenses ''ExDeps
makeLenses ''BuildDeps
makeLenses ''TestDepsSys


toExDependencies :: [CabalDependency] -> ExDepTree
toExDependencies cs = foldr addDep def cs
  where
    addDep :: CabalDependency -> ExDepTree -> ExDepTree
    addDep (CD scope fl d) exd
      = case scope of
             -- nicely mapping the cabal to exherbo structure
             Setup           -> ins (build . bSetupD)
             Test BuildRun  -> ins  haskellTest
             Test Buildtool -> ins  haskellTest
             Test Pkg       -> ins (test . tPkg)
             Test ExtraLibs -> ins (test . tExtra)
             Lib  Buildtool -> ins (build . bBuildT)
             Lib  Pkg       -> ins (build . bPkg)
             Lib  ExtraLibs -> ins (build . bExtraL)
             Exec Buildtool -> ins (build . bBuildT)
             Exec Pkg       -> ins (build . bPkg)
             Exec ExtraLibs -> ins (build . bExtraL)
             Lib  BuildRun  -> ins  haskellLib
             Exec BuildRun  -> ins  haskellBin
      where
        ins f = over (exDeps . f) (d:) $ exd
        {- ins f = case flag of -}
                     {- None   -> over (exDeps . f) (d:) $ exd -}
                     {- Flag n -> over flagsDeps -}
                                    {- (\x -> addToFlags x n $ over f (d:)) -}
                                    {- $ exd -}


-- |Adds the given ExDeps to the given list of FlagDeps. If the
-- flagname already exists in the input list, the ExDeps are added
-- to that FlagDeps element, otherwise a new FlagDeps element is created.
{- addToFlags :: [FlagDeps]         -- ^ input list -}
           {- -> String             -- ^ flagname -}
           {- -> (ExDeps -> ExDeps) -- ^ lens setter -}
           {- -> [FlagDeps] -}
{- addToFlags flags fname' setter = -}
  {- foldr (\x@(FlagDeps name deps) go b -> -}
              {- if name == fname' -}
                 {- then (FlagDeps name (setter deps)) : go True -}
                 {- else x : go b) -}
        {- (\b -> if b then [] else [FlagDeps fname' (setter mempty)]) -}
        {- flags -}
        {- False -}


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
