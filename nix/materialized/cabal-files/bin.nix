{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = {};
    package = {
      specVersion = "2.2";
      identifier = { name = "bin"; version = "0.1.4"; };
      license = "GPL-2.0-or-later";
      copyright = "(c) 2019-2021 Oleg Grenrus";
      maintainer = "Oleg.Grenrus <oleg.grenrus@iki.fi>";
      author = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      homepage = "https://github.com/phadej/vec";
      url = "";
      synopsis = "Bin: binary natural numbers.";
      description = "This package provides /binary natural numbers/ (\"Data.Bin\");\nalso utilities to work on the type level with @DataKinds@ (\"Data.Type.Bin\").\n\n@\ndata Bin\n\\    = BZ       -- ^ zero\n\\    | BP BinP  -- ^ non-zero\n\ndata BinP\n\\    = BE       -- ^ one\n\\    | B0 BinP  -- ^ double\n\\    | B1 BinP  -- ^ double plus 1\n@\n\nThere are /ordinals/ in \"Data.Bin.Pos\" module, as well as\nfixed width integers in \"Data.Wrd\".\n\nAnother implementation is at <https://hackage.haskell.org/package/nat>,\nthis differs in naming, and provides promoted variant.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."fin" or (errorHandler.buildDepError "fin"))
          (hsPkgs."boring" or (errorHandler.buildDepError "boring"))
          (hsPkgs."dec" or (errorHandler.buildDepError "dec"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."some" or (errorHandler.buildDepError "some"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/bin-0.1.4.tar.gz";
      sha256 = "f77e955b143a87e34aa12c21d3c6a51e9723cf93cca79549bd525c83952dbf2a";
    });
  }) // {
    package-description-override = "cabal-version:      2.2\nname:               bin\nversion:            0.1.4\nx-revision:         2\nsynopsis:           Bin: binary natural numbers.\ncategory:           Data, Dependent Types, Singletons, Math\ndescription:\n  This package provides /binary natural numbers/ (\"Data.Bin\");\n  also utilities to work on the type level with @DataKinds@ (\"Data.Type.Bin\").\n  .\n  @\n  data Bin\n  \\    = BZ       -- ^ zero\n  \\    | BP BinP  -- ^ non-zero\n  .\n  data BinP\n  \\    = BE       -- ^ one\n  \\    | B0 BinP  -- ^ double\n  \\    | B1 BinP  -- ^ double plus 1\n  @\n  .\n  There are /ordinals/ in \"Data.Bin.Pos\" module, as well as\n  fixed width integers in \"Data.Wrd\".\n  .\n  Another implementation is at <https://hackage.haskell.org/package/nat>,\n  this differs in naming, and provides promoted variant.\n\nhomepage:           https://github.com/phadej/vec\nbug-reports:        https://github.com/phadej/vec/issues\nlicense:            GPL-2.0-or-later\nlicense-file:       LICENSE\nauthor:             Oleg Grenrus <oleg.grenrus@iki.fi>\nmaintainer:         Oleg.Grenrus <oleg.grenrus@iki.fi>\ncopyright:          (c) 2019-2021 Oleg Grenrus\nbuild-type:         Simple\nextra-source-files: ChangeLog.md\ntested-with:\n  GHC ==8.6.5\n   || ==8.8.4\n   || ==8.10.7\n   || ==9.0.2\n   || ==9.2.8\n   || ==9.4.8\n   || ==9.6.6\n   || ==9.8.4\n   || ==9.10.1\n   || ==9.12.2\n\nsource-repository head\n  type:     git\n  location: https://github.com/phadej/vec.git\n  subdir:   bin\n\nlibrary\n  default-language: Haskell2010\n  hs-source-dirs:   src\n  ghc-options:      -Wall -fprint-explicit-kinds\n  exposed-modules:\n    Data.Bin\n    Data.Bin.Pos\n    Data.BinP\n    Data.BinP.PosP\n    Data.Type.Bin\n    Data.Type.BinP\n    Data.Wrd\n\n  other-modules:    TrustworthyCompat\n\n  -- GHC boot libs\n  build-depends:\n    , base     >=4.12.0.0 && <4.22\n    , deepseq  >=1.4.4.0  && <1.6\n\n  -- siblings\n  build-depends:    fin ^>=0.3.1\n\n  -- other dependencies\n  build-depends:\n    , boring      ^>=0.2.2\n    , dec         ^>=0.0.6\n    , hashable    ^>=1.4.4.0 || ^>=1.5.0.0\n    , QuickCheck  ^>=2.14.2  || ^>=2.15\n    , some        ^>=1.0.6\n\n  if impl(ghc >=9.0)\n    -- these flags may abort compilation with GHC-8.10\n    -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3295\n    ghc-options: -Winferred-safe-imports -Wmissing-safe-haskell-mode\n\n-- dump-core\n--  build-depends: dump-core\n--  ghc-options: -fplugin=DumpCore -fplugin-opt DumpCore:core-html\n";
  }