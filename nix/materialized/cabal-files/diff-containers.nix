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
      specVersion = "3.0";
      identifier = { name = "diff-containers"; version = "1.3.0.0"; };
      license = "Apache-2.0";
      copyright = "2022-2024 Input Output Global Inc (IOG).";
      maintainer = "operations@iohk.io, Joris Dral (joris@well-typed.com)";
      author = "Joris Dral";
      homepage = "";
      url = "";
      synopsis = "Diffs for containers";
      description = "Diffs for containers.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."monoid-subclasses" or (errorHandler.buildDepError "monoid-subclasses"))
          (hsPkgs."nonempty-containers" or (errorHandler.buildDepError "nonempty-containers"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
        ];
        buildable = true;
      };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."diff-containers" or (errorHandler.buildDepError "diff-containers"))
            (hsPkgs."nonempty-containers" or (errorHandler.buildDepError "nonempty-containers"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-classes" or (errorHandler.buildDepError "quickcheck-classes"))
            (hsPkgs."quickcheck-monoid-subclasses" or (errorHandler.buildDepError "quickcheck-monoid-subclasses"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/diff-containers-1.3.0.0.tar.gz";
      sha256 = "f826223608e030e658caf6a5569528654aa4937886b517e846d29b0f7c5ff05b";
    });
  }) // {
    package-description-override = "cabal-version:   3.0\nname:            diff-containers\nversion:         1.3.0.0\nsynopsis:        Diffs for containers\ndescription:     Diffs for containers.\nlicense:         Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\nextra-doc-files:\n  CHANGELOG.md\n  README.md\n\ncopyright:       2022-2024 Input Output Global Inc (IOG).\nauthor:          Joris Dral\nmaintainer:      operations@iohk.io, Joris Dral (joris@well-typed.com)\ncategory:        Data Structures\nbuild-type:      Simple\ntested-with:\n  GHC ==8.10 || ==9.2 || ==9.4 || ==9.6 || ==9.8 || ==9.10 || ==9.12\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/anti-diffs\n  subdir:   diff-containers\n\nsource-repository this\n  type:     git\n  location: https://github.com/input-output-hk/anti-diffs\n  subdir:   diff-containers\n  tag:      diff-containers-1.3.0.0\n\ncommon options\n  default-language: Haskell2010\n  ghc-options:\n    -Wall -Wcompat -Wincomplete-uni-patterns\n    -Wincomplete-record-updates -Wpartial-fields -Widentities\n    -Wredundant-constraints -Wmissing-export-lists -Wunused-packages\n    -Wno-unticked-promoted-constructors\n\nlibrary\n  import:          options\n  hs-source-dirs:  src\n  exposed-modules:\n    Data.Map.Diff.Strict\n    Data.Map.Diff.Strict.Internal\n\n  other-modules:   Data.Sequence.NonEmpty.Extra\n  build-depends:\n    , base                 >=4.9 && <4.22\n    , containers\n    , monoid-subclasses\n    , nonempty-containers\n    , nothunks\n\ntest-suite test\n  import:           options\n  type:             exitcode-stdio-1.0\n  default-language: Haskell2010\n  hs-source-dirs:   test\n  main-is:          Main.hs\n  other-modules:\n    Test.Data.Map.Diff.Strict\n    Test.Util\n\n  build-depends:\n    , base                          >=4.9 && <4.22\n    , containers\n    , diff-containers\n    , nonempty-containers\n    , QuickCheck\n    , quickcheck-classes\n    , quickcheck-monoid-subclasses\n    , tasty\n    , tasty-quickcheck\n";
  }