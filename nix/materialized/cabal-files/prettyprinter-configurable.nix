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
      specVersion = "2.4";
      identifier = {
        name = "prettyprinter-configurable";
        version = "1.36.0.0";
      };
      license = "NONE";
      copyright = "";
      maintainer = "plutus@iohk.io";
      author = "David Luposchainsky, effectfully";
      homepage = "https://github.com/input-output-hk/plutus/tree/master/prettyprinter-configurable/";
      url = "";
      synopsis = "Configurable pretty-printing";
      description = "";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
        ];
        buildable = true;
      };
      tests = {
        "prettyprinter-configurable-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."parser-combinators" or (errorHandler.buildDepError "parser-combinators"))
            (hsPkgs."prettyprinter-configurable" or (errorHandler.buildDepError "prettyprinter-configurable"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-text" or (errorHandler.buildDepError "quickcheck-text"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/prettyprinter-configurable-1.36.0.0.tar.gz";
      sha256 = "eaca6ad94eb9342471517b6fef277b56fda7ca1ca2abe928c5ff866d17927b63";
    });
  }) // {
    package-description-override = "cabal-version:      2.4\nname:               prettyprinter-configurable\nversion:            1.36.0.0\nsynopsis:           Configurable pretty-printing\nhomepage:\n  https://github.com/input-output-hk/plutus/tree/master/prettyprinter-configurable/\n\nlicense-files:\n  LICENSE\n  NOTICE\n\nauthor:             David Luposchainsky, effectfully\nmaintainer:         plutus@iohk.io\ncategory:           User Interfaces, Text\nbuild-type:         Simple\nextra-source-files: README.md\n\ncommon lang\n  default-language:   Haskell2010\n  default-extensions:\n    DeriveFoldable\n    DeriveFunctor\n    DeriveGeneric\n    DeriveLift\n    DeriveTraversable\n    DerivingStrategies\n    DerivingVia\n    ExplicitForAll\n    FlexibleContexts\n    GeneralizedNewtypeDeriving\n    ImportQualifiedPost\n    ScopedTypeVariables\n    StandaloneDeriving\n\n  ghc-options:\n    -Wall -Wnoncanonical-monad-instances -Wincomplete-uni-patterns\n    -Wincomplete-record-updates -Wredundant-constraints -Widentities\n    -Wunused-packages -Wmissing-deriving-strategies\n\nlibrary\n  import:          lang\n  hs-source-dirs:  src\n  exposed-modules:\n    Text.Fixity\n    Text.Fixity.Internal\n    Text.Pretty\n    Text.PrettyBy\n    Text.PrettyBy.Default\n    Text.PrettyBy.Fixity\n    Text.PrettyBy.Internal\n    Text.PrettyBy.Internal.Utils\n    Text.PrettyBy.Monad\n\n  build-depends:\n    , base           >=4.9 && <5\n    , microlens\n    , mtl\n    , prettyprinter\n    , text\n\n  ghc-options:     -O2\n\ntest-suite prettyprinter-configurable-test\n  import:         lang\n  type:           exitcode-stdio-1.0\n  main-is:        Main.hs\n  ghc-options:    -threaded -rtsopts -with-rtsopts=-N\n  hs-source-dirs: test\n  other-modules:\n    Default\n    Expr\n    NonDefault\n    Universal\n\n  build-depends:\n    , base                        >=4.9   && <5\n    , megaparsec\n    , parser-combinators\n    , prettyprinter-configurable  ^>=1.36\n    , QuickCheck\n    , quickcheck-text\n    , tasty\n    , tasty-hunit\n    , tasty-quickcheck\n    , text\n\n  ghc-options:    -threaded -rtsopts -with-rtsopts=-N\n";
  }