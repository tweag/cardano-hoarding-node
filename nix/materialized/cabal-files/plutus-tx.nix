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
      identifier = { name = "plutus-tx"; version = "1.53.1.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "michael.peyton-jones@iohk.io";
      author = "Michael Peyton Jones";
      homepage = "";
      url = "";
      synopsis = "Libraries for Plutus Tx and its prelude";
      description = "Libraries for Plutus Tx and its prelude";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."deriving-compat" or (errorHandler.buildDepError "deriving-compat"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
          (hsPkgs."plutus-core".components.sublibs.plutus-ir or (errorHandler.buildDepError "plutus-core:plutus-ir"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
        ];
        buildable = true;
      };
      sublibs = {
        "plutus-tx-testlib" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-core-testlib or (errorHandler.buildDepError "plutus-core:plutus-core-testlib"))
            (hsPkgs."plutus-core".components.sublibs.plutus-ir or (errorHandler.buildDepError "plutus-core:plutus-ir"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
          buildable = true;
        };
      };
      tests = {
        "plutus-tx-test" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hedgehog-fn" or (errorHandler.buildDepError "hedgehog-fn"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-core-testlib or (errorHandler.buildDepError "plutus-core:plutus-core-testlib"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
          buildable = if compiler.isGhcjs && true || system.isWindows
            then false
            else true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/plutus-tx-1.53.1.0.tar.gz";
      sha256 = "bb66f5064c6b5dd3f574939108b06cb541a4fbe41bf13453b495c34a35fbe346";
    });
  }) // {
    package-description-override = "cabal-version:   3.0\nname:            plutus-tx\nversion:         1.53.1.0\nlicense:         Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\nmaintainer:      michael.peyton-jones@iohk.io\nauthor:          Michael Peyton Jones\nsynopsis:        Libraries for Plutus Tx and its prelude\ndescription:     Libraries for Plutus Tx and its prelude\ncategory:        Language\nbuild-type:      Simple\nextra-doc-files: README.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/IntersectMBO/plutus\n\ncommon lang\n  default-language:   Haskell2010\n  default-extensions:\n    DeriveFoldable\n    DeriveFunctor\n    DeriveGeneric\n    DeriveLift\n    DeriveTraversable\n    ExplicitForAll\n    GeneralizedNewtypeDeriving\n    ImportQualifiedPost\n    NoImplicitPrelude\n    ScopedTypeVariables\n    StandaloneDeriving\n\n  -- See Plutus Tx readme for why we need the following flags:\n  -- -fobject-code -fno-ignore-interface-pragmas and -fno-omit-interface-pragmas\n  ghc-options:\n    -Wall -Wnoncanonical-monad-instances -Wincomplete-uni-patterns\n    -Wincomplete-record-updates -Wredundant-constraints -Widentities\n    -Wunused-packages -Wmissing-deriving-strategies -fobject-code\n    -fno-ignore-interface-pragmas -fno-omit-interface-pragmas\n\nlibrary\n  import:             lang\n  hs-source-dirs:     src\n  exposed-modules:\n    Data.Aeson.Extra\n    PlutusTx\n    PlutusTx.Applicative\n    PlutusTx.AsData\n    PlutusTx.AsData.Internal\n    PlutusTx.AssocMap\n    PlutusTx.Base\n    PlutusTx.Blueprint\n    PlutusTx.Blueprint.Argument\n    PlutusTx.Blueprint.Class\n    PlutusTx.Blueprint.Contract\n    PlutusTx.Blueprint.Definition\n    PlutusTx.Blueprint.Definition.Derive\n    PlutusTx.Blueprint.Definition.Id\n    PlutusTx.Blueprint.Definition.Internal\n    PlutusTx.Blueprint.Definition.TF\n    PlutusTx.Blueprint.Definition.Unroll\n    PlutusTx.Blueprint.Parameter\n    PlutusTx.Blueprint.PlutusVersion\n    PlutusTx.Blueprint.Preamble\n    PlutusTx.Blueprint.Purpose\n    PlutusTx.Blueprint.Schema\n    PlutusTx.Blueprint.Schema.Annotation\n    PlutusTx.Blueprint.TH\n    PlutusTx.Blueprint.Validator\n    PlutusTx.Blueprint.Write\n    PlutusTx.Bool\n    PlutusTx.BuiltinList\n    PlutusTx.Builtins\n    PlutusTx.Builtins.HasBuiltin\n    PlutusTx.Builtins.HasOpaque\n    PlutusTx.Builtins.Internal\n    PlutusTx.Code\n    PlutusTx.Coverage\n    PlutusTx.Data.AssocMap\n    PlutusTx.Data.List\n    PlutusTx.Data.List.TH\n    PlutusTx.Either\n    PlutusTx.Enum\n    PlutusTx.Eq\n    PlutusTx.ErrorCodes\n    PlutusTx.Eval\n    PlutusTx.Foldable\n    PlutusTx.Function\n    PlutusTx.Functor\n    PlutusTx.Integer\n    PlutusTx.IsData\n    PlutusTx.IsData.Class\n    PlutusTx.Lattice\n    PlutusTx.Lift\n    PlutusTx.Lift.Class\n    PlutusTx.List\n    PlutusTx.Maybe\n    PlutusTx.Monoid\n    PlutusTx.Numeric\n    PlutusTx.Optimize.Inline\n    PlutusTx.Optimize.SpaceTime\n    PlutusTx.Ord\n    PlutusTx.Plugin.Utils\n    PlutusTx.Prelude\n    PlutusTx.Ratio\n    PlutusTx.Semigroup\n    PlutusTx.Show\n    PlutusTx.Show.TH\n    PlutusTx.Sqrt\n    PlutusTx.TH\n    PlutusTx.These\n    PlutusTx.Trace\n    PlutusTx.Traversable\n    PlutusTx.Utils\n\n  other-modules:\n    PlutusTx.IsData.Instances\n    PlutusTx.IsData.TH\n    PlutusTx.Lift.Instances\n    PlutusTx.Lift.TestInstances\n    PlutusTx.Lift.TH\n    PlutusTx.Lift.THUtils\n\n  build-depends:\n    , aeson                  >=2.2\n    , aeson-pretty\n    , base                   >=4.9       && <5\n    , base16-bytestring\n    , bytestring\n    , containers\n    , data-default-class\n    , deepseq\n    , deriving-compat\n    , extra\n    , flat                   ^>=0.6\n    , formatting\n    , hashable\n    , lens\n    , memory\n    , mtl\n    , plutus-core            ^>=1.53.1.0\n    , plutus-core:plutus-ir\n    , prettyprinter\n    , serialise\n    , template-haskell       >=2.13.0.0\n    , text\n    , th-abstraction\n    , vector                 ^>=0.13.2\n\n  default-extensions: Strict\n\n  -- See Note [-fno-full-laziness in Plutus Tx]\n  ghc-options:\n    -fexpose-all-unfoldings -fno-specialise -fno-spec-constr\n    -fno-strictness -fno-unbox-strict-fields\n    -fno-unbox-small-strict-fields -fno-full-laziness\n\nlibrary plutus-tx-testlib\n  import:          lang\n  visibility:      public\n  hs-source-dirs:  testlib\n  exposed-modules:\n    Hedgehog.Laws.Common\n    Hedgehog.Laws.Eq\n    Hedgehog.Laws.Lattice\n    Hedgehog.Laws.Ord\n    PlutusTx.Test\n    PlutusTx.Test.Golden\n    PlutusTx.Test.Orphans\n    PlutusTx.Test.Run.Code\n    PlutusTx.Test.Run.Uplc\n    PlutusTx.Test.Util.Apply\n    PlutusTx.Test.Util.Compiled\n\n  build-depends:\n    , base                             >=4.9       && <5\n    , bytestring\n    , flat                             ^>=0.6\n    , hedgehog\n    , lens\n    , mtl\n    , plutus-core                      ^>=1.53.1.0\n    , plutus-core:plutus-core-testlib\n    , plutus-core:plutus-ir\n    , plutus-tx                        ^>=1.53.1.0\n    , prettyprinter\n    , serialise\n    , tasty\n    , tasty-hedgehog\n    , tasty-hunit\n    , template-haskell\n    , text\n\ntest-suite plutus-tx-test\n  import:         lang\n\n  if (impl(ghcjs) || os(windows))\n    buildable: False\n\n  type:           exitcode-stdio-1.0\n  main-is:        Spec.hs\n  ghc-options:    -threaded -rtsopts -with-rtsopts=-N\n  other-modules:\n    Blueprint.Definition.Fixture\n    Blueprint.Definition.Spec\n    Blueprint.Spec\n    List.Spec\n    Rational.Laws\n    Rational.Laws.Additive\n    Rational.Laws.Construction\n    Rational.Laws.Eq\n    Rational.Laws.Helpers\n    Rational.Laws.Module\n    Rational.Laws.Multiplicative\n    Rational.Laws.Ord\n    Rational.Laws.Other\n    Rational.Laws.Ring\n    Rational.Laws.Serialization\n    Show.Spec\n\n  hs-source-dirs: test\n  build-depends:\n    , aeson\n    , base                             >=4.9       && <5\n    , base16-bytestring\n    , bytestring\n    , cborg\n    , containers\n    , filepath\n    , hedgehog\n    , hedgehog-fn\n    , lens\n    , mtl\n    , plutus-core                      ^>=1.53.1.0\n    , plutus-core:plutus-core-testlib\n    , plutus-tx                        ^>=1.53.1.0\n    , pretty-show\n    , serialise\n    , tasty\n    , tasty-hedgehog\n    , tasty-hunit\n    , text\n";
  }