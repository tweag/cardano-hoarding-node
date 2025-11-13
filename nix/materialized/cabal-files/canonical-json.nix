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
      specVersion = "1.10";
      identifier = { name = "canonical-json"; version = "0.6.0.1"; };
      license = "BSD-3-Clause";
      copyright = "Copyright 2015-2018 Well-Typed LLP";
      maintainer = "duncan@well-typed.com, edsko@well-typed.com";
      author = "Duncan Coutts, Edsko de Vries";
      homepage = "https://github.com/well-typed/canonical-json";
      url = "";
      synopsis = "Canonical JSON for signing and hashing JSON values";
      description = "An implementation of Canonical JSON.\n\n<http://wiki.laptop.org/go/Canonical_JSON>\n\nThe \\\"canonical JSON\\\" format is designed to provide\nrepeatable hashes of JSON-encoded data. It is designed\nfor applications that need to hash, sign or authenitcate\nJSON data structures, including embedded signatures.\n\nCanonical JSON is parsable with any full JSON parser, and\nit allows whitespace for pretty-printed human readable\npresentation, but it can be put into a canonical form\nwhich then has a stable serialised representation and\nthus a stable hash.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
        ];
        buildable = true;
      };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."canonical-json" or (errorHandler.buildDepError "canonical-json"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          ];
          buildable = true;
        };
      };
      benchmarks = {
        "parse-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."canonical-json" or (errorHandler.buildDepError "canonical-json"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/canonical-json-0.6.0.1.tar.gz";
      sha256 = "33df39d9058d33357956cdf7f911184a26da20c49b90f844ec6374f6bf5ace7e";
    });
  }) // {
    package-description-override = "name:                canonical-json\r\nversion:             0.6.0.1\r\nx-revision: 1\r\nsynopsis:            Canonical JSON for signing and hashing JSON values\r\ndescription:         An implementation of Canonical JSON.\r\n                     .\r\n                     <http://wiki.laptop.org/go/Canonical_JSON>\r\n                     .\r\n                     The \\\"canonical JSON\\\" format is designed to provide\r\n                     repeatable hashes of JSON-encoded data. It is designed\r\n                     for applications that need to hash, sign or authenitcate\r\n                     JSON data structures, including embedded signatures.\r\n                     .\r\n                     Canonical JSON is parsable with any full JSON parser, and\r\n                     it allows whitespace for pretty-printed human readable\r\n                     presentation, but it can be put into a canonical form\r\n                     which then has a stable serialised representation and\r\n                     thus a stable hash.\r\nlicense:             BSD3\r\nlicense-file:        LICENSE\r\nauthor:              Duncan Coutts, Edsko de Vries\r\nmaintainer:          duncan@well-typed.com, edsko@well-typed.com\r\ncopyright:           Copyright 2015-2018 Well-Typed LLP\r\nhomepage:            https://github.com/well-typed/canonical-json\r\ncategory:            Text, JSON\r\nbuild-type:          Simple\r\nextra-source-files:  ChangeLog.md\r\ncabal-version:       >=1.10\r\n\r\nextra-source-files:  README.md\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/well-typed/canonical-json.git\r\n\r\nlibrary\r\n  exposed-modules:     Text.JSON.Canonical\r\n                       Text.JSON.Canonical.Class\r\n                       Text.JSON.Canonical.Parse\r\n                       Text.JSON.Canonical.Types\r\n  other-extensions:    CPP, GeneralizedNewtypeDeriving, DeriveDataTypeable,\r\n                       MultiParamTypeClasses, FlexibleInstances,\r\n                       ScopedTypeVariables, OverlappingInstances\r\n  build-depends:       base              >= 4.5     && < 5,\r\n                       bytestring        >= 0.10.4  && < 0.13,\r\n                       containers        >= 0.4     && < 0.8,\r\n                       deepseq           >= 1.2     && < 1.6,\r\n                       parsec            >= 3.1     && < 3.2,\r\n                       pretty            >= 1.0     && < 1.2\r\n  default-language:    Haskell2010\r\n  ghc-options:         -Wall\r\n\r\ntest-suite tests\r\n  type:                exitcode-stdio-1.0\r\n  main-is:             TestSuite.hs\r\n  hs-source-dirs:      tests\r\n  build-depends:       base,\r\n                       bytestring,\r\n                       canonical-json,\r\n                       containers,\r\n                       aeson             >= 1.4     && < 2.3,\r\n                       vector,\r\n                       unordered-containers,\r\n                       QuickCheck        >= 2.11    && < 2.16,\r\n                       tasty,\r\n                       tasty-quickcheck\r\n  default-language:    Haskell2010\r\n  -- -K100k to check for stack overflow:\r\n  ghc-options:         -Wall -with-rtsopts=-K100k\r\n\r\nbenchmark parse-bench\r\n  type:                exitcode-stdio-1.0\r\n  main-is:             Parse.hs\r\n  hs-source-dirs:      benchmark\r\n  build-depends:       base,\r\n                       bytestring,\r\n                       canonical-json,\r\n                       containers,\r\n                       criterion >= 1.1\r\n  default-language:    Haskell2010\r\n  ghc-options:         -Wall -rtsopts\r\n";
  }