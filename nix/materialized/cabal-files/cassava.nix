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
      specVersion = "1.18";
      identifier = { name = "cassava"; version = "0.5.4.1"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2012 Johan Tibell\n(c) 2012 Bryan O'Sullivan\n(c) 2011 MailRank, Inc.";
      maintainer = "https://github.com/haskell-hvr/cassava";
      author = "Johan Tibell";
      homepage = "https://github.com/haskell-hvr/cassava";
      url = "";
      synopsis = "A CSV parsing and encoding library";
      description = "@cassava@ is a library for parsing and encoding [RFC 4180](https://tools.ietf.org/html/rfc4180)\ncompliant [comma-separated values (CSV)](https://en.wikipedia.org/wiki/Comma-separated_values) data,\nwhich is a textual line-oriented format commonly used for exchanging tabular data.\n\n@cassava@'s API includes support for\n\n- Index-based record-conversion\n- Name-based record-conversion\n- Typeclass directed conversion of fields and records\n- Built-in field-conversion instances for standard types\n- Customizable record-conversion instance derivation via GHC generics\n- Low-level [bytestring](https://hackage.haskell.org/package/bytestring) builders (see \"Data.Csv.Builder\")\n- Incremental decoding and encoding API (see \"Data.Csv.Incremental\")\n- Streaming API for constant-space decoding (see \"Data.Csv.Streaming\")\n\nMoreover, this library is designed to be easy to use; for instance, here's a\nvery simple example of encoding CSV data:\n\n>>> Data.Csv.encode [(\"John\",27),(\"Jane\",28)]\n\"John,27\\r\\nJane,28\\r\\n\"\n\nPlease refer to the documentation in \"Data.Csv\" and the included [README](#readme) for more usage examples.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."text-short" or (errorHandler.buildDepError "text-short"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."Only" or (errorHandler.buildDepError "Only"))
        ];
        buildable = true;
      };
      tests = {
        "unit-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/cassava-0.5.4.1.tar.gz";
      sha256 = "d40636f530737a99c0282084322230d04d6e1b445e779a0d0a5005f6285a495f";
    });
  }) // {
    package-description-override = "cabal-version:       1.18\nName:                cassava\nVersion:             0.5.4.1\n\nSynopsis:            A CSV parsing and encoding library\nDescription: {\n\n@cassava@ is a library for parsing and encoding [RFC 4180](https://tools.ietf.org/html/rfc4180)\ncompliant [comma-separated values (CSV)](https://en.wikipedia.org/wiki/Comma-separated_values) data,\nwhich is a textual line-oriented format commonly used for exchanging tabular data.\n.\n@cassava@'s API includes support for\n.\n- Index-based record-conversion\n- Name-based record-conversion\n- Typeclass directed conversion of fields and records\n- Built-in field-conversion instances for standard types\n- Customizable record-conversion instance derivation via GHC generics\n- Low-level [bytestring](https://hackage.haskell.org/package/bytestring) builders (see \"Data.Csv.Builder\")\n- Incremental decoding and encoding API (see \"Data.Csv.Incremental\")\n- Streaming API for constant-space decoding (see \"Data.Csv.Streaming\")\n.\nMoreover, this library is designed to be easy to use; for instance, here's a\nvery simple example of encoding CSV data:\n.\n>>> Data.Csv.encode [(\"John\",27),(\"Jane\",28)]\n\"John,27\\r\\nJane,28\\r\\n\"\n.\nPlease refer to the documentation in \"Data.Csv\" and the included [README](#readme) for more usage examples.\n\n}\nHomepage:            https://github.com/haskell-hvr/cassava\nLicense:             BSD3\nLicense-file:        LICENSE\nBug-reports:         https://github.com/haskell-hvr/cassava/issues\nCopyright:           (c) 2012 Johan Tibell\n                     (c) 2012 Bryan O'Sullivan\n                     (c) 2011 MailRank, Inc.\nAuthor:              Johan Tibell\nMaintainer:          https://github.com/haskell-hvr/cassava\nCategory:            Text, Web, CSV\nBuild-type:          Simple\nExtra-source-files:  examples/*.hs\nExtra-doc-files:     CHANGES.md\n                     README.md\nTested-with:\n  GHC == 9.14.1\n  GHC == 9.12.2\n  GHC == 9.10.2\n  GHC == 9.8.4\n  GHC == 9.6.7\n  GHC == 9.4.8\n  GHC == 9.2.8\n  GHC == 9.0.2\n  GHC == 8.10.7\n  GHC == 8.8.4\n  GHC == 8.6.5\n  GHC == 8.4.4\n  GHC == 8.2.2\n  GHC == 8.0.2\n\n----------------------------------------------------------------------------\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskell-hvr/cassava.git\n\nLibrary\n  default-language: Haskell2010\n  other-extensions:\n    BangPatterns\n    CPP\n    DataKinds\n    DefaultSignatures\n    DeriveFunctor\n    FlexibleContexts\n    FlexibleInstances\n    KindSignatures\n    MultiParamTypeClasses\n    OverloadedStrings\n    PolyKinds\n    Rank2Types\n    ScopedTypeVariables\n    TypeOperators\n    UndecidableInstances\n\n  hs-source-dirs: src\n\n  Exposed-modules:\n    Data.Csv\n    Data.Csv.Builder\n    Data.Csv.Incremental\n    Data.Csv.Parser\n    Data.Csv.Streaming\n\n  Other-modules:\n    Data.Csv.Conversion\n    Data.Csv.Conversion.Internal\n    Data.Csv.Encoding\n    Data.Csv.Types\n    Data.Csv.Util\n\n  -- Lower bounds from GHC 8.0 and Stackage LTS 7.0\n  Build-depends:\n      base                  >= 4.9       && < 5\n    , array                 >= 0.5.1.1   && < 0.6\n    , attoparsec            >= 0.11.3.0  && < 0.15\n    , bytestring            >= 0.10.8.0  && < 0.13\n    , containers            >= 0.5.7.1   && < 1\n    , deepseq               >= 1.4.2.0   && < 1.6\n    , hashable              >= 1.2.4.0   && < 2\n    , scientific            >= 0.3.4.9   && < 0.4\n    , text                  >= 1.2.2.1   && < 2.2\n    , text-short            == 0.1.*\n    , unordered-containers  >= 0.2.7.1   && < 0.3\n    , vector                >= 0.11.0.0  && < 0.14\n    , Only                  >= 0.1       && < 0.1.1\n\n  ghc-options:\n    -Wall\n    -- https://ghc.haskell.org/trac/ghc/wiki/Migration/8.0#Recommendationsforforward-compatibility\n    -Wcompat\n    -Wnoncanonical-monad-instances\n\n  if impl(ghc >= 8.8)\n    ghc-options: -Wno-star-is-type\n  else\n    ghc-options: -Wnoncanonical-monadfail-instances\n\n  if impl(ghc >= 8.2)\n    ghc-options: -Wcpp-undef\n\n----------------------------------------------------------------------------\n\nTest-suite unit-tests\n  default-language: Haskell2010\n\n  Type: exitcode-stdio-1.0\n  Main-is: UnitTests.hs\n  -- dependencies with version constraints inherited via lib:cassava\n  Build-depends: base                        >= 4.11     && < 5\n               , bytestring\n               , cassava\n               , scientific\n               , text\n               , unordered-containers\n               , vector\n  -- extra dependencies not already used by lib:cassava\n  build-depends: HUnit                       >= 1.3.1.2  && < 1.7\n               , QuickCheck                  >= 2.14     && < 3\n               , quickcheck-instances        >= 0.3.12   && < 0.4\n               , test-framework              == 0.8.*\n               , test-framework-hunit        == 0.3.*\n               , test-framework-quickcheck2  == 0.3.*\n\n  hs-source-dirs: tests\n\n  ghc-options:\n    -Wall\n    -- https://ghc.haskell.org/trac/ghc/wiki/Migration/8.0#Recommendationsforforward-compatibility\n    -Wcompat\n    -Wcpp-undef\n    -Wnoncanonical-monad-instances\n\n  if impl(ghc >= 8.8)\n    ghc-options: -Wno-star-is-type\n  else\n    ghc-options: -Wnoncanonical-monadfail-instances\n";
  }