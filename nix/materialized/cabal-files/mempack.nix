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
      identifier = { name = "mempack"; version = "0.1.2.0"; };
      license = "BSD-3-Clause";
      copyright = "2024 Alexey Kuleshevich";
      maintainer = "alexey@kuleshevi.ch";
      author = "Alexey Kuleshevich";
      homepage = "https://github.com/lehins/mempack";
      url = "";
      synopsis = "Short description";
      description = "Please see the README on GitHub at <https://github.com/lehins/mempack#readme>";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."FailT" or (errorHandler.buildDepError "FailT"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
        ] ++ pkgs.lib.optional (!(compiler.isGhc && compiler.version.ge "9.4")) (hsPkgs."data-array-byte" or (errorHandler.buildDepError "data-array-byte"))) ++ pkgs.lib.optional (!(compiler.isGhc && compiler.version.ge "9.0")) (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"));
        buildable = true;
      };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."FailT" or (errorHandler.buildDepError "FailT"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."mempack" or (errorHandler.buildDepError "mempack"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ] ++ pkgs.lib.optional (!(compiler.isGhc && compiler.version.ge "9.4")) (hsPkgs."data-array-byte" or (errorHandler.buildDepError "data-array-byte"));
          buildable = true;
        };
      };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."avro" or (errorHandler.buildDepError "avro"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cereal" or (errorHandler.buildDepError "cereal"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."mempack" or (errorHandler.buildDepError "mempack"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."store" or (errorHandler.buildDepError "store"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/mempack-0.1.2.0.tar.gz";
      sha256 = "8776e207b9880e095dce828c7bc129f2e5b01c55a0c294857f7d99ae57633b52";
    });
  }) // {
    package-description-override = "name:                mempack\nversion:             0.1.2.0\nsynopsis:            Short description\ndescription:         Please see the README on GitHub at <https://github.com/lehins/mempack#readme>\nhomepage:            https://github.com/lehins/mempack\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Alexey Kuleshevich\nmaintainer:          alexey@kuleshevi.ch\ncopyright:           2024 Alexey Kuleshevich\ncategory:            Algorithms\nbuild-type:          Simple\nextra-source-files:  README.md\nextra-doc-files:     CHANGELOG.md\ncabal-version:       1.18\ntested-with:         GHC == 8.6.5\n                   , GHC == 8.8.4\n                   , GHC == 8.10.7\n                   , GHC == 9.0.2\n                   , GHC == 9.2.8\n                   , GHC == 9.4.8\n                   , GHC == 9.6.7\n                   , GHC == 9.8.4\n                   , GHC == 9.10.1\n                   , GHC == 9.12.1\n\nlibrary\n  hs-source-dirs:      src\n  exposed-modules:     Data.MemPack\n                     , Data.MemPack.Buffer\n                     , Data.MemPack.Error\n\n  other-modules:\n  build-depends:       base >= 4.12 && < 5\n                     , bytestring\n                     , FailT\n                     , mtl\n                     , text\n  if !impl(ghc >= 9.4)\n    build-depends:     data-array-byte\n  if !impl(ghc >= 9.0)\n    build-depends:     integer-gmp\n\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n                       -Wincomplete-record-updates\n                       -Wincomplete-uni-patterns\n                       -Wredundant-constraints\n\ntest-suite tests\n  type:               exitcode-stdio-1.0\n  hs-source-dirs:     tests\n  main-is:            Main.hs\n  other-modules:      Test.Common\n                    , Test.MemPackSpec\n  build-depends:      base\n                    , bytestring\n                    , FailT\n                    , hspec\n                    , mempack\n                    , mtl\n                    , QuickCheck\n                    , random >=1.2.1\n                    , text\n  if !impl(ghc >= 9.4)\n    build-depends:     data-array-byte\n\n  default-language:   Haskell2010\n  ghc-options:        -Wall\n                      -Wincomplete-record-updates\n                      -Wincomplete-uni-patterns\n                      -Wredundant-constraints\n                      -fno-warn-orphans\n                      -threaded\n                      -with-rtsopts=-N2\n\nbenchmark bench\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      bench\n  main-is:             Bench.hs\n  ghc-options:         -Wall\n                       -threaded\n                       -O2\n                       -with-rtsopts=-N\n  build-depends:       base\n                     , avro >= 0.5.1\n                     , binary\n                     , bytestring\n                     , cereal\n                     , criterion\n                     , flat\n                     , mempack\n                     , serialise\n                     , store\n                     , vector\n  default-language:    Haskell2010\n\nsource-repository head\n  type:     git\n  location: https://github.com/lehins/mempack\n";
  }