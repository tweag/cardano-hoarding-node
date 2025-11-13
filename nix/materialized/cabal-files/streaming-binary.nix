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
      identifier = { name = "streaming-binary"; version = "0.3.0.1"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2017 Mathieu Boespflug";
      maintainer = "m@tweag.io";
      author = "Mathieu Boespflug";
      homepage = "https://github.com/mboes/streaming-binary#readme";
      url = "";
      synopsis = "Streaming interface to binary.";
      description = "";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
          (hsPkgs."streaming-bytestring" or (errorHandler.buildDepError "streaming-bytestring"))
        ];
        buildable = true;
      };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
            (hsPkgs."streaming-binary" or (errorHandler.buildDepError "streaming-binary"))
            (hsPkgs."streaming-bytestring" or (errorHandler.buildDepError "streaming-bytestring"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/streaming-binary-0.3.0.1.tar.gz";
      sha256 = "cd2bfd355bae40ccf834e40baa2cbd40ff014d30b5674c64d8e6725987566958";
    });
  }) // {
    package-description-override = "name: streaming-binary\nversion: 0.3.0.1\nsynopsis: Streaming interface to binary.\nhomepage: https://github.com/mboes/streaming-binary#readme\nlicense: BSD3\nlicense-file: LICENSE\nauthor: Mathieu Boespflug\nmaintainer: m@tweag.io\ncopyright: (c) 2017 Mathieu Boespflug\ncategory: Streaming, Parsing\nbuild-type: Simple\nextra-source-files: README.md\ncabal-version: >=1.10\n\nsource-repository head\n  type: git\n  location: https://github.com/mboes/streaming-binary\n\nlibrary\n  hs-source-dirs: src\n  exposed-modules: Streaming.Binary\n  build-depends:\n    base >= 4.7 && < 5,\n    binary >= 0.8,\n    bytestring >= 0.10,\n    streaming >= 0.1.4,\n    streaming-bytestring >= 0.1.4\n  default-language: Haskell2010\n\ntest-suite tests\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test\n  main-is: Main.hs\n  other-modules:\n    Spec\n    Streaming.BinarySpec\n  build-depends:\n    base,\n    binary,\n    bytestring >= 0.10,\n    hspec >= 2.4,\n    streaming,\n    streaming-binary,\n    streaming-bytestring\n  ghc-options: -threaded -rtsopts -with-rtsopts=-N\n  default-language: Haskell2010\n";
  }