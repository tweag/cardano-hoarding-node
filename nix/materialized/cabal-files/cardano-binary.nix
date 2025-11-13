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
      identifier = { name = "cardano-binary"; version = "1.7.2.0"; };
      license = "Apache-2.0";
      copyright = "2019-2021 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Binary serialization for Cardano";
      description = "This package includes the binary serialization format for Cardano";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-fix" or (errorHandler.buildDepError "data-fix"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."recursion-schemes" or (errorHandler.buildDepError "recursion-schemes"))
          (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
        ];
        buildable = true;
      };
      sublibs = {
        "testlib" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-prelude-test" or (errorHandler.buildDepError "cardano-prelude-test"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
          ];
          buildable = true;
        };
      };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-binary:testlib"))
            (hsPkgs."cardano-prelude-test" or (errorHandler.buildDepError "cardano-prelude-test"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/cardano-binary-1.7.2.0.tar.gz";
      sha256 = "891041a868cdcc503499ffaaf05d964f79f93496d352b63da0aafdfda0ed7112";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: cardano-binary\nversion: 1.7.2.0\nsynopsis: Binary serialization for Cardano\ndescription: This package includes the binary serialization format for Cardano\nlicense: Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\nauthor: IOHK\nmaintainer: operations@iohk.io\ncopyright: 2019-2021 IOHK\ncategory: Currency\nbuild-type: Simple\nextra-source-files:\n  CHANGELOG.md\n  README.md\n\ncommon base\n  build-depends: base >=4.18 && <5\n\ncommon project-config\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wpartial-fields\n    -Wredundant-constraints\n    -Wunused-packages\n\nlibrary\n  import: base, project-config\n  hs-source-dirs: src\n  exposed-modules: Cardano.Binary\n  other-modules:\n    Cardano.Binary.Deserialize\n    Cardano.Binary.FromCBOR\n    Cardano.Binary.Serialize\n    Cardano.Binary.ToCBOR\n\n  build-depends:\n    base,\n    bytestring,\n    cborg >=0.2.9 && <0.3,\n    containers,\n    data-fix,\n    formatting,\n    primitive,\n    recursion-schemes >=5.1 && <5.3,\n    safe-exceptions,\n    tagged,\n    text,\n    time,\n    vector,\n\nlibrary testlib\n  import: base, project-config\n  visibility: public\n  hs-source-dirs: testlib\n  exposed-modules:\n    Test.Cardano.Binary.Helpers\n    Test.Cardano.Binary.Helpers.GoldenRoundTrip\n    Test.Cardano.Binary.TreeDiff\n\n  build-depends:\n    QuickCheck,\n    base,\n    base16-bytestring,\n    bytestring,\n    cardano-binary,\n    cardano-prelude-test,\n    cborg,\n    containers,\n    formatting,\n    hedgehog,\n    hspec,\n    pretty-show,\n    quickcheck-instances,\n    text,\n    tree-diff,\n\ntest-suite test\n  import: base, project-config\n  hs-source-dirs: test\n  main-is: test.hs\n  type: exitcode-stdio-1.0\n  other-modules:\n    Test.Cardano.Binary.Failure\n    Test.Cardano.Binary.RoundTrip\n    Test.Cardano.Binary.Serialization\n    Test.Cardano.Binary.SizeBounds\n\n  build-depends:\n    base,\n    bytestring,\n    cardano-binary:{cardano-binary, testlib},\n    cardano-prelude-test,\n    cborg,\n    containers,\n    hedgehog,\n    tagged,\n    text,\n    time,\n    vector,\n\n  ghc-options:\n    -threaded\n    -rtsopts\n";
  }