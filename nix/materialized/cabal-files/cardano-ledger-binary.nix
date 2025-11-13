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
      identifier = { name = "cardano-ledger-binary"; version = "1.7.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "https://github.com/intersectmbo/cardano-ledger";
      url = "";
      synopsis = "Binary serialization library used throughout ledger";
      description = "";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."FailT" or (errorHandler.buildDepError "FailT"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-praos" or (errorHandler.buildDepError "cardano-crypto-praos"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-fix" or (errorHandler.buildDepError "data-fix"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."mempack" or (errorHandler.buildDepError "mempack"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."recursion-schemes" or (errorHandler.buildDepError "recursion-schemes"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."vector-map" or (errorHandler.buildDepError "vector-map"))
        ];
        buildable = true;
      };
      sublibs = {
        "testlib" = {
          depends = [
            (hsPkgs."ImpSpec" or (errorHandler.buildDepError "ImpSpec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-tests" or (errorHandler.buildDepError "cardano-crypto-tests"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-prelude-test" or (errorHandler.buildDepError "cardano-prelude-test"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cardano-slotting".components.sublibs.testlib or (errorHandler.buildDepError "cardano-slotting:testlib"))
            (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."cuddle" or (errorHandler.buildDepError "cuddle"))
            (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
            (hsPkgs."half" or (errorHandler.buildDepError "half"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-core" or (errorHandler.buildDepError "hspec-core"))
            (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
            (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."prettyprinter-ansi-terminal" or (errorHandler.buildDepError "prettyprinter-ansi-terminal"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
            (hsPkgs."typed-process" or (errorHandler.buildDepError "typed-process"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."vector-map" or (errorHandler.buildDepError "vector-map"))
          ];
          buildable = true;
        };
      };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-praos" or (errorHandler.buildDepError "cardano-crypto-praos"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."cardano-prelude-test" or (errorHandler.buildDepError "cardano-prelude-test"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hedgehog-quickcheck" or (errorHandler.buildDepError "hedgehog-quickcheck"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
            (hsPkgs."cardano-ledger-binary".components.sublibs.testlib or (errorHandler.buildDepError "cardano-ledger-binary:testlib"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."vector-map" or (errorHandler.buildDepError "vector-map"))
          ];
          buildable = true;
        };
      };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-ledger-binary" or (errorHandler.buildDepError "cardano-ledger-binary"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/cardano-ledger-binary-1.7.0.0.tar.gz";
      sha256 = "114ded02215bd1c200928e575af85e5ce278ece1ee5d280bbba5ae9cb97c7d90";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: cardano-ledger-binary\nversion: 1.7.0.0\nlicense: Apache-2.0\nmaintainer: operations@iohk.io\nauthor: IOHK\nhomepage: https://github.com/intersectmbo/cardano-ledger\nsynopsis: Binary serialization library used throughout ledger\ncategory: Network\nbuild-type: Simple\n\nsource-repository head\n  type: git\n  location: https://github.com/intersectmbo/cardano-ledger\n  subdir: libs/cardano-ledger-binary\n\nlibrary\n  exposed-modules:\n    Cardano.Ledger.Binary\n    Cardano.Ledger.Binary.Coders\n    Cardano.Ledger.Binary.Crypto\n    Cardano.Ledger.Binary.Decoding\n    Cardano.Ledger.Binary.Encoding\n    Cardano.Ledger.Binary.FlatTerm\n    Cardano.Ledger.Binary.Group\n    Cardano.Ledger.Binary.Plain\n    Cardano.Ledger.Binary.Version\n\n  hs-source-dirs: src\n  other-modules:\n    Cardano.Ledger.Binary.Decoding.Annotated\n    Cardano.Ledger.Binary.Decoding.Coders\n    Cardano.Ledger.Binary.Decoding.DecCBOR\n    Cardano.Ledger.Binary.Decoding.Decoder\n    Cardano.Ledger.Binary.Decoding.Drop\n    Cardano.Ledger.Binary.Decoding.Sharing\n    Cardano.Ledger.Binary.Decoding.Sized\n    Cardano.Ledger.Binary.Encoding.Coders\n    Cardano.Ledger.Binary.Encoding.EncCBOR\n    Cardano.Ledger.Binary.Encoding.Encoder\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wunused-packages\n\n  build-depends:\n    FailT,\n    aeson,\n    base >=4.18 && <5,\n    base16-bytestring,\n    binary,\n    bytestring,\n    cardano-binary >=1.7,\n    cardano-crypto-class ^>=2.2,\n    cardano-crypto-praos >=2.2,\n    cardano-slotting >=0.2,\n    cardano-strict-containers >=0.1.2,\n    cborg >=0.2.10,\n    containers,\n    data-fix,\n    deepseq,\n    formatting,\n    iproute,\n    mempack,\n    microlens,\n    mtl,\n    network,\n    nothunks,\n    plutus-ledger-api >=1.27.0,\n    primitive,\n    random,\n    recursion-schemes,\n    serialise,\n    tagged,\n    text,\n    time,\n    transformers >=0.5,\n    vector,\n    vector-map ^>=1.1,\n\nlibrary testlib\n  exposed-modules:\n    Test.Cardano.Ledger.Binary\n    Test.Cardano.Ledger.Binary.Arbitrary\n    Test.Cardano.Ledger.Binary.Cddl\n    Test.Cardano.Ledger.Binary.Cuddle\n    Test.Cardano.Ledger.Binary.Plain.Golden\n    Test.Cardano.Ledger.Binary.Plain.RoundTrip\n    Test.Cardano.Ledger.Binary.Random\n    Test.Cardano.Ledger.Binary.RoundTrip\n    Test.Cardano.Ledger.Binary.TreeDiff\n    Test.Cardano.Ledger.Binary.Twiddle\n    Test.Cardano.Ledger.Binary.Vintage.Helpers\n    Test.Cardano.Ledger.Binary.Vintage.Helpers.GoldenRoundTrip\n\n  visibility: public\n  hs-source-dirs: testlib\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wunused-packages\n\n  build-depends:\n    ImpSpec,\n    QuickCheck,\n    base,\n    base16-bytestring,\n    bytestring,\n    cardano-binary,\n    cardano-crypto-class,\n    cardano-crypto-tests,\n    cardano-ledger-binary,\n    cardano-prelude-test,\n    cardano-slotting:{cardano-slotting, testlib} >=0.1.2,\n    cardano-strict-containers,\n    cborg,\n    containers,\n    cuddle >=0.4,\n    formatting,\n    half,\n    hedgehog,\n    hspec,\n    hspec-core,\n    iproute,\n    pretty-show,\n    pretty-simple,\n    prettyprinter,\n    prettyprinter-ansi-terminal,\n    primitive,\n    quickcheck-instances >=0.3.32,\n    random ^>=1.2,\n    tasty-hunit,\n    text,\n    tree-diff,\n    typed-process,\n    unliftio,\n    vector-map,\n\ntest-suite tests\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  hs-source-dirs: test\n  other-modules:\n    Test.Cardano.Ledger.Binary.Failure\n    Test.Cardano.Ledger.Binary.PlainSpec\n    Test.Cardano.Ledger.Binary.RoundTripSpec\n    Test.Cardano.Ledger.Binary.Success\n    Test.Cardano.Ledger.Binary.Vintage.Coders\n    Test.Cardano.Ledger.Binary.Vintage.Drop\n    Test.Cardano.Ledger.Binary.Vintage.Failure\n    Test.Cardano.Ledger.Binary.Vintage.RoundTrip\n    Test.Cardano.Ledger.Binary.Vintage.Serialization\n    Test.Cardano.Ledger.Binary.Vintage.SizeBounds\n\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wunused-packages\n    -threaded\n    -rtsopts\n    -with-rtsopts=-N\n\n  build-depends:\n    QuickCheck,\n    base,\n    bytestring,\n    cardano-crypto-class,\n    cardano-crypto-praos,\n    cardano-ledger-binary,\n    cardano-prelude-test,\n    cardano-slotting,\n    cardano-strict-containers,\n    cborg,\n    containers,\n    hedgehog,\n    hedgehog-quickcheck,\n    hspec,\n    iproute,\n    primitive,\n    tagged,\n    testlib,\n    text,\n    time,\n    vector,\n    vector-map,\n\nbenchmark bench\n  type: exitcode-stdio-1.0\n  main-is: Bench.hs\n  hs-source-dirs: bench\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n    -Wunused-packages\n    -threaded\n    -rtsopts\n    -O2\n\n  build-depends:\n    base,\n    bytestring,\n    cardano-ledger-binary,\n    containers,\n    criterion,\n    deepseq,\n    random,\n";
  }