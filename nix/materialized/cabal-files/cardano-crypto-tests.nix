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
    flags = { secp256k1-support = true; };
    package = {
      specVersion = "2.2";
      identifier = { name = "cardano-crypto-tests"; version = "2.2.2.0"; };
      license = "Apache-2.0";
      copyright = "2020-2021 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Tests for cardano-crypto-class and -praos";
      description = "Tests for cardano-crypto-class and -praos";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-praos" or (errorHandler.buildDepError "cardano-crypto-praos"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
          (hsPkgs."crypton" or (errorHandler.buildDepError "crypton"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."mempack" or (errorHandler.buildDepError "mempack"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
          (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
          (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
        ];
        buildable = true;
      };
      tests = {
        "test-crypto" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-tests" or (errorHandler.buildDepError "cardano-crypto-tests"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          ];
          buildable = true;
        };
      };
      benchmarks = {
        "bench-crypto" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-tests" or (errorHandler.buildDepError "cardano-crypto-tests"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/cardano-crypto-tests-2.2.2.0.tar.gz";
      sha256 = "43525e1b81e4ad2e1563339bf595c3142a764872dba108b169aacf9cfdf1405a";
    });
  }) // {
    package-description-override = "cabal-version: 2.2\nname: cardano-crypto-tests\nversion: 2.2.2.0\nsynopsis: Tests for cardano-crypto-class and -praos\ndescription: Tests for cardano-crypto-class and -praos\nlicense: Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\nauthor: IOHK\nmaintainer: operations@iohk.io\ncopyright: 2020-2021 IOHK\ncategory: Currency\nbuild-type: Simple\nextra-source-files:\n  CHANGELOG.md\n  README.md\n\ndata-files:\n  bls12-381-test-vectors/test_vectors/bls_sig_aug_test_vectors\n  bls12-381-test-vectors/test_vectors/ec_operations_test_vectors\n  bls12-381-test-vectors/test_vectors/h2c_large_dst\n  bls12-381-test-vectors/test_vectors/pairing_test_vectors\n  bls12-381-test-vectors/test_vectors/serde_test_vectors\n  test_vectors/vrf_ver03_generated_1\n  test_vectors/vrf_ver03_generated_2\n  test_vectors/vrf_ver03_generated_3\n  test_vectors/vrf_ver03_generated_4\n  test_vectors/vrf_ver03_standard_10\n  test_vectors/vrf_ver03_standard_11\n  test_vectors/vrf_ver03_standard_12\n  test_vectors/vrf_ver13_generated_1\n  test_vectors/vrf_ver13_generated_2\n  test_vectors/vrf_ver13_generated_3\n  test_vectors/vrf_ver13_generated_4\n  test_vectors/vrf_ver13_standard_10\n  test_vectors/vrf_ver13_standard_11\n  test_vectors/vrf_ver13_standard_12\n\nflag secp256k1-support\n  description:\n    Enable support for functions from libsecp256k1. Requires\n    a recent libsecp256k1 with support for Schnorr signatures.\n\n  default: True\n  manual: True\n\ncommon base\n  build-depends: base >=4.18 && <5\n\ncommon project-config\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wpartial-fields\n    -Wredundant-constraints\n    -Wunused-packages\n\nlibrary\n  import: base, project-config\n  hs-source-dirs: src\n  other-modules: Paths_cardano_crypto_tests\n  exposed-modules:\n    Bench.Crypto.BenchData\n    Bench.Crypto.DSIGN\n    Bench.Crypto.HASH\n    Bench.Crypto.KES\n    Bench.Crypto.VRF\n    Test.Crypto.AllocLog\n    Test.Crypto.DSIGN\n    Test.Crypto.EllipticCurve\n    Test.Crypto.EqST\n    Test.Crypto.Hash\n    Test.Crypto.Instances\n    Test.Crypto.KES\n    Test.Crypto.Regressions\n    Test.Crypto.RunIO\n    Test.Crypto.Util\n    Test.Crypto.VRF\n    Test.Crypto.Vector.SerializationUtils\n\n  build-depends:\n    QuickCheck,\n    base,\n    base16-bytestring,\n    bytestring >=0.10.12.0,\n    cardano-binary,\n    cardano-crypto-class ^>=2.2.2,\n    cardano-crypto-praos >=2.2.1,\n    cborg,\n    containers,\n    contra-tracer ==0.1.0.1 || ==0.1.0.2,\n    criterion,\n    crypton,\n    deepseq,\n    formatting,\n    io-classes >=1.4.0,\n    mempack,\n    mtl,\n    nothunks,\n    pretty-show,\n    quickcheck-instances,\n    tasty,\n    tasty-hunit,\n    tasty-quickcheck,\n    vector,\n\n  if flag(secp256k1-support)\n    cpp-options: -DSECP256K1_ENABLED\n    exposed-modules:\n      Test.Crypto.Vector.Secp256k1DSIGN\n      Test.Crypto.Vector.StringConstants\n      Test.Crypto.Vector.Vectors\n\ntest-suite test-crypto\n  import: base, project-config\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test\n  other-modules: Paths_cardano_crypto_tests\n  main-is: Main.hs\n  build-depends:\n    base,\n    cardano-crypto-class,\n    cardano-crypto-tests,\n    tasty,\n    tasty-quickcheck,\n\n  if flag(secp256k1-support)\n    cpp-options: -DSECP256K1_ENABLED\n  ghc-options:\n    -threaded\n    -rtsopts\n    -with-rtsopts=-N\n\nbenchmark bench-crypto\n  import: base, project-config\n  type: exitcode-stdio-1.0\n  hs-source-dirs: bench\n  main-is: Main.hs\n  build-depends:\n    base,\n    cardano-crypto-class,\n    cardano-crypto-tests,\n    criterion,\n\n  ghc-options: -threaded\n";
  }