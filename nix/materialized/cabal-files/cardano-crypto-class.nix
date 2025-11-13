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
      identifier = { name = "cardano-crypto-class"; version = "2.2.3.1"; };
      license = "Apache-2.0";
      copyright = "2019-2021 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Type classes abstracting over cryptography primitives for Cardano";
      description = "Type classes abstracting over cryptography primitives for Cardano";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-strict-containers" or (errorHandler.buildDepError "cardano-strict-containers"))
          (hsPkgs."crypton" or (errorHandler.buildDepError "crypton"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."heapwords" or (errorHandler.buildDepError "heapwords"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."memory-pool" or (errorHandler.buildDepError "memory-pool"))
          (hsPkgs."mempack" or (errorHandler.buildDepError "mempack"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."th-compat" or (errorHandler.buildDepError "th-compat"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
        ] ++ pkgs.lib.optional (compiler.isGhc && compiler.version.lt "9.0.0") (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"));
        pkgconfig = [
          (pkgconfPkgs."libblst" or (errorHandler.pkgConfDepError "libblst"))
          (pkgconfPkgs."libsodium" or (errorHandler.pkgConfDepError "libsodium"))
        ] ++ pkgs.lib.optional (flags.secp256k1-support) (pkgconfPkgs."libsecp256k1" or (errorHandler.pkgConfDepError "libsecp256k1"));
        buildable = true;
      };
      tests = {
        "test-memory-example" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          ] ++ pkgs.lib.optional (system.isLinux || system.isOsx) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
          buildable = false;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/cardano-crypto-class-2.2.3.1.tar.gz";
      sha256 = "94d62d9d2b6c0993f7fd37d8d5cbb341190ae875a978bedbce634282084d489f";
    });
  }) // {
    package-description-override = "cabal-version: 2.2\nname: cardano-crypto-class\nversion: 2.2.3.1\nsynopsis:\n  Type classes abstracting over cryptography primitives for Cardano\n\ndescription:\n  Type classes abstracting over cryptography primitives for Cardano\n\nlicense: Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\nauthor: IOHK\nmaintainer: operations@iohk.io\ncopyright: 2019-2021 IOHK\ncategory: Currency\nbuild-type: Simple\nextra-source-files:\n  CHANGELOG.md\n  README.md\n\nflag secp256k1-support\n  description:\n    Enable support for functions from libsecp256k1. Requires\n    a recent libsecp256k1 with support for Schnorr signatures.\n\n  default: True\n  manual: True\n\ncommon base\n  build-depends: base >=4.18 && <5\n\ncommon project-config\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wpartial-fields\n    -Wredundant-constraints\n    -Wunused-packages\n\nlibrary\n  import: base, project-config\n  hs-source-dirs: src\n  exposed-modules:\n    Cardano.Crypto.DSIGN\n    Cardano.Crypto.DSIGN.Class\n    Cardano.Crypto.DSIGN.Ed25519\n    Cardano.Crypto.DSIGN.Ed448\n    Cardano.Crypto.DSIGN.Mock\n    Cardano.Crypto.DSIGN.NeverUsed\n    Cardano.Crypto.DirectSerialise\n    Cardano.Crypto.EllipticCurve.BLS12_381\n    Cardano.Crypto.EllipticCurve.BLS12_381.Internal\n    Cardano.Crypto.Hash\n    Cardano.Crypto.Hash.Blake2b\n    Cardano.Crypto.Hash.Class\n    Cardano.Crypto.Hash.Keccak256\n    Cardano.Crypto.Hash.NeverUsed\n    Cardano.Crypto.Hash.RIPEMD160\n    Cardano.Crypto.Hash.SHA256\n    Cardano.Crypto.Hash.SHA3_256\n    Cardano.Crypto.Hash.SHA3_512\n    Cardano.Crypto.Hash.SHA512\n    Cardano.Crypto.Hash.Short\n    Cardano.Crypto.Init\n    Cardano.Crypto.KES\n    Cardano.Crypto.KES.Class\n    Cardano.Crypto.KES.CompactSingle\n    Cardano.Crypto.KES.CompactSum\n    Cardano.Crypto.KES.Mock\n    Cardano.Crypto.KES.NeverUsed\n    Cardano.Crypto.KES.Simple\n    Cardano.Crypto.KES.Single\n    Cardano.Crypto.KES.Sum\n    Cardano.Crypto.Libsodium\n    Cardano.Crypto.Libsodium.C\n    Cardano.Crypto.Libsodium.Constants\n    Cardano.Crypto.Libsodium.Hash\n    Cardano.Crypto.Libsodium.Hash.Class\n    Cardano.Crypto.Libsodium.Init\n    Cardano.Crypto.Libsodium.MLockedBytes\n    Cardano.Crypto.Libsodium.MLockedBytes.Internal\n    Cardano.Crypto.Libsodium.MLockedSeed\n    Cardano.Crypto.Libsodium.Memory\n    Cardano.Crypto.Libsodium.Memory.Internal\n    Cardano.Crypto.Libsodium.UnsafeC\n    Cardano.Crypto.PinnedSizedBytes\n    Cardano.Crypto.Seed\n    Cardano.Crypto.Util\n    Cardano.Crypto.VRF\n    Cardano.Crypto.VRF.Class\n    Cardano.Crypto.VRF.Mock\n    Cardano.Crypto.VRF.NeverUsed\n    Cardano.Crypto.VRF.Simple\n    Cardano.Foreign\n\n  other-modules:\n    Cardano.Crypto.PackedBytes\n\n  build-depends:\n    aeson,\n    base,\n    base16-bytestring >=1,\n    bytestring,\n    cardano-binary >=1.6,\n    cardano-strict-containers,\n    crypton,\n    deepseq,\n    heapwords,\n    io-classes >=1.4.1,\n    memory,\n    memory-pool,\n    mempack,\n    mtl,\n    nothunks,\n    primitive >=0.8,\n    serialise,\n    template-haskell,\n    text,\n    th-compat,\n    transformers,\n    vector,\n\n  if impl(ghc <9.0.0)\n    build-depends:\n      integer-gmp\n  pkgconfig-depends:\n    libblst >=0.3.14,\n    libsodium,\n\n  c-sources: cbits/blst_util.c\n\n  if flag(secp256k1-support)\n    exposed-modules:\n      Cardano.Crypto.DSIGN.EcdsaSecp256k1\n      Cardano.Crypto.DSIGN.SchnorrSecp256k1\n      Cardano.Crypto.SECP256K1.C\n      Cardano.Crypto.SECP256K1.Constants\n\n    pkgconfig-depends: libsecp256k1\n    cpp-options: -DSECP256K1_ENABLED\n\ntest-suite test-memory-example\n  import: base, project-config\n  -- Temporarily removing this as it is breaking the CI, and\n  -- we don't see the benefit. Will circle back to this to decide\n  -- whether to modify or completely remove.\n  buildable: False\n  type: exitcode-stdio-1.0\n  hs-source-dirs: memory-example\n  main-is: Main.hs\n  build-depends:\n    base,\n    bytestring,\n    cardano-crypto-class,\n\n  if (os(linux) || os(osx))\n    build-depends: unix\n";
  }