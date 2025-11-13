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
    flags = { external-libsodium-vrf = true; };
    package = {
      specVersion = "2.2";
      identifier = { name = "cardano-crypto-praos"; version = "2.2.1.1"; };
      license = "Apache-2.0";
      copyright = "2019-2021 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Crypto primitives from libsodium";
      description = "VRF (and KES, tba) primitives from libsodium.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
        ];
        pkgconfig = [
          (pkgconfPkgs."libsodium" or (errorHandler.pkgConfDepError "libsodium"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/cardano-crypto-praos-2.2.1.1.tar.gz";
      sha256 = "85c35179d6e8c20904945bbf06ef08ac5d7313283f9f396cb4e9b85cd15aa640";
    });
  }) // {
    package-description-override = "cabal-version: 2.2\nname: cardano-crypto-praos\nversion: 2.2.1.1\nsynopsis: Crypto primitives from libsodium\ndescription: VRF (and KES, tba) primitives from libsodium.\nlicense: Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\nauthor: IOHK\nmaintainer: operations@iohk.io\ncopyright: 2019-2021 IOHK\ncategory: Currency\nbuild-type: Simple\nextra-source-files:\n  CHANGELOG.md\n  README.md\n\nextra-source-files:\n  cbits/crypto_vrf.h\n  cbits/private/common.h\n  cbits/private/core_h2c.h\n  cbits/private/ed25519_ref10.h\n  cbits/private/ed25519_ref10_fe_25_5.h\n  cbits/private/ed25519_ref10_fe_51.h\n  cbits/private/fe_25_5/base.h\n  cbits/private/fe_25_5/base2.h\n  cbits/private/fe_25_5/constants.h\n  cbits/private/fe_25_5/fe.h\n  cbits/private/fe_51/base.h\n  cbits/private/fe_51/base2.h\n  cbits/private/fe_51/constants.h\n  cbits/private/fe_51/fe.h\n  cbits/vrf03/crypto_vrf_ietfdraft03.h\n  cbits/vrf13_batchcompat/crypto_vrf_ietfdraft13.h\n\nflag external-libsodium-vrf\n  description:\n    Rely on a special libsodium fork containing the VRF code.\n    Otherwise expect a normal unaltered system libsodium, and\n    bundle the VRF code.\n\n  default: True\n  manual: True\n\ncommon base\n  build-depends: base >=4.18 && <5\n\ncommon project-config\n  default-language: Haskell2010\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wpartial-fields\n    -Wredundant-constraints\n    -Wunused-packages\n\nlibrary\n  import: base, project-config\n  hs-source-dirs: src\n  exposed-modules:\n    Cardano.Crypto.RandomBytes\n    Cardano.Crypto.VRF.Praos\n    Cardano.Crypto.VRF.PraosBatchCompat\n\n  build-depends:\n    base,\n    bytestring,\n    cardano-binary,\n    cardano-crypto-class >=2.1.1,\n    deepseq,\n    nothunks,\n\n  pkgconfig-depends: libsodium\n\n  if !flag(external-libsodium-vrf)\n    c-sources:\n      cbits/crypto_vrf.c\n      cbits/private/core_h2c.c\n      cbits/private/ed25519_ref10.c\n      cbits/vrf03/prove.c\n      cbits/vrf03/verify.c\n      cbits/vrf03/vrf.c\n      cbits/vrf13_batchcompat/prove.c\n      cbits/vrf13_batchcompat/verify.c\n      cbits/vrf13_batchcompat/vrf.c\n";
  }