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
      identifier = { name = "cardano-lmdb"; version = "0.4.0.3"; };
      license = "BSD-2-Clause";
      copyright = "(c) 2014 by David Barbour";
      maintainer = "operations@iohk.io";
      author = "David Barbour";
      homepage = "http://github.com/input-output-hk/haskell-lmdb";
      url = "";
      synopsis = "Lightning MDB bindings";
      description = "LMDB is a read-optimized Berkeley DB replacement developed by Symas\nfor the OpenLDAP project. LMDB has impressive performance characteristics\nand a friendly BSD-style OpenLDAP license. See <http://symas.com/mdb/>.\n.\nThis library has Haskell bindings to the LMDB library. You must install\nthe lmdb development files before installing this library,\ne.g. `sudo apt-get install liblmdb-dev` works for Ubuntu.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."cardano-lmdb".components.sublibs.ffi or (errorHandler.buildDepError "cardano-lmdb:ffi"))
        ];
        buildable = true;
      };
      sublibs = {
        "ffi" = {
          depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
          pkgconfig = [
            (pkgconfPkgs."lmdb" or (errorHandler.pkgConfDepError "lmdb"))
          ];
          build-tools = [
            (hsPkgs.pkgsBuildBuild.hsc2hs.components.exes.hsc2hs or (pkgs.pkgsBuildBuild.hsc2hs or (errorHandler.buildToolDepError "hsc2hs:hsc2hs")))
          ];
          buildable = true;
        };
      };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-lmdb" or (errorHandler.buildDepError "cardano-lmdb"))
            (hsPkgs."cardano-lmdb".components.sublibs.ffi or (errorHandler.buildDepError "cardano-lmdb:ffi"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/cardano-lmdb-0.4.0.3.tar.gz";
      sha256 = "411cb219d06fd1443fbfe0926e6cd6614c9ca0100b4e7fdfff439b984c56f253";
    });
  }) // {
    package-description-override = "cabal-version:   3.0\nname:            cardano-lmdb\nversion:         0.4.0.3\nsynopsis:        Lightning MDB bindings\ncategory:        Database\ndescription:\n  LMDB is a read-optimized Berkeley DB replacement developed by Symas\n  for the OpenLDAP project. LMDB has impressive performance characteristics\n  and a friendly BSD-style OpenLDAP license. See <http://symas.com/mdb/>.\n  .\n  This library has Haskell bindings to the LMDB library. You must install\n  the lmdb development files before installing this library,\n  e.g. `sudo apt-get install liblmdb-dev` works for Ubuntu.\n\nauthor:          David Barbour\nmaintainer:      operations@iohk.io\nhomepage:        http://github.com/input-output-hk/haskell-lmdb\ncopyright:       (c) 2014 by David Barbour\nlicense:         BSD-2-Clause\nlicense-file:    LICENSE\nstability:       experimental\nbuild-type:      Simple\ntested-with:     GHC ==8.10 || ==9.2 || ==9.4 || ==9.6 || ==9.8 || ==9.10 || ==9.12\nextra-doc-files:\n  CHANGELOG.md\n  INPUT-OUTPUT-FORK.md\n  README.md\n\nsource-repository head\n  type:     git\n  location: http://github.com/input-output-hk/haskell-lmdb.git\n\nsource-repository head\n  type:     git\n  location: http://github.com/input-output-hk/haskell-lmdb.git\n  tag:      cardano-lmdb-0.4.0.3\n\ncommon warnings\n  ghc-options:\n    -Wall -Wcompat -Wincomplete-uni-patterns\n    -Wincomplete-record-updates -Wpartial-fields -Widentities\n    -Wredundant-constraints -Wmissing-export-lists\n    -Wno-unticked-promoted-constructors -Wunused-packages\n\nlibrary\n  import:           warnings\n  hs-source-dirs:   src\n  default-language: Haskell2010\n  build-depends:\n    , base              >=4.14 && <4.22\n    , cardano-lmdb:ffi\n\n  exposed-modules:  Database.LMDB.Raw\n\nlibrary ffi\n  import:             warnings\n  visibility:         private\n  hs-source-dirs:     src\n  default-language:   Haskell2010\n  build-depends:      base >=4.14 && <4.22\n  build-tool-depends: hsc2hs:hsc2hs\n  exposed-modules:    Database.LMDB.FFI\n  pkgconfig-depends:  lmdb >=0.9 && <0.10\n\ntest-suite test\n  import:           warnings\n  default-language: Haskell2010\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   test\n  main-is:          Main.hs\n  build-depends:\n    , async\n    , base              >=4.14 && <4.22\n    , cardano-lmdb\n    , cardano-lmdb:ffi\n    , tasty\n    , tasty-hunit\n    , temporary\n\n  ghc-options:      -fno-ignore-asserts -threaded\n";
  }