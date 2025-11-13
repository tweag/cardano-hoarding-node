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
    flags = { checkmvarinvariants = false; checktvarinvariants = false; };
    package = {
      specVersion = "3.0";
      identifier = { name = "strict-checked-vars"; version = "0.2.1.0"; };
      license = "Apache-2.0";
      copyright = "2019-2023 Input Output Global Inc (IOG).";
      maintainer = "operations@iohk.io, Joris Dral";
      author = "IOG Engineering Team";
      homepage = "";
      url = "";
      synopsis = "Strict MVars and TVars with invariant checking for IO and IOSim";
      description = "Strict @MVar@ and @TVar@ interfaces with invariant checking compatible with\n[IO](https://hackage.haskell.org/package/base-4.18.0.0/docs/Prelude.html#t:IO)\n& [io-sim](https://hackage.haskell.org/package/io-sim).";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."io-classes".components.sublibs.strict-mvar or (errorHandler.buildDepError "io-classes:strict-mvar"))
          (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
        ];
        buildable = true;
      };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
            (hsPkgs."strict-checked-vars" or (errorHandler.buildDepError "strict-checked-vars"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/strict-checked-vars-0.2.1.0.tar.gz";
      sha256 = "9532b13ddf376a37ebabe45ee43adc55ae6ded4063e5a745cc8df54a33918489";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: strict-checked-vars\nversion: 0.2.1.0\nsynopsis:\n  Strict MVars and TVars with invariant checking for IO and IOSim\n\ndescription:\n  Strict @MVar@ and @TVar@ interfaces with invariant checking compatible with\n  [IO](https://hackage.haskell.org/package/base-4.18.0.0/docs/Prelude.html#t:IO)\n  & [io-sim](https://hackage.haskell.org/package/io-sim).\n\nlicense: Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\ncopyright: 2019-2023 Input Output Global Inc (IOG).\nauthor: IOG Engineering Team\nmaintainer: operations@iohk.io, Joris Dral\ncategory: Concurrency\nbuild-type: Simple\nextra-doc-files:\n  CHANGELOG.md\n  README.md\n\nbug-reports: https://github.com/IntersectMBO/io-classes-extra/issues\ntested-with: ghc ==9.6 || ==9.8 || ==9.10 || ==9.12\n\nsource-repository head\n  type: git\n  location: https://github.com/IntersectMBO/io-classes-extra\n  subdir: strict-checked-vars\n\nsource-repository this\n  type: git\n  location: https://github.com/IntersectMBO/io-classes-extra\n  subdir: strict-checked-vars\n  tag: strict-checked-vars-0.2.1.0\n\nflag checkmvarinvariants\n  description: Enable runtime invariant checks on StrictMVars\n  manual: True\n  default: False\n\nflag checktvarinvariants\n  description: Enable runtime invariant checks on StrictTVars\n  manual: True\n  default: False\n\nlibrary\n  hs-source-dirs: src\n  exposed-modules:\n    Control.Concurrent.Class.MonadMVar.Strict.Checked\n    Control.Concurrent.Class.MonadSTM.Strict.TVar.Checked\n\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n  build-depends:\n    base >=4.9 && <5,\n    io-classes:{io-classes, strict-mvar, strict-stm} ^>=1.8,\n\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wunused-packages\n\n  if flag(checkmvarinvariants)\n    cpp-options: -DCHECK_MVAR_INVARIANTS\n\n  if flag(checktvarinvariants)\n    cpp-options: -DCHECK_TVAR_INVARIANTS\n\ntest-suite test\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test\n  main-is: Main.hs\n  other-modules:\n    Test.Control.Concurrent.Class.MonadMVar.Strict.Checked\n    Test.Control.Concurrent.Class.MonadMVar.Strict.Checked.WHNF\n    Test.Control.Concurrent.Class.MonadSTM.Strict.TVar.Checked\n    Test.Control.Concurrent.Class.MonadSTM.Strict.TVar.Checked.WHNF\n    Test.Utils\n\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n  build-depends:\n    QuickCheck,\n    base,\n    io-classes,\n    io-sim,\n    nothunks,\n    strict-checked-vars,\n    tasty,\n    tasty-quickcheck,\n\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wunused-packages\n    -fno-ignore-asserts\n\n  if flag(checkmvarinvariants)\n    cpp-options: -DCHECK_MVAR_INVARIANTS\n\n  if flag(checktvarinvariants)\n    cpp-options: -DCHECK_TVAR_INVARIANTS\n";
  }