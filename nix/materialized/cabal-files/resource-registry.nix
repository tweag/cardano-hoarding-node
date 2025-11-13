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
      identifier = { name = "resource-registry"; version = "0.1.1.0"; };
      license = "Apache-2.0";
      copyright = "2019-2023 Input Output Global Inc (IOG)\n2023-2024 INTERSECT";
      maintainer = "operations@iohk.io";
      author = "IOG Engineering Team";
      homepage = "";
      url = "";
      synopsis = "Track allocated resources";
      description = "When the scope of a @bracket@ doesn't enclose all uses of the resource, a\n'ResourceRegistry' can be used instead to capture the lifetime of those\nresources.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
        ];
        buildable = true;
      };
      tests = {
        "resource-registry-test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."generics-sop" or (errorHandler.buildDepError "generics-sop"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-classes".components.sublibs.si-timers or (errorHandler.buildDepError "io-classes:si-timers"))
            (hsPkgs."io-classes".components.sublibs.strict-mvar or (errorHandler.buildDepError "io-classes:strict-mvar"))
            (hsPkgs."io-classes".components.sublibs.strict-stm or (errorHandler.buildDepError "io-classes:strict-stm"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."quickcheck-state-machine".components.sublibs.no-vendored-treediff or (errorHandler.buildDepError "quickcheck-state-machine:no-vendored-treediff"))
            (hsPkgs."resource-registry" or (errorHandler.buildDepError "resource-registry"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/resource-registry-0.1.1.0.tar.gz";
      sha256 = "37edfa14c7d390fd29810061965e5053e243b5dfb46fad742025b1c95ab4907f";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: resource-registry\nversion: 0.1.1.0\nsynopsis: Track allocated resources\ndescription:\n  When the scope of a @bracket@ doesn't enclose all uses of the resource, a\n  'ResourceRegistry' can be used instead to capture the lifetime of those\n  resources.\n\nlicense: Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\nauthor: IOG Engineering Team\nmaintainer: operations@iohk.io\ncopyright:\n  2019-2023 Input Output Global Inc (IOG)\n  2023-2024 INTERSECT\n\ncategory: Control\nbuild-type: Simple\nbug-reports: https://github.com/IntersectMBO/io-classes-extra/issues\ntested-with: ghc ==9.6 || ==9.8 || ==9.10 || ==9.12\nextra-doc-files:\n  CHANGELOG.md\n  README.md\n\nsource-repository head\n  type: git\n  location: https://github.com/IntersectMBO/io-classes-extra\n  subdir: resource-registry\n\nsource-repository this\n  type: git\n  location: https://github.com/IntersectMBO/io-classes-extra\n  subdir: resource-registry\n  tag: resource-registry-0.1.1.0\n\ncommon warnings\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wmissing-export-lists\n    -Wunused-packages\n    -Wno-unticked-promoted-constructors\n\nlibrary\n  import: warnings\n  exposed-modules: Control.ResourceRegistry\n  build-depends:\n    base >=4.14 && <4.22,\n    bimap ^>=0.5,\n    containers >=0.6.7 && <0.8,\n    io-classes:{io-classes, strict-stm} ^>=1.8,\n    mtl ^>=2.3,\n    nothunks ^>=0.2,\n\n  hs-source-dirs: src\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n\ntest-suite resource-registry-test\n  import: warnings\n  default-language: Haskell2010\n  default-extensions: ImportQualifiedPost\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test\n  main-is: Main.hs\n  other-modules:\n    Test.Util.QSM\n    Test.Util.SOP\n    Test.Util.ToExpr\n\n  build-depends:\n    QuickCheck,\n    base,\n    containers,\n    generics-sop,\n    io-classes:{io-classes, si-timers, strict-mvar, strict-stm},\n    mtl,\n    quickcheck-state-machine:no-vendored-treediff,\n    resource-registry,\n    tasty,\n    tasty-quickcheck,\n    tree-diff,\n";
  }