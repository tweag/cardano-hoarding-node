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
      identifier = { name = "safe-wild-cards"; version = "1.0.0.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "amesgen <amesgen@amesgen.de>";
      author = "Emily April Kazak";
      homepage = "https://github.com/amesgen/safe-wild-cards";
      url = "";
      synopsis = "Use RecordWildCards safely";
      description = "@-XRecordWildCards@ is convenient, but sometimes you want to assert that\nyou have handled all fields of a record, and there is no easy way to do that.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
        ];
        buildable = true;
      };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."safe-wild-cards" or (errorHandler.buildDepError "safe-wild-cards"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/safe-wild-cards-1.0.0.2.tar.gz";
      sha256 = "576d1c4f7b27d0e30cfb45bf5513fabdb287e19916d8f9bbdfd4579f464844ac";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\r\n\r\nname: safe-wild-cards\r\nversion: 1.0.0.2\r\nx-revision: 2\r\nsynopsis: Use RecordWildCards safely\r\ndescription:\r\n  @-XRecordWildCards@ is convenient, but sometimes you want to assert that\r\n  you have handled all fields of a record, and there is no easy way to do that.\r\n\r\nlicense: BSD-3-Clause\r\nlicense-file: LICENSE\r\nauthor: Emily April Kazak\r\nmaintainer: amesgen <amesgen@amesgen.de>\r\nhomepage: https://github.com/amesgen/safe-wild-cards\r\ncategory: Control\r\nbuild-type: Simple\r\nextra-source-files:\r\n  README.md\r\nextra-doc-files:\r\n  CHANGELOG.md\r\ntested-with:\r\n  GHC ==8.0.2\r\n  GHC ==8.2.2\r\n  GHC ==8.4.4\r\n  GHC ==8.6.5\r\n  GHC ==8.8.4\r\n  GHC ==8.10.7\r\n  GHC ==9.0.2\r\n  GHC ==9.2.8\r\n  GHC ==9.4.8\r\n  GHC ==9.6.6\r\n  GHC ==9.8.4\r\n  GHC ==9.10.1\r\n  GHC ==9.12.1\r\n\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/amesgen/safe-wild-cards\r\n\r\nlibrary\r\n  exposed-modules:\r\n    SafeWildCards\r\n  build-depends:\r\n    base <5,\r\n    template-haskell <2.24,\r\n    th-abstraction >=0.3 && <0.8\r\n  hs-source-dirs:\r\n    src\r\n  default-language:\r\n    Haskell2010\r\n  ghc-options:\r\n    -Wall\r\n\r\ntest-suite test\r\n  type: exitcode-stdio-1.0\r\n  main-is:\r\n    Test.hs\r\n  other-modules:\r\n    TestTypes\r\n  build-depends:\r\n    base,\r\n    safe-wild-cards\r\n  hs-source-dirs:\r\n    test\r\n  default-language:\r\n    Haskell2010\r\n  ghc-options:\r\n    -Wall\r\n";
  }