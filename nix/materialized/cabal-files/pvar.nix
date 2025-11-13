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
      specVersion = "1.12";
      identifier = { name = "pvar"; version = "1.0.0.0"; };
      license = "BSD-3-Clause";
      copyright = "2020 Alexey Kuleshevich";
      maintainer = "alexey@kuleshevi.ch";
      author = "Alexey Kuleshevich";
      homepage = "https://github.com/lehins/pvar#readme";
      url = "";
      synopsis = "Mutable variable with primitive values";
      description = "Mutable variable `PVar` that is backed by a single value `MutableByteArray`";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
        ];
        buildable = true;
      };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."pvar" or (errorHandler.buildDepError "pvar"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."genvalidity" or (errorHandler.buildDepError "genvalidity"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/pvar-1.0.0.0.tar.gz";
      sha256 = "96a683b532ea7ccda7813e09147a9da65578e9385d8001607dadd19fd0e24838";
    });
  }) // {
    package-description-override = "cabal-version: 1.12\r\n\r\nname:           pvar\r\nversion:        1.0.0.0\r\nx-revision: 1\r\nsynopsis:       Mutable variable with primitive values\r\ndescription:    Mutable variable `PVar` that is backed by a single value `MutableByteArray`\r\nhomepage:       https://github.com/lehins/pvar#readme\r\nbug-reports:    https://github.com/lehins/pvar/issues\r\nauthor:         Alexey Kuleshevich\r\nmaintainer:     alexey@kuleshevi.ch\r\ncopyright:      2020 Alexey Kuleshevich\r\ncategory:       Data\r\nlicense:        BSD3\r\nlicense-file:   LICENSE\r\nbuild-type:     Simple\r\ntested-with:    GHC == 8.0.1\r\n              , GHC == 8.0.2\r\n              , GHC == 8.2.2\r\n              , GHC == 8.4.3\r\n              , GHC == 8.4.4\r\n              , GHC == 8.6.3\r\n              , GHC == 8.6.4\r\n              , GHC == 8.6.5\r\n              , GHC == 8.8.2\r\n              , GHC == 8.8.3\r\nextra-source-files: README.md\r\n                  , CHANGELOG.md\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/lehins/pvar\r\n\r\nlibrary\r\n  exposed-modules: Data.Primitive.PVar\r\n                 , Data.Primitive.PVar.Unsafe\r\n  other-modules: Data.Primitive.PVar.Internal\r\n  hs-source-dirs: src\r\n  build-depends: base >=4.9 && <6\r\n               , deepseq\r\n               , primitive >= 0.7.1.0\r\n  default-language: Haskell2010\r\n  ghc-options: -Wall\r\n               -Wno-simplifiable-class-constraints\r\n  if impl(ghc < 8.2)\r\n    c-sources: cbits/pvar.c\r\n\r\ntest-suite tests\r\n  type: exitcode-stdio-1.0\r\n  main-is: Main.hs\r\n  other-modules: Spec\r\n               , Test.Primitive.PVarSpec\r\n  hs-source-dirs: tests\r\n  ghc-options: -Wall -threaded -rtsopts -with-rtsopts=-N\r\n  build-depends: base >=4.7 && <5\r\n               , async\r\n               , deepseq\r\n               , pvar\r\n               , primitive\r\n               , hspec\r\n               , QuickCheck\r\n               , genvalidity\r\n  default-language: Haskell2010\r\n";
  }