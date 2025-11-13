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
      specVersion = "1.10";
      identifier = { name = "generic-lens"; version = "2.2.2.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "kiss.csongor.kiss@gmail.com";
      author = "Csongor Kiss";
      homepage = "https://github.com/kcsongor/generic-lens";
      url = "";
      synopsis = "Generically derive traversals, lenses and prisms.";
      description = "This library uses GHC.Generics to derive efficient optics (traversals, lenses and prisms) for algebraic data types in a type-directed way, with a focus on good type inference and error messages when possible.\n\nThe library exposes a van Laarhoven interface. For an alternative interface, supporting an opaque optic type, see\n@<https://hackage.haskell.org/package/generic-optics generic-optics>@.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."generic-lens-core" or (errorHandler.buildDepError "generic-lens-core"))
          (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
        ];
        buildable = true;
      };
      tests = {
        "inspection-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
            (hsPkgs."inspection-testing" or (errorHandler.buildDepError "inspection-testing"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
          ];
          buildable = true;
        };
        "generic-lens-bifunctor" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
          ];
          buildable = true;
        };
        "generic-lens-syb-tree" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
          ];
          buildable = true;
        };
        "doctests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/generic-lens-2.2.2.0.tar.gz";
      sha256 = "868dc9c8cd02150b419859c3c8a53a62e5b41f5f3fd5d46bb355eb1074288b68";
    });
  }) // {
    package-description-override = "cabal-version:        >= 1.10\nname:                 generic-lens\nversion:              2.2.2.0\nx-revision:           1\nsynopsis:             Generically derive traversals, lenses and prisms.\ndescription:          This library uses GHC.Generics to derive efficient optics (traversals, lenses and prisms) for algebraic data types in a type-directed way, with a focus on good type inference and error messages when possible.\n                      .\n                      The library exposes a van Laarhoven interface. For an alternative interface, supporting an opaque optic type, see\n                      @<https://hackage.haskell.org/package/generic-optics generic-optics>@.\n\nhomepage:             https://github.com/kcsongor/generic-lens\nlicense:              BSD3\nlicense-file:         LICENSE\nauthor:               Csongor Kiss\nmaintainer:           kiss.csongor.kiss@gmail.com\ncategory:             Generics, Records, Lens\nbuild-type:           Simple\n\ntested-with:\n  GHC == 9.8.1\n  GHC == 9.6.3\n  GHC == 9.4.8\n  GHC == 9.2.8\n  GHC == 9.0.2\n  GHC == 8.10.7\n  GHC == 8.8.4\n  GHC == 8.6.5\n  GHC == 8.4.4\n\n\nextra-source-files:   examples/StarWars.hs\n                    , examples/Examples.hs\n                    , ChangeLog.md\n\nlibrary\n  exposed-modules:    Data.Generics.Wrapped\n                    , Data.Generics.Product\n                    , Data.Generics.Product.Any\n                    , Data.Generics.Product.Fields\n                    , Data.Generics.Product.Param\n                    , Data.Generics.Product.Positions\n                    , Data.Generics.Product.Subtype\n                    , Data.Generics.Product.Typed\n                    , Data.Generics.Product.Types\n                    , Data.Generics.Product.HList\n                    , Data.Generics.Labels\n\n                    , Data.Generics.Sum\n                    , Data.Generics.Sum.Any\n                    , Data.Generics.Sum.Constructors\n                    , Data.Generics.Sum.Typed\n                    , Data.Generics.Sum.Subtype\n\n                    , Data.Generics.Internal.VL\n                    , Data.Generics.Internal.VL.Lens\n                    , Data.Generics.Internal.VL.Prism\n                    , Data.Generics.Internal.VL.Iso\n\n  build-depends:      base        >= 4.11 && < 5\n                    , generic-lens-core == 2.2.1.0\n                    , profunctors\n                    , text        >= 1.2 && < 1.3 || >= 2.0 && < 2.2\n\n  hs-source-dirs:     src\n  default-language:   Haskell2010\n  ghc-options:        -Wall\n\ntest-suite inspection-tests\n  type:               exitcode-stdio-1.0\n  hs-source-dirs:     test\n  main-is:            Spec.hs\n  other-modules:      Util Test24 Test88 Test25 Test40 Test62 Test63 CustomChildren\n\n  build-depends:      base\n                    , generic-lens\n                    , lens\n                    , profunctors\n                    , inspection-testing >= 0.2\n                    , HUnit\n\n  default-language:   Haskell2010\n  ghc-options:        -Wall\n\ntest-suite generic-lens-bifunctor\n  type:               exitcode-stdio-1.0\n  hs-source-dirs:     test\n  main-is:            Bifunctor.hs\n\n  build-depends:      base\n                    , generic-lens\n                    , lens\n                    , HUnit\n\n  default-language:   Haskell2010\n  ghc-options:        -Wall\n\ntest-suite generic-lens-syb-tree\n  type:               exitcode-stdio-1.0\n  hs-source-dirs:     test/syb\n  main-is:            Tree.hs\n\n  build-depends:      base\n                    , generic-lens\n                    , lens\n                    , profunctors\n                    , HUnit\n\n  default-language:   Haskell2010\n  ghc-options:        -Wall\n\ntest-suite doctests\n  default-language:   Haskell2010\n  type:               exitcode-stdio-1.0\n  ghc-options:        -threaded\n  main-is:            doctest.hs\n  build-depends:      base >4 && <5\n                    , doctest\n                    , lens\n  hs-source-dirs:     examples\n";
  }