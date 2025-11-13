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
      identifier = { name = "size-based"; version = "0.1.3.3"; };
      license = "BSD-3-Clause";
      copyright = "(c) Jonas Duregård";
      maintainer = "byorgey@gmail.com";
      author = "Jonas Duregård";
      homepage = "";
      url = "";
      synopsis = "Sized functors, for size-based enumerations";
      description = "A framework for size-based enumerations. See the module documentation for details.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."dictionary-sharing" or (errorHandler.buildDepError "dictionary-sharing"))
          (hsPkgs."testing-type-modifiers" or (errorHandler.buildDepError "testing-type-modifiers"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/size-based-0.1.3.3.tar.gz";
      sha256 = "2fe4ea0c7fb7f83819c2075bfed933c569ce85e64664084faa30ed537c0c83f5";
    });
  }) // {
    package-description-override = "name:                size-based\r\nversion:             0.1.3.3\r\nx-revision: 1\r\nsynopsis:            Sized functors, for size-based enumerations\r\ndescription:         A framework for size-based enumerations. See the module documentation for details.\r\n\r\nlicense:             BSD3\r\nlicense-file:        LICENSE\r\nauthor:              Jonas Duregård\r\nmaintainer:          byorgey@gmail.com\r\ncopyright:           (c) Jonas Duregård\r\ncategory:            Data\r\nbuild-type:          Simple\r\ncabal-version:       >=1.10\r\nextra-source-files:  CHANGELOG.md\r\ntested-with:         GHC ==8.6.5 || ==8.8.4 || ==8.10.7 || ==9.0.2 || ==9.2.8 || ==9.4.8 || ==9.6.6 || ==9.8.4 || ==9.10.1 || ==9.12.1\r\n\r\nsource-repository head\r\n  type:      git\r\n  location:  https://github.com/size-based/size-based\r\n\r\nlibrary\r\n  exposed-modules:\r\n    Control.Sized\r\n    Control.Enumerable\r\n    Control.Enumerable.Count\r\n    Control.Enumerable.Values\r\n--    Control.Enumerable.Functions\r\n--    Control.Enumerable.LazySearch\r\n\r\n  other-modules:\r\n    Control.Enumerable.Derive\r\n  other-extensions:    GADTs, DeriveDataTypeable\r\n  build-depends:       base >=4.9 && <5,\r\n                       dictionary-sharing >= 0.1 && < 1.0,\r\n                       testing-type-modifiers >= 0.1 && < 1.0,\r\n                       template-haskell  <2.24\r\n  default-language:    Haskell2010\r\n";
  }