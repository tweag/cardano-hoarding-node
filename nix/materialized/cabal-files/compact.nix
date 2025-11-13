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
      identifier = { name = "compact"; version = "0.2.0.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2017 Edward Z. Yang, Ben Gamari";
      maintainer = "ezyang@mit.edu, ben@smart-cactus.org";
      author = "Edward Z. Yang, Ben Gamari";
      homepage = "https://github.com/ezyang/compact";
      url = "";
      synopsis = "Non-GC'd, contiguous storage for immutable data structures";
      description = "This package provides user-facing APIs for working with\n\"compact regions\", which hold a fully evaluated Haskell object graph.\nThese regions maintain the invariant that no pointers live inside the struct\nthat point outside it, which ensures efficient garbage collection without\never reading the structure contents (effectively, it works as a manually\nmanaged \"oldest generation\" which is never freed until the whole is\nreleased).\nThis package is currently highly experimental, but we hope it may\nbe useful to some people.  It is GHC 8.2 and later only.\nThe bare-bones library that ships with GHC is @ghc-compact@.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ghc-compact" or (errorHandler.buildDepError "ghc-compact"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
        ];
        buildable = true;
      };
      tests = {
        "compact-test" = {
          depends = [
            (hsPkgs."compact" or (errorHandler.buildDepError "compact"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/compact-0.2.0.0.tar.gz";
      sha256 = "03a6b534f4f8f06fe1f3921be74f90c2fab9da7f28e7520612c91c28da266277";
    });
  }) // {
    package-description-override = "name:                compact\r\nversion:             0.2.0.0\r\nx-revision: 4\r\nsynopsis:            Non-GC'd, contiguous storage for immutable data structures\r\ndescription:\r\n    This package provides user-facing APIs for working with\r\n    \"compact regions\", which hold a fully evaluated Haskell object graph.\r\n    These regions maintain the invariant that no pointers live inside the struct\r\n    that point outside it, which ensures efficient garbage collection without\r\n    ever reading the structure contents (effectively, it works as a manually\r\n    managed \"oldest generation\" which is never freed until the whole is\r\n    released).\r\n\r\n    This package is currently highly experimental, but we hope it may\r\n    be useful to some people.  It is GHC 8.2 and later only.  \r\n    The bare-bones library that ships with GHC is @ghc-compact@.\r\nlicense:             BSD3\r\nlicense-file:        LICENSE\r\nauthor:              Edward Z. Yang, Ben Gamari\r\nmaintainer:          ezyang@mit.edu, ben@smart-cactus.org\r\nhomepage:            https://github.com/ezyang/compact\r\ncopyright:           (c) 2017 Edward Z. Yang, Ben Gamari\r\ncategory:            Data\r\nbuild-type:          Simple\r\nextra-source-files:  ChangeLog.md\r\ncabal-version:       >=1.10\r\nextra-source-files:  README.md tests/sample1.hs tests/sample2.hs\r\ntested-with:         GHC==8.2.1, GHC==8.2.2,\r\n                     GHC==8.4.1, GHC==8.4.2, GHC==8.4.3, GHC==8.4.4,\r\n                     GHC==8.6.1, GHC==8.6.2, GHC==8.6.3, GHC==8.6.4, GHC==8.6.5,\r\n                     GHC==8.8.1, GHC==8.8.2, GHC==8.8.3,\r\n                     GHC==8.10.1\r\n\r\nsource-repository head\r\n  type:                git\r\n  location:            https://github.com/ezyang/compact\r\n\r\nlibrary\r\n  exposed-modules:     Data.Compact\r\n                       Data.Compact.Serialize\r\n  build-depends:       base       >= 4.10 && < 4.22,\r\n                       ghc-compact,\r\n                       -- TODO: SomeTypeRep instance is in unreleased version\r\n                       -- of binary that is bundled with GHC dev branch\r\n                       binary     >= 0.8.4.1 && < 0.11,\r\n                       bytestring >= 0.10 && < 0.13\r\n  default-language:    Haskell2010\r\n\r\ntest-suite compact-test\r\n  type:                exitcode-stdio-1.0\r\n  hs-source-dirs:      tests\r\n  main-is:             compact-test.hs\r\n  build-depends:       compact, base, directory\r\n  default-language:    Haskell2010\r\n";
  }