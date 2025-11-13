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
    flags = { fast = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "criterion-measurement"; version = "0.2.3.0"; };
      license = "BSD-3-Clause";
      copyright = "2009-2016 Bryan O'Sullivan and others";
      maintainer = "Marco Zocca <zocca.marco gmail>, Ryan Scott <ryan.gl.scott@gmail.com>";
      author = "Bryan O'Sullivan <bos@serpentine.com>";
      homepage = "https://github.com/haskell/criterion";
      url = "";
      synopsis = "Criterion measurement functionality and associated types";
      description = "Measurement-related functionality extracted from Criterion, with minimal dependencies. The rationale for this is to enable alternative analysis front-ends.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/criterion-measurement-0.2.3.0.tar.gz";
      sha256 = "cc75dca35e8473d6e0c7419fd5a577c2cbb202b3ae0d8af237756c2aeb7cdfe2";
    });
  }) // {
    package-description-override = "name:                criterion-measurement\nversion:             0.2.3.0\nsynopsis:            Criterion measurement functionality and associated types\ndescription:         Measurement-related functionality extracted from Criterion, with minimal dependencies. The rationale for this is to enable alternative analysis front-ends.\nhomepage:            https://github.com/haskell/criterion\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Bryan O'Sullivan <bos@serpentine.com>\nmaintainer:          Marco Zocca <zocca.marco gmail>, Ryan Scott <ryan.gl.scott@gmail.com>\ncopyright:           2009-2016 Bryan O'Sullivan and others\ncategory:            Development, Performance, Testing, Benchmarking\nbuild-type:          Simple\nextra-source-files:  README.md, changelog.md\ncabal-version:       >=1.10\ntested-with:\n  GHC==8.0.2,\n  GHC==8.2.2,\n  GHC==8.4.4,\n  GHC==8.6.5,\n  GHC==8.8.4,\n  GHC==8.10.7,\n  GHC==9.0.2,\n  GHC==9.2.8,\n  GHC==9.4.8,\n  GHC==9.6.6,\n  GHC==9.8.2,\n  GHC==9.10.1\n\nflag fast\n  description: compile without optimizations\n  default: False\n  manual: True\n\nlibrary\n  hs-source-dirs:      src\n  exposed-modules:     Criterion.Measurement\n                       Criterion.Measurement.Types\n                       Criterion.Measurement.Types.Internal\n  build-depends:       aeson >= 2 && < 2.3\n                     , base >= 4.9 && < 5\n                     , base-compat >= 0.9\n                     , binary >= 0.8.3.0\n                     , containers\n                     , deepseq >= 1.1.0.0\n                     , ghc-prim >= 0.5\n                     , vector >= 0.7.1\n\n  default-language: Haskell2010\n  ghc-options: -Wall -funbox-strict-fields -Wtabs\n  if flag(fast)\n    ghc-options: -O0\n  else\n    ghc-options: -O2\n\n\n  c-sources: cbits/cycles.c\n  if os(darwin)\n    c-sources: cbits/time-osx.c\n  else {\n    if os(windows)\n      c-sources: cbits/time-windows.c\n    else\n      c-sources: cbits/time-posix.c\n  }\n\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskell/criterion\n  subdir:   criterion-measurement\n";
  }