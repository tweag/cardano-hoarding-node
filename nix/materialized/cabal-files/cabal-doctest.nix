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
      identifier = { name = "cabal-doctest"; version = "1.0.11"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2017-2020 Oleg Grenrus, 2020- package maintainers";
      maintainer = "Max Ulidtko <ulidtko@gmail.com>";
      author = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      homepage = "https://github.com/ulidtko/cabal-doctest";
      url = "";
      synopsis = "A Setup.hs helper for running doctests";
      description = "As of now (end of 2024), there isn't @cabal doctest@\ncommand. Yet, to properly work, @doctest@ needs plenty of configuration.\nThis library provides the common bits for writing a custom @Setup.hs@.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/cabal-doctest-1.0.11.tar.gz";
      sha256 = "dc2df2f0b427028617f10e445ff8ab3352d3bbeef1b8dd847e879d2d36bf923f";
    });
  }) // {
    package-description-override = "name:               cabal-doctest\nversion:            1.0.11\n-- x-revision:      0\nsynopsis:           A Setup.hs helper for running doctests\ndescription:\n  As of now (end of 2024), there isn't @cabal doctest@\n  command. Yet, to properly work, @doctest@ needs plenty of configuration.\n  This library provides the common bits for writing a custom @Setup.hs@.\n\nhomepage:           https://github.com/ulidtko/cabal-doctest\nlicense:            BSD3\nlicense-file:       LICENSE\nauthor:             Oleg Grenrus <oleg.grenrus@iki.fi>\nmaintainer:         Max Ulidtko <ulidtko@gmail.com>\ncopyright:          (c) 2017-2020 Oleg Grenrus, 2020- package maintainers\ncategory:           Distribution\nbuild-type:         Simple\ncabal-version:      >=1.10\nextra-source-files:\n  changelog.md\n  README.md\n\ntested-with:\n  GHC == 9.10.1\n  GHC == 9.8.2\n  GHC == 9.6.5\n  GHC == 9.4.8\n  GHC == 9.2.8\n  GHC == 9.0.2\n  GHC == 8.10.7\n  GHC == 8.8.4\n  GHC == 8.6.5\n  GHC == 8.4.4\n  GHC == 8.2.2\n  GHC == 8.0.2\n  -- 2023-10-14: Dropped CI support for GHC 7.x\n\nsource-repository head\n  type:     git\n  location: https://github.com/ulidtko/cabal-doctest\n\nlibrary\n  exposed-modules:  Distribution.Extra.Doctest\n  other-modules:\n  other-extensions:\n  build-depends:\n    -- NOTE: contrary to PVP, some upper-bounds are intentionally set to major-major.\n    -- This is to increase signal-to-noise ratio of CI failures. \"Too tight bounds\"\n    -- is an extremely boring (and practically guaranteed, repeatedly) failure mode.\n    -- OTOH, genuine build failures due to breaking changes in dependencies are:\n    --  1) unlikely to occur, as this package is so small, moreso regularly;\n    --  2) best caught in CI pipelines that don't induce alert fatigue.\n    -- In any case, revisions may set tighter bounds afterwards, if exceptional\n    -- circumstances would warrant that.\n      base       >=4.9  && <5\n    , Cabal      >=1.24 && <3.16\n    , directory  >=1.3  && <2\n    , filepath   >=1.4  && <2\n\n  hs-source-dirs:   src\n  default-language: Haskell2010\n  ghc-options:      -Wall\n";
  }