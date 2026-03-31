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
      identifier = { name = "vector-th-unbox"; version = "0.2.2"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2012-2015 Liyang HU";
      maintainer = "Fumiaki Kinoshita <fumiexcel@gmail.com>";
      author = "Liyang HU <vector-th-unbox@liyang.hu>";
      homepage = "https://github.com/tsurucapital/vector-th-unbox";
      url = "";
      synopsis = "Deriver for Data.Vector.Unboxed using Template Haskell";
      description = "A Template Haskell deriver for unboxed vectors, given a pair of coercion\nfunctions to and from some existing type with an Unbox instance.\n\nRefer to \"Data.Vector.Unboxed.Deriving\" for documentation and examples.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
        ];
        buildable = true;
      };
      tests = {
        "sanity" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."vector-th-unbox" or (errorHandler.buildDepError "vector-th-unbox"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/vector-th-unbox-0.2.2.tar.gz";
      sha256 = "8aa4ca464e842706e5b5234b8242d1aafec9ee755659b0e3ff44ecde13a80149";
    });
  }) // {
    package-description-override = "name:           vector-th-unbox\nversion:        0.2.2\nx-revision:     9\nsynopsis:       Deriver for Data.Vector.Unboxed using Template Haskell\ndescription:\n    A Template Haskell deriver for unboxed vectors, given a pair of coercion\n    functions to and from some existing type with an Unbox instance.\n    .\n    Refer to \"Data.Vector.Unboxed.Deriving\" for documentation and examples.\nstability:      experimental\nhomepage:       https://github.com/tsurucapital/vector-th-unbox\nlicense:        BSD3\nlicense-file:   LICENSE\ncopyright:      (c) 2012-2015 Liyang HU\nauthor:         Liyang HU <vector-th-unbox@liyang.hu>\nmaintainer:     Fumiaki Kinoshita <fumiexcel@gmail.com>\ncategory:       Data\nbuild-type:     Simple\ncabal-version:  >= 1.10\ntested-with:\n  GHC == 9.12,\n  GHC == 9.10,\n  GHC == 9.8,\n  GHC == 9.6,\n  GHC == 9.4,\n  GHC == 9.2,\n  GHC == 9.0,\n  GHC == 8.10,\n  GHC == 8.8,\n  GHC == 8.6,\n  GHC == 8.4,\n  GHC == 8.2,\n  GHC == 8.0\nextra-source-files:\n  CHANGELOG.md\n  README.md\n\nsource-repository head\n    type:       git\n    location:   http://github.com/tsurucapital/vector-th-unbox\n\nlibrary\n    default-language: Haskell2010\n    exposed-modules:\n        Data.Vector.Unboxed.Deriving\n\n    build-depends:\n        base >= 4.9 && < 4.23,\n        template-haskell >= 2.5 && <2.25,\n        vector >= 0.7.1 && <0.14\n\ntest-suite sanity\n    default-language: Haskell2010\n    type: exitcode-stdio-1.0\n    hs-source-dirs: tests\n    main-is: sanity.hs\n    build-depends:\n        base,\n        data-default,\n        vector,\n        vector-th-unbox\n    ghc-options: -Wall\n\n-- vim: et sw=4 ts=4 sts=4:\n\n";
  }