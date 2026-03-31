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
      specVersion = "1.24";
      identifier = { name = "singletons-th"; version = "3.4"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Ryan Scott <ryan.gl.scott@gmail.com>";
      author = "Richard Eisenberg <rae@cs.brynmawr.edu>, Jan Stolarek <jan.stolarek@p.lodz.pl>";
      homepage = "http://www.github.com/goldfirere/singletons";
      url = "";
      synopsis = "A framework for generating singleton types";
      description = "@singletons-th@ defines Template Haskell functionality that allows\n/promotion/ of term-level functions to type-level equivalents and\n/singling/ functions to dependently typed equivalents. This library was\noriginally presented in /Dependently Typed Programming with Singletons/,\npublished at the Haskell Symposium, 2012.\n(<https://richarde.dev/papers/2012/singletons/paper.pdf>)\nSee also the paper published at Haskell Symposium, 2014, which describes\nhow promotion works in greater detail:\n<https://richarde.dev/papers/2014/promotion/promotion.pdf>.\n\n@singletons-th@ generates code that relies on bleeding-edge GHC language\nextensions. As such, @singletons-th@ only supports the latest major version\nof GHC (currently GHC 9.10). For more information,\nconsult the @singletons@\n@<https://github.com/goldfirere/singletons/blob/master/README.md README>@.\n\nYou may also be interested in the following related libraries:\n\n* The @singletons@ library is a small, foundational library that defines\nbasic singleton-related types and definitions.\n\n* The @singletons-base@ library uses @singletons-th@ to define promoted and\nsingled functions from the @base@ library, including the \"Prelude\".";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."ghc-boot-th" or (errorHandler.buildDepError "ghc-boot-th"))
          (hsPkgs."singletons" or (errorHandler.buildDepError "singletons"))
          (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."th-desugar" or (errorHandler.buildDepError "th-desugar"))
          (hsPkgs."th-orphans" or (errorHandler.buildDepError "th-orphans"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/singletons-th-3.4.tar.gz";
      sha256 = "a1c47fa40361d21b16d64dd4c0d9d90d66e9ff50a394afbfe17a804780546eba";
    });
  }) // {
    package-description-override = "name:           singletons-th\nversion:        3.4\ncabal-version:  1.24\nsynopsis:       A framework for generating singleton types\nhomepage:       http://www.github.com/goldfirere/singletons\ncategory:       Dependent Types\nauthor:         Richard Eisenberg <rae@cs.brynmawr.edu>, Jan Stolarek <jan.stolarek@p.lodz.pl>\nmaintainer:     Ryan Scott <ryan.gl.scott@gmail.com>\nbug-reports:    https://github.com/goldfirere/singletons/issues\nstability:      experimental\ntested-with:    GHC == 9.10.1\nextra-source-files: README.md, CHANGES.md\nlicense:        BSD3\nlicense-file:   LICENSE\nbuild-type:     Simple\ndescription:\n    @singletons-th@ defines Template Haskell functionality that allows\n    /promotion/ of term-level functions to type-level equivalents and\n    /singling/ functions to dependently typed equivalents. This library was\n    originally presented in /Dependently Typed Programming with Singletons/,\n    published at the Haskell Symposium, 2012.\n    (<https://richarde.dev/papers/2012/singletons/paper.pdf>)\n    See also the paper published at Haskell Symposium, 2014, which describes\n    how promotion works in greater detail:\n    <https://richarde.dev/papers/2014/promotion/promotion.pdf>.\n    .\n    @singletons-th@ generates code that relies on bleeding-edge GHC language\n    extensions. As such, @singletons-th@ only supports the latest major version\n    of GHC (currently GHC 9.10). For more information,\n    consult the @singletons@\n    @<https://github.com/goldfirere/singletons/blob/master/README.md README>@.\n    .\n    You may also be interested in the following related libraries:\n    .\n    * The @singletons@ library is a small, foundational library that defines\n      basic singleton-related types and definitions.\n    .\n    * The @singletons-base@ library uses @singletons-th@ to define promoted and\n      singled functions from the @base@ library, including the \"Prelude\".\n\nsource-repository this\n  type:     git\n  location: https://github.com/goldfirere/singletons.git\n  subdir:   singletons-th\n  tag:      v3.1.2\n\nsource-repository head\n  type:     git\n  location: https://github.com/goldfirere/singletons.git\n  subdir:   singletons-th\n  branch:   master\n\nlibrary\n  hs-source-dirs:     src\n  build-depends:      base             >= 4.20 && < 4.21,\n                      containers       >= 0.5,\n                      mtl              >= 2.2.1 && < 2.4,\n                      ghc-boot-th,\n                      singletons       == 3.0.*,\n                      syb              >= 0.4,\n                      template-haskell >= 2.22 && < 2.23,\n                      th-desugar       >= 1.17 && < 1.18,\n                      th-orphans       >= 0.13.11 && < 0.14,\n                      transformers     >= 0.5.2\n  default-language:   GHC2021\n  other-extensions:   TemplateHaskellQuotes\n  exposed-modules:    Data.Singletons.TH\n                      Data.Singletons.TH.CustomStar\n                      Data.Singletons.TH.Options\n                      Data.Singletons.TH.SuppressUnusedWarnings\n\n  other-modules:      Data.Singletons.TH.Deriving.Bounded\n                      Data.Singletons.TH.Deriving.Enum\n                      Data.Singletons.TH.Deriving.Eq\n                      Data.Singletons.TH.Deriving.Foldable\n                      Data.Singletons.TH.Deriving.Functor\n                      Data.Singletons.TH.Deriving.Infer\n                      Data.Singletons.TH.Deriving.Ord\n                      Data.Singletons.TH.Deriving.Show\n                      Data.Singletons.TH.Deriving.Traversable\n                      Data.Singletons.TH.Deriving.Util\n                      Data.Singletons.TH.Names\n                      Data.Singletons.TH.Partition\n                      Data.Singletons.TH.Promote\n                      Data.Singletons.TH.Promote.Defun\n                      Data.Singletons.TH.Promote.Monad\n                      Data.Singletons.TH.Promote.Type\n                      Data.Singletons.TH.Single\n                      Data.Singletons.TH.Single.Data\n                      Data.Singletons.TH.Single.Decide\n                      Data.Singletons.TH.Single.Defun\n                      Data.Singletons.TH.Single.Fixity\n                      Data.Singletons.TH.Single.Monad\n                      Data.Singletons.TH.Single.Ord\n                      Data.Singletons.TH.Single.Type\n                      Data.Singletons.TH.Syntax\n                      Data.Singletons.TH.Util\n\n  -- singletons re-exports\n  reexported-modules: Data.Singletons\n                    , Data.Singletons.Decide\n                    , Data.Singletons.ShowSing\n                    , Data.Singletons.Sigma\n\n  ghc-options:        -Wall -Wcompat\n";
  }