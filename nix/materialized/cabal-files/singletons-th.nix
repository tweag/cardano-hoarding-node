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
      identifier = { name = "singletons-th"; version = "3.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Ryan Scott <ryan.gl.scott@gmail.com>";
      author = "Richard Eisenberg <rae@cs.brynmawr.edu>, Jan Stolarek <jan.stolarek@p.lodz.pl>";
      homepage = "http://www.github.com/goldfirere/singletons";
      url = "";
      synopsis = "A framework for generating singleton types";
      description = "@singletons-th@ defines Template Haskell functionality that allows\n/promotion/ of term-level functions to type-level equivalents and\n/singling/ functions to dependently typed equivalents. This library was\noriginally presented in /Dependently Typed Programming with Singletons/,\npublished at the Haskell Symposium, 2012.\n(<https://richarde.dev/papers/2012/singletons/paper.pdf>)\nSee also the paper published at Haskell Symposium, 2014, which describes\nhow promotion works in greater detail:\n<https://richarde.dev/papers/2014/promotion/promotion.pdf>.\n\n@singletons-th@ generates code that relies on bleeding-edge GHC language\nextensions. As such, @singletons-th@ only supports the latest major version\nof GHC (currently GHC 9.6). For more information,\nconsult the @singletons@\n@<https://github.com/goldfirere/singletons/blob/master/README.md README>@.\n\nYou may also be interested in the following related libraries:\n\n* The @singletons@ library is a small, foundational library that defines\nbasic singleton-related types and definitions.\n\n* The @singletons-base@ library uses @singletons-th@ to define promoted and\nsingled functions from the @base@ library, including the \"Prelude\".";
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
      url = "http://hackage.haskell.org/package/singletons-th-3.2.tar.gz";
      sha256 = "775e084f4304e9aba82642da37d3e9111dade2eff2603538a70661ce134c95e4";
    });
  }) // {
    package-description-override = "name:           singletons-th\r\nversion:        3.2\r\ncabal-version:  1.24\r\nsynopsis:       A framework for generating singleton types\r\nhomepage:       http://www.github.com/goldfirere/singletons\r\ncategory:       Dependent Types\r\nauthor:         Richard Eisenberg <rae@cs.brynmawr.edu>, Jan Stolarek <jan.stolarek@p.lodz.pl>\r\nmaintainer:     Ryan Scott <ryan.gl.scott@gmail.com>\r\nbug-reports:    https://github.com/goldfirere/singletons/issues\r\nstability:      experimental\r\ntested-with:    GHC == 9.6.1\r\nextra-source-files: README.md, CHANGES.md\r\nlicense:        BSD3\r\nlicense-file:   LICENSE\r\nbuild-type:     Simple\r\ndescription:\r\n    @singletons-th@ defines Template Haskell functionality that allows\r\n    /promotion/ of term-level functions to type-level equivalents and\r\n    /singling/ functions to dependently typed equivalents. This library was\r\n    originally presented in /Dependently Typed Programming with Singletons/,\r\n    published at the Haskell Symposium, 2012.\r\n    (<https://richarde.dev/papers/2012/singletons/paper.pdf>)\r\n    See also the paper published at Haskell Symposium, 2014, which describes\r\n    how promotion works in greater detail:\r\n    <https://richarde.dev/papers/2014/promotion/promotion.pdf>.\r\n    .\r\n    @singletons-th@ generates code that relies on bleeding-edge GHC language\r\n    extensions. As such, @singletons-th@ only supports the latest major version\r\n    of GHC (currently GHC 9.6). For more information,\r\n    consult the @singletons@\r\n    @<https://github.com/goldfirere/singletons/blob/master/README.md README>@.\r\n    .\r\n    You may also be interested in the following related libraries:\r\n    .\r\n    * The @singletons@ library is a small, foundational library that defines\r\n      basic singleton-related types and definitions.\r\n    .\r\n    * The @singletons-base@ library uses @singletons-th@ to define promoted and\r\n      singled functions from the @base@ library, including the \"Prelude\".\r\n\r\nsource-repository this\r\n  type:     git\r\n  location: https://github.com/goldfirere/singletons.git\r\n  subdir:   singletons-th\r\n  tag:      v3.1.2\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/goldfirere/singletons.git\r\n  subdir:   singletons-th\r\n  branch:   master\r\n\r\nlibrary\r\n  hs-source-dirs:     src\r\n  build-depends:      base             >= 4.18 && < 4.19,\r\n                      containers       >= 0.5,\r\n                      mtl              >= 2.2.1 && < 2.4,\r\n                      ghc-boot-th,\r\n                      singletons       == 3.0.*,\r\n                      syb              >= 0.4,\r\n                      template-haskell >= 2.20 && < 2.21,\r\n                      th-desugar       >= 1.15 && < 1.16,\r\n                      th-orphans       >= 0.13.11 && < 0.14,\r\n                      transformers     >= 0.5.2\r\n  default-language:   GHC2021\r\n  other-extensions:   TemplateHaskellQuotes\r\n  exposed-modules:    Data.Singletons.TH\r\n                      Data.Singletons.TH.CustomStar\r\n                      Data.Singletons.TH.Options\r\n                      Data.Singletons.TH.SuppressUnusedWarnings\r\n\r\n  other-modules:      Data.Singletons.TH.Deriving.Bounded\r\n                      Data.Singletons.TH.Deriving.Enum\r\n                      Data.Singletons.TH.Deriving.Eq\r\n                      Data.Singletons.TH.Deriving.Foldable\r\n                      Data.Singletons.TH.Deriving.Functor\r\n                      Data.Singletons.TH.Deriving.Infer\r\n                      Data.Singletons.TH.Deriving.Ord\r\n                      Data.Singletons.TH.Deriving.Show\r\n                      Data.Singletons.TH.Deriving.Traversable\r\n                      Data.Singletons.TH.Deriving.Util\r\n                      Data.Singletons.TH.Names\r\n                      Data.Singletons.TH.Partition\r\n                      Data.Singletons.TH.Promote\r\n                      Data.Singletons.TH.Promote.Defun\r\n                      Data.Singletons.TH.Promote.Monad\r\n                      Data.Singletons.TH.Promote.Type\r\n                      Data.Singletons.TH.Single\r\n                      Data.Singletons.TH.Single.Data\r\n                      Data.Singletons.TH.Single.Decide\r\n                      Data.Singletons.TH.Single.Defun\r\n                      Data.Singletons.TH.Single.Fixity\r\n                      Data.Singletons.TH.Single.Monad\r\n                      Data.Singletons.TH.Single.Type\r\n                      Data.Singletons.TH.Syntax\r\n                      Data.Singletons.TH.Util\r\n\r\n  -- singletons re-exports\r\n  reexported-modules: Data.Singletons\r\n                    , Data.Singletons.Decide\r\n                    , Data.Singletons.ShowSing\r\n                    , Data.Singletons.Sigma\r\n\r\n  ghc-options:        -Wall -Wcompat\r\n";
  }