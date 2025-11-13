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
    flags = { build-readme = true; };
    package = {
      specVersion = "2.0";
      identifier = { name = "constraints-extras"; version = "0.4.0.2"; };
      license = "BSD-3-Clause";
      copyright = "Obsidian Systems LLC";
      maintainer = "maintainer@obsidian.systems";
      author = "Cale Gibbard, Ali Abrar";
      homepage = "https://github.com/obsidiansystems/constraints-extras";
      url = "";
      synopsis = "Utility package for constraints";
      description = "Convenience functions and TH for working with constraints. See <https://github.com/obsidiansystems/constraints-extras/blob/develop/README.md README.md> for example usage.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."constraints" or (errorHandler.buildDepError "constraints"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
        ] ++ pkgs.lib.optional (compiler.isGhc && compiler.version.ge "9.12") (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"));
        buildable = true;
      };
      exes = {
        "readme" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."constraints" or (errorHandler.buildDepError "constraints"))
            (hsPkgs."constraints-extras" or (errorHandler.buildDepError "constraints-extras"))
          ];
          buildable = if !flags.build-readme then false else true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/constraints-extras-0.4.0.2.tar.gz";
      sha256 = "cc7fdb2625f1baeb16501176d8e2eb6da16a3448f2f0a37e188289c503cb482c";
    });
  }) // {
    package-description-override = "name: constraints-extras\nversion: 0.4.0.2\nsynopsis: Utility package for constraints\ndescription: Convenience functions and TH for working with constraints. See <https://github.com/obsidiansystems/constraints-extras/blob/develop/README.md README.md> for example usage.\ncategory: Constraints\nlicense: BSD3\nlicense-file: LICENSE\nauthor: Cale Gibbard, Ali Abrar\nmaintainer: maintainer@obsidian.systems\nhomepage: https://github.com/obsidiansystems/constraints-extras\nbug-reports: https://github.com/obsidiansystems/constraints-extras/issues\ncopyright: Obsidian Systems LLC\nbuild-type: Simple\ncabal-version: 2.0\ntested-with:\n  GHC ==8.6.5 || ==8.8.4 || ==8.10.7\n   || ==9.0.2 || ==9.2.8 || ==9.4.8 || ==9.6.6 || ==9.8.4 || ==9.10.1 || ==9.12.1\nextra-doc-files: ChangeLog.md\nextra-source-files: README.md\n\nflag build-readme\n  default: True\n\nlibrary\n  exposed-modules: Data.Constraint.Extras\n                 , Data.Constraint.Extras.TH\n                 , Data.Constraint.Compose\n                 , Data.Constraint.Flip\n  other-extensions: LambdaCase\n                  , MultiParamTypeClasses\n                  , QuasiQuotes\n                  , TypeFamilies\n                  , TypeOperators\n                  , ConstraintKinds\n                  , TemplateHaskell\n  build-depends: base >=4.9 && <4.23\n               , constraints >= 0.9 && < 0.15\n               , template-haskell >=2.11 && <2.24\n\n  hs-source-dirs:  src\n  default-language: Haskell2010\n\n  -- This is needed to get around a bug/misfeature in the cabal solver which is choosing an\n  -- old version of aeson (0.9.*) for some reason.\n  if impl (ghc >= 9.12)\n    build-depends: aeson >= 2\n\n\nexecutable readme\n  if !flag(build-readme)\n    buildable: False\n  build-depends: base\n               , aeson\n               , constraints\n               , constraints-extras\n  main-is: README.lhs\n  ghc-options: -Wall -optL -q\n  default-language: Haskell2010\n\nsource-repository head\n  type:     git\n  location: git://github.com/obsidiansystems/constraints-extras.git\n";
  }