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
      specVersion = "1.8";
      identifier = { name = "hspec-expectations-lifted"; version = "0.10.0"; };
      license = "MIT";
      copyright = "(c) 2011-2016 Simon Hengel";
      maintainer = "Simon Hengel <sol@typeful.net>";
      author = "Simon Hengel <sol@typeful.net>";
      homepage = "";
      url = "";
      synopsis = "A version of hspec-expectations generalized to MonadIO";
      description = "A version of hspec-expectations generalized to MonadIO";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."hspec-expectations" or (errorHandler.buildDepError "hspec-expectations"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hspec-expectations-lifted-0.10.0.tar.gz";
      sha256 = "22cdf1509b396fea2f53a0bb88dec3552f540d58cc60962a82970264c1e73828";
    });
  }) // {
    package-description-override = "name:             hspec-expectations-lifted\nversion:          0.10.0\nsynopsis:         A version of hspec-expectations generalized to MonadIO\ndescription:      A version of hspec-expectations generalized to MonadIO\nlicense:          MIT\nlicense-file:     LICENSE\ncopyright:        (c) 2011-2016 Simon Hengel\nauthor:           Simon Hengel <sol@typeful.net>\nmaintainer:       Simon Hengel <sol@typeful.net>\nbuild-type:       Simple\ncategory:         Testing\ncabal-version:    >= 1.8\n\nsource-repository head\n  type: git\n  location: https://github.com/hspec/hspec-expectations-lifted\n\nlibrary\n  ghc-options:\n      -Wall\n  build-depends:\n      base == 4.*\n    , hspec-expectations >= 0.8.2\n    , transformers\n  hs-source-dirs:\n      src\n  exposed-modules:\n      Test.Hspec.Expectations.Lifted\n";
  }