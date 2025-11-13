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
      identifier = { name = "heredoc"; version = "0.2.0.0"; };
      license = "LicenseRef-PublicDomain";
      copyright = "";
      maintainer = "jameshfisher@gmail.com";
      author = "James H. Fisher";
      homepage = "http://hackage.haskell.org/package/heredoc";
      url = "";
      synopsis = "multi-line string / here document using QuasiQuotes";
      description = "multi-line string / here document using QuasiQuotes";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/heredoc-0.2.0.0.tar.gz";
      sha256 = "c90d9fc61cb8cd812be510845493b6a6eddcc4b772581fd40a9433ed8f130f40";
    });
  }) // {
    package-description-override = "name:                heredoc\nversion:             0.2.0.0\nsynopsis:            multi-line string / here document using QuasiQuotes\ndescription:         multi-line string / here document using QuasiQuotes\n\nlicense:             PublicDomain\nlicense-file:        LICENSE.txt\n\nauthor:              James H. Fisher\nmaintainer:          jameshfisher@gmail.com\nhomepage:            http://hackage.haskell.org/package/heredoc\n\ncategory:            Text\n\nbuild-type:          Simple\ncabal-version:       >=1.8\n\nlibrary\n  exposed-modules:   Text.Heredoc\n  build-depends:\n    base >= 4 && < 5,\n    template-haskell >= 2.5\n  hs-source-dirs:    src\n";
  }