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
      specVersion = "1.6";
      identifier = { name = "dependent-map"; version = "0.4.0.0"; };
      license = "LicenseRef-OtherLicense";
      copyright = "";
      maintainer = "Obsidian Systems, LLC <maintainer@obsidian.systems>";
      author = "James Cook <mokus@deepbondi.net>";
      homepage = "https://github.com/obsidiansystems/dependent-map";
      url = "";
      synopsis = "Dependent finite maps (partial dependent products)";
      description = "Provides a type called @DMap@ which generalizes\n@Data.Map.Map@, allowing keys to specify the type\nof value that can be associated with them.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."dependent-sum" or (errorHandler.buildDepError "dependent-sum"))
          (hsPkgs."constraints-extras" or (errorHandler.buildDepError "constraints-extras"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/dependent-map-0.4.0.0.tar.gz";
      sha256 = "53ce0b52d8be1b85fc6489fb27656f16d837bee4fbe0ddf39c844e3ea8871f2c";
    });
  }) // {
    package-description-override = "name:                   dependent-map\r\nversion:                0.4.0.0\r\nx-revision: 2\r\nstability:              provisional\r\n\r\ncabal-version:          >= 1.6\r\nbuild-type:             Simple\r\n\r\nauthor:                 James Cook <mokus@deepbondi.net>\r\nmaintainer:             Obsidian Systems, LLC <maintainer@obsidian.systems>\r\nlicense:                OtherLicense\r\nlicense-file:           LICENSE\r\nhomepage:               https://github.com/obsidiansystems/dependent-map\r\n\r\ncategory:               Data, Dependent Types\r\nsynopsis:               Dependent finite maps (partial dependent products)\r\ndescription:            Provides a type called @DMap@ which generalizes\r\n                        @Data.Map.Map@, allowing keys to specify the type\r\n                        of value that can be associated with them.\r\n\r\nextra-source-files: ChangeLog.md\r\n                    README.md\r\n\r\ntested-with:            GHC == 8.0.2,\r\n                        GHC == 8.2.2,\r\n                        GHC == 8.4.4,\r\n                        GHC == 8.6.5,\r\n                        GHC == 8.8.3\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/obsidiansystems/dependent-map\r\n\r\nLibrary\r\n  hs-source-dirs:       src\r\n  ghc-options:          -fwarn-unused-imports -fwarn-unused-binds\r\n  exposed-modules:      Data.Dependent.Map,\r\n                        Data.Dependent.Map.Lens,\r\n                        Data.Dependent.Map.Internal\r\n  other-modules:        Data.Dependent.Map.PtrEquality\r\n  build-depends:        base >= 4.9 && < 5,\r\n                        containers >= 0.5.7.1 && <0.8,\r\n                        dependent-sum >= 0.6.1 && < 0.8,\r\n                        constraints-extras >= 0.2.3.0 && < 0.5\r\n";
  }