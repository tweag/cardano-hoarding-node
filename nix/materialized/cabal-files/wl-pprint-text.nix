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
      identifier = { name = "wl-pprint-text"; version = "1.2.0.2"; };
      license = "BSD-3-Clause";
      copyright = "2007 Daan Leijen, 2010 Ivan Lazar Miljenovic";
      maintainer = "Ivan.Miljenovic@gmail.com";
      author = "Ivan Lazar Miljenovic";
      homepage = "";
      url = "";
      synopsis = "A Wadler/Leijen Pretty Printer for Text values";
      description = "A clone of wl-pprint for use with the text library.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/wl-pprint-text-1.2.0.2.tar.gz";
      sha256 = "9215778d58ab9f71a4b8b5fb75c06438ff6ea7319a31eb6e97b4f67520dfb12b";
    });
  }) // {
    package-description-override = "Cabal-version:       >=1.10\r\nName:                wl-pprint-text\r\nVersion:             1.2.0.2\r\nx-revision: 3\r\nSynopsis:            A Wadler/Leijen Pretty Printer for Text values\r\nDescription:         A clone of wl-pprint for use with the text library.\r\nLicense:             BSD3\r\nLicense-file:        LICENSE\r\nAuthor:              Ivan Lazar Miljenovic\r\nMaintainer:          Ivan.Miljenovic@gmail.com\r\nCopyright:           2007 Daan Leijen, 2010 Ivan Lazar Miljenovic\r\nCategory:            Text\r\nBuild-type:          Simple\r\n\r\nTested-With:\r\n    GHC == 7.4.2\r\n    GHC == 7.6.3\r\n    GHC == 7.8.4\r\n    GHC == 7.10.3\r\n    GHC == 8.0.2\r\n    GHC == 8.2.2\r\n    GHC == 8.4.4\r\n    GHC == 8.6.5\r\n    GHC == 8.8.4\r\n    GHC == 8.10.7\r\n    GHC == 9.0.1\r\n    GHC == 9.2.1\r\n\r\nExtra-Source-Files: Changelog.md\r\n                    README.md\r\n\r\nSource-Repository head\r\n    type: git\r\n    location: https://github.com/ivan-m/wl-pprint-text.git\r\n\r\nLibrary\r\n  Exposed-modules:     Text.PrettyPrint.Leijen.Text,\r\n                       Text.PrettyPrint.Leijen.Text.Monadic\r\n  Build-depends:       base        >= 4.5.0.0  && < 5,\r\n                       base-compat >= 0.10     && < 0.15,\r\n                       text        >= 0.11.0.0 && < 2.2\r\n  Default-language:    Haskell98\r\n  GHC-Options:         -Wall\r\n";
  }