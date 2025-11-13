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
      identifier = { name = "time-manager"; version = "0.2.3"; };
      license = "MIT";
      copyright = "";
      maintainer = "kazu@iij.ad.jp";
      author = "Michael Snoyman and Kazu Yamamoto";
      homepage = "http://github.com/yesodweb/wai";
      url = "";
      synopsis = "Scalable timer";
      description = "Scalable timer functions provided by a timer manager\nand thread management functions to prevent thread\nleak by a thread manager.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."auto-update" or (errorHandler.buildDepError "auto-update"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/time-manager-0.2.3.tar.gz";
      sha256 = "eeb7c33d3b4492af771db0061222239a29727ee68a786a0488b1dba0a63d68e8";
    });
  }) // {
    package-description-override = "Name:                time-manager\nVersion:             0.2.3\nSynopsis:            Scalable timer\nLicense:             MIT\nLicense-file:        LICENSE\nAuthor:              Michael Snoyman and Kazu Yamamoto\nMaintainer:          kazu@iij.ad.jp\nHomepage:            http://github.com/yesodweb/wai\nCategory:            System\nBuild-Type:          Simple\nCabal-Version:       >=1.10\nStability:           Stable\nDescription:         Scalable timer functions provided by a timer manager\n                     and thread management functions to prevent thread\n                     leak by a thread manager.\nExtra-Source-Files:  ChangeLog.md\n\nLibrary\n  Build-Depends:     base                      >= 4.12       && < 5\n                   , auto-update               >= 0.2        && < 0.3\n                   , containers\n                   , stm\n  Default-Language:  Haskell2010\n  Exposed-modules:   System.TimeManager\n  Exposed-modules:   System.ThreadManager\n  Ghc-Options:       -Wall\n";
  }