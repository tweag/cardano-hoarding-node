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
      identifier = { name = "hdaemonize"; version = "0.5.7"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Jeremy Bornstein <jeremy@jeremy.org>";
      author = "Anton Tayanovskyy, Fred Ross, Lana Black";
      homepage = "http://github.com/unprolix/hdaemonize";
      url = "";
      synopsis = "Library to handle the details of writing daemons for UNIX";
      description = "Provides functions that help writing better UNIX daemons,\ndaemonize and serviced/serviced': daemonize does what\na daemon should do (forking and closing descriptors),\nwhile serviced does that and more (syslog interface,\nPID file writing, start-stop-restart command line\nhandling, dropping privileges).";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
          (hsPkgs."hsyslog" or (errorHandler.buildDepError "hsyslog"))
          (hsPkgs."extensible-exceptions" or (errorHandler.buildDepError "extensible-exceptions"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hdaemonize-0.5.7.tar.gz";
      sha256 = "748823288eec23252335c00576384fc322e4f91c8e86f456530ba5d7c727f01b";
    });
  }) // {
    package-description-override = "Name:\t\thdaemonize\nVersion:\t0.5.7\nCabal-Version:  >= 1.10\nLicense:\tBSD3\nLicense-file:   LICENSE\nAuthor:         Anton Tayanovskyy, Fred Ross, Lana Black\nMaintainer:     Jeremy Bornstein <jeremy@jeremy.org>\nHomepage:       http://github.com/unprolix/hdaemonize\nCategory:\tSystem\nSynopsis:       Library to handle the details of writing daemons for UNIX\nDescription:\tProvides functions that help writing better UNIX daemons,\n                daemonize and serviced/serviced': daemonize does what\n                a daemon should do (forking and closing descriptors),\n                while serviced does that and more (syslog interface,\n                PID file writing, start-stop-restart command line\n                handling, dropping privileges).\nBuild-Type:\tSimple\nExtra-Source-Files:\tREADME.md\n\nLibrary\n  Build-Depends:    base >= 4 && < 5\n                  , bytestring\n                  , unix\n                  , hsyslog == 5.*\n                  , extensible-exceptions\n                  , filepath\n                  , mtl\n  Default-Language: Haskell2010\n  Exposed-modules:      System.Posix.Daemonize\n  if impl(ghc > 6.12)\n      Ghc-Options:      -Wall -fno-warn-unused-do-bind -fno-warn-type-defaults -fno-warn-name-shadowing\n  else\n      Ghc-Options:      -Wall -fno-warn-type-defaults -fno-warn-name-shadowing\n\nsource-repository head\n  type:     git\n  location: https://github.com/unprolix/hdaemonize.git\n";
  }