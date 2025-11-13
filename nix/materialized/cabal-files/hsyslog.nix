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
    flags = { install-examples = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "hsyslog"; version = "5.0.2"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (c) 2004-2017 by Peter Simons";
      maintainer = "Peter Simons <simons@cryp.to>";
      author = "Peter Simons, John Lato, Jonathan Childress";
      homepage = "https://github.com/peti/hsyslog";
      url = "";
      synopsis = "FFI interface to syslog(3) from POSIX.1-2001";
      description = "A Haskell interface to @syslog(3)@ as specified in\n<http://pubs.opengroup.org/onlinepubs/9699919799/functions/syslog.html POSIX.1-2008>.\nThe entire public API lives in \"System.Posix.Syslog\". There is a set of exposed\nmodules available underneath that one, which contain various implementation details\nthat may be useful to other developers who want to implement syslog-related\nfunctionality. /Users/ of @syslog@, however, do not need them.\n\nAn example program that demonstrates how to use this library is available in the\n<https://github.com/peti/hsyslog/blob/master/example/Main.hs examples> directory of\nthis package.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        build-tools = [
          (hsPkgs.pkgsBuildBuild.hsc2hs.components.exes.hsc2hs or (pkgs.pkgsBuildBuild.hsc2hs or (errorHandler.buildToolDepError "hsc2hs:hsc2hs")))
        ];
        buildable = true;
      };
      exes = {
        "hsyslog-example" = {
          depends = pkgs.lib.optionals (flags.install-examples) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hsyslog" or (errorHandler.buildDepError "hsyslog"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ];
          buildable = if flags.install-examples then true else false;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hsyslog-5.0.2.tar.gz";
      sha256 = "3eec43c8fb42c23d03f1db7b0b594d39cd94275c2284dcd0c64aa4d680bd7ece";
    });
  }) // {
    package-description-override = "name:           hsyslog\r\nversion:        5.0.2\r\nx-revision: 1\r\ncabal-version:  >= 1.10\r\nbuild-type:     Simple\r\nlicense:        BSD3\r\nlicense-file:   LICENSE\r\ncopyright:      Copyright (c) 2004-2017 by Peter Simons\r\nauthor:         Peter Simons, John Lato, Jonathan Childress\r\nmaintainer:     Peter Simons <simons@cryp.to>\r\nhomepage:       https://github.com/peti/hsyslog\r\nbug-reports:    https://github.com/peti/hsyslog/issues\r\nsynopsis:       FFI interface to syslog(3) from POSIX.1-2001\r\ncategory:       Foreign\r\ntested-with:    GHC == 7.6.3, GHC == 7.8.4, GHC == 7.10.3, GHC == 8.0.2\r\n              , GHC == 8.2.2, GHC == 8.4.4, GHC == 8.6.3\r\n\r\nextra-source-files:\r\n  c-bits/simple-syslog.c\r\n  c-bits/make-log-mask.c\r\n\r\ndescription:\r\n  A Haskell interface to @syslog(3)@ as specified in\r\n  <http://pubs.opengroup.org/onlinepubs/9699919799/functions/syslog.html POSIX.1-2008>.\r\n  The entire public API lives in \"System.Posix.Syslog\". There is a set of exposed\r\n  modules available underneath that one, which contain various implementation details\r\n  that may be useful to other developers who want to implement syslog-related\r\n  functionality. /Users/ of @syslog@, however, do not need them.\r\n  .\r\n  An example program that demonstrates how to use this library is available in the\r\n  <https://github.com/peti/hsyslog/blob/master/example/Main.hs examples> directory of\r\n  this package.\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: git://github.com/peti/hsyslog.git\r\n\r\nFlag install-examples\r\n  Description:   Build and install example programs.\r\n  Default:       False\r\n\r\nlibrary\r\n  exposed-modules:  System.Posix.Syslog\r\n                    System.Posix.Syslog.Facility\r\n                    System.Posix.Syslog.Functions\r\n                    System.Posix.Syslog.LogMask\r\n                    System.Posix.Syslog.Options\r\n                    System.Posix.Syslog.Priority\r\n  build-depends:    base >= 4.6 && < 5\r\n  other-extensions: ForeignFunctionInterface, DeriveGeneric\r\n  hs-source-dirs:   src\r\n  c-sources:        c-bits/simple-syslog.c\r\n                    c-bits/make-log-mask.c\r\n  default-language: Haskell2010\r\n  build-tools:      hsc2hs\r\n\r\nexecutable hsyslog-example\r\n  main-is:            Main.hs\r\n  hs-source-dirs:     example\r\n  if flag(install-examples)\r\n    buildable:        True\r\n    build-depends:    base, hsyslog, bytestring\r\n    other-extensions: TypeSynonymInstances, FlexibleInstances\r\n  else\r\n    buildable:        False\r\n  default-language:   Haskell2010\r\n";
  }