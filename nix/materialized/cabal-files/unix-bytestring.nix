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
      specVersion = "2.2";
      identifier = { name = "unix-bytestring"; version = "0.4.0.3"; };
      license = "BSD-3-Clause";
      copyright = "2010–2025 wren romano";
      maintainer = "wren@cpan.org";
      author = "wren gayle romano";
      homepage = "https://wrengr.org/software/hackage.html";
      url = "";
      synopsis = "Unix/Posix-specific functions for ByteStrings.";
      description = "Unix\\/Posix-specific functions for ByteStrings.\n\nProvides @ByteString@ file-descriptor based I\\/O API, designed\nloosely after the @String@ file-descriptor based I\\/O API in\n\"System.Posix.IO\". The functions here wrap standard C implementations\nof the functions specified by the ISO\\/IEC 9945-1:1990 (``POSIX.1'')\nand X\\/Open Portability Guide Issue 4, Version 2 (``XPG4.2'')\nspecifications.\n\nNote that this package doesn't require the @unix@ package as a\ndependency. But you'll need it in order to get your hands on\nan @Fd@, so we're not offering a complete replacement.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/unix-bytestring-0.4.0.3.tar.gz";
      sha256 = "548ccdc28092796c16710cb1223d0eb590fad5542f986db870bac87460c3b678";
    });
  }) // {
    package-description-override = "Cabal-Version:  2.2\n-- Cabal >=2.2 is required for:\n--    <https://cabal.readthedocs.io/en/latest/cabal-package.html#common-stanzas>\n-- Since 2.1, the Cabal-Version must be the absolutely first thing\n-- in the file, even before comments.  Also, no longer uses \">=\".\n--    <https://github.com/haskell/cabal/issues/4899>\n\n----------------------------------------------------------------\n-- wren gayle romano <wren@cpan.org>                ~ 2025-02-11\n----------------------------------------------------------------\n\nName:           unix-bytestring\nVersion:        0.4.0.3\nBuild-Type:     Simple\nStability:      provisional\nHomepage:       https://wrengr.org/software/hackage.html\nBug-Reports:    https://github.com/wrengr/unix-bytestring/issues\nAuthor:         wren gayle romano\nMaintainer:     wren@cpan.org\nCopyright:      2010–2025 wren romano\n-- Cabal-2.2 requires us to say \"BSD-3-Clause\" not \"BSD3\"\nLicense:        BSD-3-Clause\nLicense-File:   LICENSE\n\nCategory:       System\nSynopsis:       Unix/Posix-specific functions for ByteStrings.\nDescription:    Unix\\/Posix-specific functions for ByteStrings.\n    .\n    Provides @ByteString@ file-descriptor based I\\/O API, designed\n    loosely after the @String@ file-descriptor based I\\/O API in\n    \"System.Posix.IO\". The functions here wrap standard C implementations\n    of the functions specified by the ISO\\/IEC 9945-1:1990 (``POSIX.1'')\n    and X\\/Open Portability Guide Issue 4, Version 2 (``XPG4.2'')\n    specifications.\n    .\n    Note that this package doesn't require the @unix@ package as a\n    dependency. But you'll need it in order to get your hands on\n    an @Fd@, so we're not offering a complete replacement.\n\nExtra-source-files:\n    README.md, CHANGELOG\n\n-- This used to work as far back as GHC 6.12.1 but we don't verify that with CI.\n-- <https://github.com/wrengr/unix-bytestring/actions?query=workflow%3Aci>\nTested-With:\n    GHC ==8.0.2,\n    GHC ==8.2.2,\n    GHC ==8.4.4,\n    GHC ==8.6.5,\n    GHC ==8.8.4,\n    GHC ==8.10.3,\n    GHC ==9.0.1,\n    GHC ==9.2.4,\n    GHC ==9.4.8,\n    GHC ==9.6.5,\n    GHC ==9.8.2,\n    GHC ==9.10.1,\n    GHC ==9.12.1\n\nSource-Repository head\n    Type:     git\n    Location: https://github.com/wrengr/unix-bytestring.git\n\n----------------------------------------------------------------\nLibrary\n    Default-Language: Haskell2010\n    Hs-Source-Dirs:  src\n    Exposed-Modules: Foreign.C.Error.Safe\n                   , System.Posix.IO.ByteString.Ext\n                   , System.Posix.IO.ByteString.Ext.Lazy\n                   , System.Posix.Types.Iovec\n\n    -- We require base>=4.1 for Foreign.C.Error.throwErrnoIfMinus1Retry.\n    --\n    -- We would require unix>=2.4 for System.Posix.IO.fdReadBuf/fdWriteBuf\n    -- (and unix-2.4.0.0 requires base>=4.1 too), except we define\n    -- them on our own for better backwards compatibility.\n    --\n    -- FIXME: unix-2.8.0.0 introduces some additional conflicts\n    Build-Depends: base       >= 4.9    && < 4.22\n                 , bytestring >= 0.10.8 && < 0.13\n\n----------------------------------------------------------------\n----------------------------------------------------------- fin.\n";
  }