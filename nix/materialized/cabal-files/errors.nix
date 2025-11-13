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
      identifier = { name = "errors"; version = "2.3.0"; };
      license = "BSD-3-Clause";
      copyright = "2012, 2013 Gabriella Gonzalez";
      maintainer = "GenuineGabriella@gmail.com";
      author = "Gabriella Gonzalez";
      homepage = "";
      url = "";
      synopsis = "Simplified error-handling";
      description = "The one-stop shop for all your error-handling needs!  Just import\n\"Control.Error\".\n\nThis library encourages an error-handling style that directly uses the type\nsystem, rather than out-of-band exceptions.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
        ] ++ [ (hsPkgs."safe" or (errorHandler.buildDepError "safe")) ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/errors-2.3.0.tar.gz";
      sha256 = "6772e5689f07e82077ffe3339bc672934d83d83a97a7d4f1349de1302cb71f75";
    });
  }) // {
    package-description-override = "Name: errors\nVersion: 2.3.0\nx-revision: 5\nCabal-Version: >=1.8.0.2\nBuild-Type: Simple\nTested-With: GHC == 7.8.4, GHC == 7.10.2, GHC == 8.0.1\nLicense: BSD3\nLicense-File: LICENSE\nCopyright: 2012, 2013 Gabriella Gonzalez\nAuthor: Gabriella Gonzalez\nMaintainer: GenuineGabriella@gmail.com\nBug-Reports: https://github.com/Gabriella439/Haskell-Errors-Library/issues\nSynopsis: Simplified error-handling\nDescription:\n    The one-stop shop for all your error-handling needs!  Just import\n    \"Control.Error\".\n    .\n    This library encourages an error-handling style that directly uses the type\n    system, rather than out-of-band exceptions.\nCategory: Control, Error Handling\nextra-source-files: CHANGELOG.md\nSource-Repository head\n    Type: git\n    Location: https://github.com/Gabriella439/Haskell-Errors-Library\n\nLibrary\n    Build-Depends:\n        base                >= 4.7   && < 5   ,\n        exceptions          >= 0.6   && < 0.11,\n        text                            < 2.2 ,\n        transformers        >= 0.2   && < 0.7 ,\n        transformers-compat >= 0.4   && < 0.8\n    if impl(ghc <= 7.6.3)\n        Build-Depends:\n            safe            >= 0.3.3 && < 0.3.10\n    else\n        Build-Depends:\n            safe            >= 0.3.3 && < 0.4\n    Exposed-Modules:\n        Control.Error,\n        Control.Error.Safe,\n        Control.Error.Script,\n        Control.Error.Util,\n        Data.EitherR\n";
  }