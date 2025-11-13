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
      identifier = { name = "quickcheck-transformer"; version = "0.3.1.2"; };
      license = "MIT";
      copyright = "(c) 2019, Henning Thielemann\n(c) 2013, Nikita Volkov";
      maintainer = "Henning Thielemann <haskell@henning-thielemann.de>";
      author = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      homepage = "https://hub.darcs.net/thielema/quickcheck-transformer/";
      url = "";
      synopsis = "A GenT monad transformer for QuickCheck library.";
      description = "A fork of QuickCheck-GenT that works for older GHC versions\nand uses the Test module folder like QuickCheck.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/quickcheck-transformer-0.3.1.2.tar.gz";
      sha256 = "309f57093f7fb59167d546e464ce5b63024d91d029acc966a3e351875898c61f";
    });
  }) // {
    package-description-override = "Cabal-Version: 2.2\nName:          quickcheck-transformer\nVersion:       0.3.1.2\nx-revision: 1\nSynopsis:      A GenT monad transformer for QuickCheck library.\nDescription:\n  A fork of QuickCheck-GenT that works for older GHC versions\n  and uses the Test module folder like QuickCheck.\nLicense:       MIT\nLicense-File:  LICENSE\nHomepage:      https://hub.darcs.net/thielema/quickcheck-transformer/\nAuthor:        Nikita Volkov <nikita.y.volkov@mail.ru>\nMaintainer:    Henning Thielemann <haskell@henning-thielemann.de>\nCopyright:\n  (c) 2019, Henning Thielemann\n  (c) 2013, Nikita Volkov\nCategory:      Testing\nBuild-Type:    Simple\n\n\nSource-Repository this\n  Tag:         0.3.1.2\n  Type:        darcs\n  Location:    https://hub.darcs.net/thielema/quickcheck-transformer/\n\nSource-Repository head\n  Type:        darcs\n  Location:    https://hub.darcs.net/thielema/quickcheck-transformer/\n\n\nLibrary\n  Hs-Source-Dirs: src\n  If impl(ghc>=8.8)\n    Hs-Source-Dirs: src-fail/from-4.13/\n  Else\n    Hs-Source-Dirs: src-fail/before-4.13/\n  Exposed-Modules:\n    Test.QuickCheck.GenT\n  Other-Modules:\n    Test.QuickCheck.GenT.Private\n  Build-Depends:\n    QuickCheck >=2.7 && <3,\n    random >=1.0 && <1.4,\n    transformers >=0.5 && <0.7,\n    base >=4.3 && <5\n  Default-Language: Haskell2010\n  GHC-Options: -Wall\n";
  }