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
    flags = { no-double-conversion = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "formatting"; version = "7.2.0"; };
      license = "BSD-3-Clause";
      copyright = "2020 Alex Chapman, 2013 Chris Done, Shachaf Ben-Kiki, Martijn van Steenbergen, Mike Meyer, 2011 MailRank, Inc.";
      maintainer = "alex@farfromthere.net";
      author = "Chris Done, Shachaf Ben-Kiki, Martijn van Steenbergen, Mike Meyer, Bryan O'Sullivan, Alex Chapman";
      homepage = "https://github.com/AJChapman/formatting#readme";
      url = "";
      synopsis = "Combinator-based type-safe formatting (like printf() or FORMAT)";
      description = "Combinator-based type-safe formatting (like printf() or FORMAT), modelled from the HoleyMonoids package.\n\nSee the README at <https://github.com/AJChapman/formatting#readme> for more info.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
          (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
        ] ++ pkgs.lib.optional (!(compiler.isGhcjs && true) && !flags.no-double-conversion) (hsPkgs."double-conversion" or (errorHandler.buildDepError "double-conversion"));
        buildable = true;
      };
      tests = {
        "formatting-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ];
          buildable = true;
        };
      };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/formatting-7.2.0.tar.gz";
      sha256 = "c07b18177af614e7e5f32e6fd924f7b35c1b1c219b3491608ee8e7276e706a6d";
    });
  }) // {
    package-description-override = "cabal-version:       2.2\nname:                formatting\nversion:             7.2.0\nsynopsis:            Combinator-based type-safe formatting (like printf() or FORMAT)\ndescription:         Combinator-based type-safe formatting (like printf() or FORMAT), modelled from the HoleyMonoids package.\n                     .\n                     See the README at <https://github.com/AJChapman/formatting#readme> for more info.\nhomepage:            https://github.com/AJChapman/formatting#readme\nbug-reports:         https://github.com/AJChapman/formatting/issues\nlicense:             BSD-3-Clause\nlicense-file:        LICENSE\nauthor:              Chris Done, Shachaf Ben-Kiki, Martijn van Steenbergen, Mike Meyer, Bryan O'Sullivan, Alex Chapman\nmaintainer:          alex@farfromthere.net\ncopyright:           2020 Alex Chapman, 2013 Chris Done, Shachaf Ben-Kiki, Martijn van Steenbergen, Mike Meyer, 2011 MailRank, Inc.\ncategory:            Text\nbuild-type:          Simple\nextra-source-files:  CHANGELOG.md\n                     README.md\ntested-with:           GHC == 8.4.4\n                     , GHC == 8.6.5\n                     , GHC == 8.8.4\n                     , GHC == 8.10.7\n                     , GHC == 9.0.2\n                     , GHC == 9.2.2\n\ncommon deps\n  build-depends:\n    base >= 4.11 && < 5,\n    text >= 0.11.0.8\n\n-- Warnings list list taken from\n-- https://medium.com/mercury-bank/enable-all-the-warnings-a0517bc081c3\n-- Enable all warnings with -Weverything, then disable the ones we\n-- don’t care about\n  default-language:    Haskell2010\n  ghc-options:         -Weverything\n                       -Wno-all-missed-specialisations\n                       -Wno-implicit-prelude\n                       -Wno-missed-specialisations\n                       -Wno-missing-exported-signatures\n                       -Wno-missing-import-lists\n                       -Wno-missing-local-signatures\n                       -Wno-monomorphism-restriction\n                       -Wno-missing-deriving-strategies\n                       -Wno-safe\n                       -Wno-unsafe\n                       -fprint-potential-instances\n  if impl(ghc >= 8.10)\n    ghc-options:       -Wno-prepositive-qualified-module\n                       -Wno-missing-safe-haskell-mode\n\nflag no-double-conversion\n  description: Avoid 'double-conversion' dependency, which is large and uses C code\n  manual: False\n  default: False\n\nlibrary\n  import: deps\n  hs-source-dirs:    src\n  build-depends:\n    clock >= 0.4,\n    old-locale,\n    scientific >= 0.3.0.0,\n    time >= 1.5,\n    transformers,\n  if !impl(ghcjs) && !flag(no-double-conversion)\n    build-depends:\n      double-conversion ^>= 2.0.2.0,\n  exposed-modules:\n    Formatting\n    Formatting.Formatters\n    Formatting.ShortFormatters\n    Formatting.Combinators\n    Formatting.Examples\n    Formatting.Time\n    Formatting.Clock\n    Formatting.Internal\n    Formatting.Internal.Raw\n    Formatting.Buildable\n    Formatting.FromBuilder\n  other-modules:\n    Data.Text.Format.Functions\n    Data.Text.Format.Types\n    Data.Text.Format\n\ntest-suite formatting-test\n  import:              deps\n  build-depends:       formatting, hspec, scientific, time\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  main-is:             Spec.hs\n  ghc-options:         -threaded -rtsopts -with-rtsopts=-N\n\nbenchmark bench\n  import:              deps\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      bench\n  main-is:             bench.hs\n  build-depends:       formatting, criterion, QuickCheck\n  ghc-options:         -O2\n\nsource-repository head\n  type:     git\n  location: http://github.com/AJChapman/formatting\n";
  }