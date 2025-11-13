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
    flags = { example = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "ansi-wl-pprint"; version = "1.0.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Edward Kmett <ekmett@gmail.com>";
      author = "Daan Leijen, Max Bolingbroke";
      homepage = "http://github.com/ekmett/ansi-wl-pprint";
      url = "";
      synopsis = "The Wadler/Leijen Pretty Printer for colored ANSI terminal output";
      description = "This is a pretty printing library based on Wadler's paper [\"A Prettier Printer\"](https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf).\nIt has been enhanced with support for ANSI terminal colored output using the [ansi-terminal](https://hackage.haskell.org/package/ansi-terminal) package.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."prettyprinter-compat-ansi-wl-pprint" or (errorHandler.buildDepError "prettyprinter-compat-ansi-wl-pprint"))
        ];
        buildable = true;
      };
      exes = {
        "ansi-wl-pprint-example" = {
          depends = pkgs.lib.optionals (flags.example) [
            (hsPkgs."ansi-wl-pprint" or (errorHandler.buildDepError "ansi-wl-pprint"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."prettyprinter-ansi-terminal" or (errorHandler.buildDepError "prettyprinter-ansi-terminal"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
          buildable = if flags.example then true else false;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/ansi-wl-pprint-1.0.2.tar.gz";
      sha256 = "234e1813a178e5466d121635e190e6ff33ea6f09c45120138824d5de76af2747";
    });
  }) // {
    package-description-override = "cabal-version:       >= 1.10\nname:                ansi-wl-pprint\nversion:             1.0.2\n\ncategory:            User Interfaces, Text\nsynopsis:            The Wadler/Leijen Pretty Printer for colored ANSI terminal output\ndescription:         {\n\nThis is a pretty printing library based on Wadler's paper [\"A Prettier Printer\"](https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf).\nIt has been enhanced with support for ANSI terminal colored output using the [ansi-terminal](https://hackage.haskell.org/package/ansi-terminal) package.\n\n}\nlicense:             BSD3\nlicense-file:        LICENSE\nextra-source-files:  README.md Changelog.md\nauthor:              Daan Leijen, Max Bolingbroke\nmaintainer:          Edward Kmett <ekmett@gmail.com>\nbug-reports:         http://github.com/ekmett/ansi-wl-pprint/issues\nhomepage:            http://github.com/ekmett/ansi-wl-pprint\nbuild-type:          Simple\ntested-with:         GHC==7.4.2, GHC==7.6.3, GHC==7.8.4, GHC==7.10.3, GHC==8.0.2, GHC==8.2.2, GHC==8.4.3, GHC==8.6.5, GHC==8.8.4, GHC==8.10.7, GHC==9.0.2, GHC==9.2.7, GHC==9.4.5, GHC==9.6.1\n\nsource-repository head\n  type: git\n  location: https://github.com/ekmett/ansi-wl-pprint.git\n\nflag Example\n  description:    Build the example application\n  default:        False\n  manual:         True\n\nlibrary\n  default-language: Haskell2010\n  hs-source-dirs: .\n  exposed-modules: Text.PrettyPrint.ANSI.Leijen\n  other-extensions: PackageImports\n\n  build-depends: base >=4.5 && <5\n               , prettyprinter-compat-ansi-wl-pprint >=1.0.2 && <1.0.3\n\nexecutable ansi-wl-pprint-example\n  default-language: Haskell2010\n  hs-source-dirs: src-exe\n  main-is: Example.hs\n\n  if flag(example)\n    build-depends: ansi-wl-pprint\n    -- dependencies whose constraints are inherited via lib:ansi-wl-pprint\n    build-depends: base, ansi-terminal, prettyprinter, prettyprinter-ansi-terminal, text\n  else\n    buildable: False\n";
  }