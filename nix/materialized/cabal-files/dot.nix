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
    flags = { examples = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "dot"; version = "0.3"; };
      license = "BSD-3-Clause";
      copyright = "2019 Andrew Martin";
      maintainer = "Andrew Martin <andrew.thaddeus@gmail.com>\nchessai <chessai1996@gmail.com>";
      author = "Andrew Martin";
      homepage = "https://github.com/andrewthad/dot";
      url = "";
      synopsis = "Datatypes and encoding for graphviz dot files";
      description = "Datatypes and encoding for graphviz dot files.\nSee the example directory for example usage.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
        ];
        buildable = true;
      };
      exes = {
        "example" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."dot" or (errorHandler.buildDepError "dot"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
          buildable = if !flags.examples then false else true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/dot-0.3.tar.gz";
      sha256 = "b6144d948d86fe8f8df6c4ec12d4d127733dc6b194a6d204792a5fadb42e8483";
    });
  }) // {
    package-description-override = "cabal-version: 2.2\nname:\n  dot\nversion:\n  0.3\nsynopsis:\n  Datatypes and encoding for graphviz dot files\ndescription:\n  Datatypes and encoding for graphviz dot files.\n  See the example directory for example usage.\nhomepage:\n  https://github.com/andrewthad/dot\nlicense:\n  BSD-3-Clause\nauthor:\n  Andrew Martin\nmaintainer:\n  Andrew Martin <andrew.thaddeus@gmail.com>\n  chessai <chessai1996@gmail.com>\ncopyright:\n  2019 Andrew Martin\ncategory:\n  Data,Graphics,Graphs\nbuild-type:\n  Simple\n\nlibrary\n  hs-source-dirs:\n    src\n  exposed-modules:\n    Dot\n    Dot.Text\n    Dot.Types\n  build-depends:\n    , base >= 4.7 && < 5\n    , text\n  default-language:\n    Haskell2010\n\nexecutable example\n  hs-source-dirs:\n    example\n  main-is:\n    Example.hs\n  build-depends:\n    , base\n    , dot\n    , text\n  default-language:\n    Haskell2010\n  if !(flag(examples))\n    buildable: False\n\nflag examples\n  description:\n    Also compile examples\n  manual:\n    True\n  default:\n    False\n\nsource-repository head\n  type:\n    git\n  location:\n    https://github.com/andrewthad/dot\n";
  }