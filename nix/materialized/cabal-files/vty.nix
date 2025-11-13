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
      specVersion = "1.18";
      identifier = { name = "vty"; version = "6.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Jonathan Daugherty (cygnus@foobox.com)";
      author = "AUTHORS";
      homepage = "https://github.com/jtdaugherty/vty";
      url = "";
      synopsis = "A simple terminal UI library";
      description = "vty is terminal GUI library in the niche of ncurses. It is intended to\nbe easy to use and to provide good support for common terminal types.\n\nSee the @vty-examples@ package as well as the program\n@examples/interactive_terminal_test.hs@ included in the @vty@\nrepository for examples on how to use the library.\n\nImport the @Graphics.Vty@ convenience module to get access to the core\nparts of the library.\n\n&#169; 2006-2007 Stefan O'Rear; BSD3 license.\n\n&#169; Corey O'Connor; BSD3 license.\n\n&#169; Jonathan Daugherty; BSD3 license.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."blaze-builder" or (errorHandler.buildDepError "blaze-builder"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."microlens-mtl" or (errorHandler.buildDepError "microlens-mtl"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
        ] ++ pkgs.lib.optionals (!(compiler.isGhc && compiler.version.ge "8.0")) [
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/vty-6.2.tar.gz";
      sha256 = "6f1f48b260b4de0a0506e70faf4d41b8c1757ff429a95ba6b6b76cf76c73987b";
    });
  }) // {
    package-description-override = "name:                vty\r\nversion:             6.2\r\nx-revision: 1\r\nlicense:             BSD3\r\nlicense-file:        LICENSE\r\nauthor:              AUTHORS\r\nmaintainer:          Jonathan Daugherty (cygnus@foobox.com)\r\nhomepage:            https://github.com/jtdaugherty/vty\r\ncategory:            User Interfaces\r\nsynopsis:            A simple terminal UI library\r\ndescription:\r\n  vty is terminal GUI library in the niche of ncurses. It is intended to\r\n  be easy to use and to provide good support for common terminal types.\r\n  .\r\n  See the @vty-examples@ package as well as the program\r\n  @examples/interactive_terminal_test.hs@ included in the @vty@\r\n  repository for examples on how to use the library.\r\n  .\r\n  Import the @Graphics.Vty@ convenience module to get access to the core\r\n  parts of the library.\r\n  .\r\n  &#169; 2006-2007 Stefan O'Rear; BSD3 license.\r\n  .\r\n  &#169; Corey O'Connor; BSD3 license.\r\n  .\r\n  &#169; Jonathan Daugherty; BSD3 license.\r\ncabal-version:       1.18\r\nbuild-type:          Simple\r\nextra-doc-files:     README.md,\r\n                     AUTHORS,\r\n                     CHANGELOG.md,\r\n                     LICENSE\r\ntested-with:         GHC==8.0.2, GHC==8.2.2, GHC==8.4.3, GHC==8.6.5, GHC==8.8.4, GHC==8.10.7, GHC==9.0.2, GHC==9.2.8, GHC==9.4.5, GHC==9.6.2\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/jtdaugherty/vty.git\r\n\r\nlibrary\r\n  default-language:    Haskell2010\r\n  include-dirs:        cbits\r\n  hs-source-dirs:      src\r\n  ghc-options:         -O2 -funbox-strict-fields -Wall -fspec-constr -fspec-constr-count=10\r\n  ghc-prof-options:    -O2 -funbox-strict-fields -caf-all -Wall -fspec-constr -fspec-constr-count=10\r\n  build-depends:       base >= 4.8 && < 5,\r\n                       blaze-builder >= 0.3.3.2 && < 0.5,\r\n                       bytestring,\r\n                       deepseq >= 1.1 && < 1.6,\r\n                       microlens < 0.6,\r\n                       microlens-mtl,\r\n                       mtl >= 1.1.1.0 && < 2.4,\r\n                       stm,\r\n                       text >= 0.11.3,\r\n                       utf8-string >= 0.3 && < 1.1,\r\n                       vector >= 0.7,\r\n                       binary,\r\n                       parsec,\r\n                       filepath,\r\n                       directory\r\n\r\n  if !impl(ghc >= 8.0)\r\n    build-depends:     semigroups >= 0.16,\r\n                       fail\r\n\r\n  exposed-modules:     Graphics.Text.Width\r\n                       Graphics.Vty\r\n                       Graphics.Vty.Attributes\r\n                       Graphics.Vty.Attributes.Color\r\n                       Graphics.Vty.Attributes.Color240\r\n                       Graphics.Vty.Config\r\n                       Graphics.Vty.Debug\r\n                       Graphics.Vty.DisplayAttributes\r\n                       Graphics.Vty.Error\r\n                       Graphics.Vty.Image\r\n                       Graphics.Vty.Image.Internal\r\n                       Graphics.Vty.Inline\r\n                       Graphics.Vty.Input\r\n                       Graphics.Vty.Input.Events\r\n                       Graphics.Vty.Output\r\n                       Graphics.Vty.Output.Mock\r\n                       Graphics.Vty.Picture\r\n                       Graphics.Vty.PictureToSpans\r\n                       Graphics.Vty.Span\r\n                       Graphics.Vty.UnicodeWidthTable.IO\r\n                       Graphics.Vty.UnicodeWidthTable.Install\r\n                       Graphics.Vty.UnicodeWidthTable.Main\r\n                       Graphics.Vty.UnicodeWidthTable.Query\r\n                       Graphics.Vty.UnicodeWidthTable.Types\r\n  c-sources:           cbits/mk_wcwidth.c\r\n";
  }