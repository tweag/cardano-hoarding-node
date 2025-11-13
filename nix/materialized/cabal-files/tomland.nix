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
    flags = { build-readme = false; build-play-tomland = false; };
    package = {
      specVersion = "2.4";
      identifier = { name = "tomland"; version = "1.3.3.3"; };
      license = "MPL-2.0";
      copyright = "2018-2023 Kowainik";
      maintainer = "Kowainik <xrom.xkov@gmail.com>";
      author = "Dmitrii Kovanikov, Veronika Romashkina";
      homepage = "https://github.com/kowainik/tomland";
      url = "";
      synopsis = "Bidirectional TOML serialization";
      description = "Implementation of bidirectional TOML serialization. Simple codecs look like this:\n\n@\n__data__ User = User\n\\    { userName :: Text\n\\    , userAge  :: Int\n\\    }\n\\\n\\userCodec :: TomlCodec User\n\\userCodec = User\n\\    \\<$\\> Toml.text \"name\" .= userName\n\\    \\<*\\> Toml.int  \"age\"  .= userAge\n@\n\nThe following blog post has more details about library design:\n\n* [tomland: Bidirectional TOML serialization](https://kowainik.github.io/posts/2019-01-14-tomland)";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."parser-combinators" or (errorHandler.buildDepError "parser-combinators"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."validation-selective" or (errorHandler.buildDepError "validation-selective"))
        ];
        buildable = true;
      };
      exes = {
        "readme" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."tomland" or (errorHandler.buildDepError "tomland"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ];
          build-tools = [
            (hsPkgs.pkgsBuildBuild.markdown-unlit.components.exes.markdown-unlit or (pkgs.pkgsBuildBuild.markdown-unlit or (errorHandler.buildToolDepError "markdown-unlit:markdown-unlit")))
          ];
          buildable = if !flags.build-readme || system.isWindows
            then false
            else true;
        };
        "play-tomland" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."tomland" or (errorHandler.buildDepError "tomland"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ];
          buildable = if !flags.build-play-tomland || compiler.isGhc && compiler.version.lt "8.6"
            then false
            else true;
        };
      };
      tests = {
        "tomland-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-hedgehog" or (errorHandler.buildDepError "hspec-hedgehog"))
            (hsPkgs."hspec-megaparsec" or (errorHandler.buildDepError "hspec-megaparsec"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."tomland" or (errorHandler.buildDepError "tomland"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/tomland-1.3.3.3.tar.gz";
      sha256 = "a0992bea52a636e2aa2597b0ab25c54f0fb389e0052fe5a6436fe53acef956ab";
    });
  }) // {
    package-description-override = "cabal-version:       2.4\r\nname:                tomland\r\nversion:             1.3.3.3\r\nx-revision: 3\r\nsynopsis:            Bidirectional TOML serialization\r\ndescription:\r\n    Implementation of bidirectional TOML serialization. Simple codecs look like this:\r\n    .\r\n    @\r\n    __data__ User = User\r\n    \\    { userName :: Text\r\n    \\    , userAge  :: Int\r\n    \\    }\r\n    \\\r\n    \\userCodec :: TomlCodec User\r\n    \\userCodec = User\r\n    \\    \\<$\\> Toml.text \"name\" .= userName\r\n    \\    \\<*\\> Toml.int  \"age\"  .= userAge\r\n    @\r\n    .\r\n    The following blog post has more details about library design:\r\n    .\r\n    * [tomland: Bidirectional TOML serialization](https://kowainik.github.io/posts/2019-01-14-tomland)\r\n\r\nhomepage:            https://github.com/kowainik/tomland\r\nbug-reports:         https://github.com/kowainik/tomland/issues\r\nlicense:             MPL-2.0\r\nlicense-file:        LICENSE\r\nauthor:              Dmitrii Kovanikov, Veronika Romashkina\r\nmaintainer:          Kowainik <xrom.xkov@gmail.com>\r\ncopyright:           2018-2023 Kowainik\r\ncategory:            TOML, Text, Configuration\r\nbuild-type:          Simple\r\nextra-doc-files:     README.md\r\n                     CHANGELOG.md\r\nextra-source-files:  test/golden/*.golden\r\n                     test/examples/*.toml\r\ntested-with:         GHC == 8.4.4\r\n                     GHC == 8.6.5\r\n                     GHC == 8.8.4\r\n                     GHC == 8.10.7\r\n                     GHC == 9.0.2\r\n                     GHC == 9.2.7\r\n                     GHC == 9.4.4\r\n                     GHC == 9.6.3\r\n                     GHC == 9.8.1\r\n\r\nsource-repository head\r\n  type:                git\r\n  location:            https://github.com/kowainik/tomland.git\r\n\r\nflag build-readme\r\n  description:         Build README generator.\r\n  default:             False\r\n  manual:              True\r\n\r\nflag build-play-tomland\r\n  description:         Build play-tomland executable.\r\n  default:             False\r\n  manual:              True\r\n\r\ncommon common-options\r\n  build-depends:       base >= 4.11 && < 4.22\r\n\r\n  ghc-options:         -Wall\r\n                       -Wcompat\r\n                       -Widentities\r\n                       -Wincomplete-uni-patterns\r\n                       -Wincomplete-record-updates\r\n                       -Wredundant-constraints\r\n                       -fhide-source-paths\r\n                       -freverse-errors\r\n  if impl(ghc >= 8.4)\r\n    ghc-options:       -Wmissing-export-lists\r\n                       -Wpartial-fields\r\n  if impl(ghc >= 8.8.1)\r\n    ghc-options:       -Wmissing-deriving-strategies\r\n                       -Werror=missing-deriving-strategies\r\n  if impl(ghc >= 8.10)\r\n    ghc-options:       -Wunused-packages\r\n  if impl(ghc >= 9.2)\r\n    ghc-options:\r\n                       -Wredundant-bang-patterns\r\n\r\n  default-language:    Haskell2010\r\n  default-extensions:  DeriveGeneric\r\n                       DerivingStrategies\r\n                       GeneralizedNewtypeDeriving\r\n                       InstanceSigs\r\n                       LambdaCase\r\n                       OverloadedStrings\r\n                       RecordWildCards\r\n                       ScopedTypeVariables\r\n                       TypeApplications\r\n\r\nlibrary\r\n  import:              common-options\r\n  hs-source-dirs:      src\r\n\r\n  exposed-modules:     Toml\r\n                         Toml.Codec\r\n                           Toml.Codec.BiMap\r\n                             Toml.Codec.BiMap.Conversion\r\n                           Toml.Codec.Code\r\n                           Toml.Codec.Combinator\r\n                             Toml.Codec.Combinator.Common\r\n                             Toml.Codec.Combinator.Custom\r\n                             Toml.Codec.Combinator.List\r\n                             Toml.Codec.Combinator.Map\r\n                             Toml.Codec.Combinator.Monoid\r\n                             Toml.Codec.Combinator.Primitive\r\n                             Toml.Codec.Combinator.Set\r\n                             Toml.Codec.Combinator.Table\r\n                             Toml.Codec.Combinator.Time\r\n                             Toml.Codec.Combinator.Tuple\r\n                           Toml.Codec.Di\r\n                           Toml.Codec.Error\r\n                           Toml.Codec.Generic\r\n                           Toml.Codec.Types\r\n                         Toml.Parser\r\n                           Toml.Parser.Core\r\n                           Toml.Parser.Item\r\n                           Toml.Parser.Key\r\n                           Toml.Parser.String\r\n                           Toml.Parser.Validate\r\n                           Toml.Parser.Value\r\n                         Toml.Type\r\n                           Toml.Type.AnyValue\r\n                           Toml.Type.Edsl\r\n                           Toml.Type.Key\r\n                           Toml.Type.PrefixTree\r\n                           Toml.Type.Printer\r\n                           Toml.Type.TOML\r\n                           Toml.Type.UValue\r\n                           Toml.Type.Value\r\n\r\n  build-depends:       bytestring >= 0.10 && < 0.13\r\n                     , containers >= 0.5.7 && < 0.9\r\n                     , deepseq >= 1.4 && < 1.6\r\n                     , hashable >= 1.3.1.0 && < 1.6\r\n                     , megaparsec >= 7.0.5 && < 9.8\r\n                     , mtl >= 2.2 && < 2.4\r\n                     , parser-combinators >= 1.1.0 && < 1.4\r\n                     , text >= 1.2 && < 2.2\r\n                     , time >= 1.8 && < 1.16\r\n                     , unordered-containers ^>= 0.2.7\r\n                     , validation-selective >= 0.1.0 && < 0.3\r\n\r\nexecutable readme\r\n  import:              common-options\r\n  -- doesn't work on windows for unknown reasons\r\n  if !flag(build-readme) || os(windows)\r\n    buildable: False\r\n  main-is:             README.lhs\r\n  build-depends:       tomland\r\n                     , text\r\n                     , time\r\n\r\n  build-tool-depends:  markdown-unlit:markdown-unlit\r\n  ghc-options:         -pgmL markdown-unlit\r\n\r\nexecutable play-tomland\r\n  import:              common-options\r\n  -- We are using DerivingVia that works only with > 8.6\r\n  if !flag(build-play-tomland) || impl(ghc < 8.6)\r\n    buildable: False\r\n  main-is:             Main.hs\r\n  build-depends:       tomland\r\n                     , bytestring\r\n                     , containers\r\n                     , hashable\r\n                     , text\r\n                     , time\r\n                     , unordered-containers\r\n\r\n  hs-source-dirs:      examples\r\n  ghc-options:         -threaded -Wall\r\n\r\ntest-suite tomland-test\r\n  import:              common-options\r\n  type:                exitcode-stdio-1.0\r\n  hs-source-dirs:      test\r\n  main-is:             Spec.hs\r\n\r\n  other-modules:       Test.Toml.Codec\r\n                         Test.Toml.Codec.BiMap\r\n                           Test.Toml.Codec.BiMap.Conversion\r\n                         Test.Toml.Codec.Code\r\n                         Test.Toml.Codec.Combinator\r\n                           Test.Toml.Codec.Combinator.Common\r\n                           Test.Toml.Codec.Combinator.Custom\r\n                           Test.Toml.Codec.Combinator.List\r\n                           Test.Toml.Codec.Combinator.Map\r\n                           Test.Toml.Codec.Combinator.Monoid\r\n                           Test.Toml.Codec.Combinator.Primitive\r\n                           Test.Toml.Codec.Combinator.Set\r\n                           Test.Toml.Codec.Combinator.Table\r\n                           Test.Toml.Codec.Combinator.Time\r\n                           Test.Toml.Codec.Combinator.Tuple\r\n                         Test.Toml.Codec.Di\r\n                         Test.Toml.Codec.Generic\r\n                         Test.Toml.Codec.SmallType\r\n\r\n                       Test.Toml.Parser\r\n                         Test.Toml.Parser.Examples\r\n                         Test.Toml.Parser.Property\r\n                         Test.Toml.Parser.Validate\r\n                         -- unit tests for parsing different parts of TOML ast\r\n                         Test.Toml.Parser.Array\r\n                         Test.Toml.Parser.Bool\r\n                         Test.Toml.Parser.Common\r\n                         Test.Toml.Parser.Date\r\n                         Test.Toml.Parser.Double\r\n                         Test.Toml.Parser.Integer\r\n                         Test.Toml.Parser.Key\r\n                         Test.Toml.Parser.Text\r\n                         Test.Toml.Parser.Toml\r\n\r\n                       Test.Toml.Type\r\n                         Test.Toml.Type.Key\r\n                         Test.Toml.Type.PrefixTree\r\n                         Test.Toml.Type.Printer\r\n                         Test.Toml.Type.TOML\r\n\r\n                       -- helpers\r\n                       Test.Toml.Gen\r\n                       Test.Toml.Property\r\n\r\n  build-depends:       bytestring\r\n                     , containers >= 0.5.7 && < 0.9\r\n                     , hashable\r\n                     , hedgehog >= 1.0.1 && < 1.6\r\n                     , hspec >= 2.7.1 && < 2.12\r\n                     , hspec-hedgehog >= 0.0.1.1 && < 0.4\r\n                     , hspec-megaparsec >= 2.0.0 && < 2.3.0\r\n                     , megaparsec\r\n                     , directory ^>= 1.3\r\n                     , text\r\n                     , time\r\n                     , tomland\r\n                     , unordered-containers\r\n\r\n  ghc-options:         -threaded -rtsopts -with-rtsopts=-N\r\n";
  }