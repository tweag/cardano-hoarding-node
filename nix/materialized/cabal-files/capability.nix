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
    flags = { hspec-jenkins = false; dev = false; };
    package = {
      specVersion = "1.18";
      identifier = { name = "capability"; version = "0.5.0.1"; };
      license = "BSD-3-Clause";
      copyright = "2018 EURL Tweag";
      maintainer = "andreas.herrmann@tweag.io";
      author = "";
      homepage = "https://github.com/tweag/capability";
      url = "";
      synopsis = "Extensional capabilities and deriving combinators";
      description = "Standard capability type classes for extensional effects and combinators\nto derive capability instances with little boilerplate.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."constraints" or (errorHandler.buildDepError "constraints"))
          (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."mutable-containers" or (errorHandler.buildDepError "mutable-containers"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."reflection" or (errorHandler.buildDepError "reflection"))
          (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
          (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
        ];
        buildable = true;
      };
      tests = {
        "examples" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."capability" or (errorHandler.buildDepError "capability"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."silently" or (errorHandler.buildDepError "silently"))
            (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          ] ++ pkgs.lib.optional (flags.hspec-jenkins) (hsPkgs."hspec-jenkins" or (errorHandler.buildDepError "hspec-jenkins"));
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/capability-0.5.0.1.tar.gz";
      sha256 = "91e10f3f425b0dfd261ad8908e5373616acaae313ec1a0ce56052bee05697a6a";
    });
  }) // {
    package-description-override = "name: capability\r\nversion: 0.5.0.1\r\nx-revision: 4\r\nhomepage: https://github.com/tweag/capability\r\nlicense: BSD3\r\nlicense-file: LICENSE.md\r\nmaintainer: andreas.herrmann@tweag.io\r\ncopyright: 2018 EURL Tweag\r\ncategory: Control\r\nbuild-type: Simple\r\nextra-source-files:\r\n  ChangeLog.md\r\n  CONTRIBUTING.md\r\n  README.md\r\ncabal-version: 1.18\r\ntested-with: GHC==8.10.4, GHC==9.2.2\r\nsynopsis: Extensional capabilities and deriving combinators\r\ndescription:\r\n  Standard capability type classes for extensional effects and combinators\r\n  to derive capability instances with little boilerplate.\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/tweag/capability\r\n\r\nflag hspec-jenkins\r\n  description:\r\n    You can enable the use of the `hspec-jenkins` package using `-fhspec-jenkins`.\r\n    .\r\n    This package allows JUnit formatted test reporting for CI.\r\n  default: False\r\n\r\nflag dev\r\n  description: Turn on development settings.\r\n  manual: True\r\n  default: False\r\n\r\nlibrary\r\n  exposed-modules:\r\n    Capability\r\n    Capability.Accessors\r\n    Capability.Constraints\r\n    Capability.Derive\r\n    Capability.Error\r\n    Capability.Reader\r\n    Capability.Reader.Internal.Class\r\n    Capability.Reader.Internal.Strategies\r\n    Capability.Reflection\r\n    Capability.Sink\r\n    Capability.Sink.Internal.Class\r\n    Capability.Sink.Internal.Strategies\r\n    Capability.Source\r\n    Capability.Source.Internal.Class\r\n    Capability.Source.Internal.Strategies\r\n    Capability.State\r\n    Capability.State.Internal.Class\r\n    Capability.State.Internal.Strategies\r\n    Capability.State.Internal.Strategies.Common\r\n    Capability.Stream\r\n    Capability.TypeOf\r\n    Capability.Writer\r\n  build-depends:\r\n      base >= 4.14 && < 5.0\r\n    , constraints >= 0.1 && < 0.15\r\n    , dlist >= 0.8 && < 1.1\r\n    , exceptions >= 0.6 && < 0.11\r\n    , generic-lens >= 2.0 && < 2.3\r\n    , lens >= 4.16 && < 5.4\r\n    , monad-control >= 1.0 && < 1.1\r\n    , mtl >= 2.0 && < 3.0\r\n    , mutable-containers >= 0.3 && < 0.4\r\n    , primitive >= 0.6 && < 0.10\r\n    , reflection >= 2.1 && < 2.2\r\n    , safe-exceptions >= 0.1 && < 0.2\r\n    , streaming >= 0.2 && < 0.3\r\n    , transformers >= 0.5.5 && < 0.7\r\n    , unliftio >= 0.2 && < 0.3\r\n    , unliftio-core >= 0.1 && < 0.3\r\n  if flag(dev)\r\n    ghc-options: -Wall -Werror -Wcompat\r\n                 -Wincomplete-record-updates\r\n                 -Wincomplete-uni-patterns\r\n                 -Wnoncanonical-monad-instances\r\n  else\r\n    ghc-options: -Wall\r\n  hs-source-dirs: src\r\n  default-language: Haskell2010\r\n\r\ntest-suite examples\r\n  type: exitcode-stdio-1.0\r\n  other-modules:\r\n    Reflection\r\n    WordCount\r\n    CountLog\r\n    Error\r\n    Reader\r\n    Sink\r\n    State\r\n    Test.Common\r\n    Writer\r\n  main-is: Test.hs\r\n  build-depends:\r\n      base >= 4.14 && < 5.0\r\n    , capability\r\n    , containers\r\n    , dlist\r\n    , hspec\r\n    , lens\r\n    , mtl\r\n    , silently\r\n    , streaming\r\n    , temporary\r\n    , text\r\n    , unliftio\r\n  if flag(hspec-jenkins)\r\n    build-depends: hspec-jenkins\r\n  if flag(dev)\r\n    ghc-options: -Wall -Werror -Wcompat\r\n                 -Wincomplete-record-updates\r\n                 -Wincomplete-uni-patterns\r\n                 -Wnoncanonical-monad-instances\r\n  else\r\n    ghc-options: -Wall\r\n  hs-source-dirs: examples\r\n  default-language: Haskell2010\r\n";
  }