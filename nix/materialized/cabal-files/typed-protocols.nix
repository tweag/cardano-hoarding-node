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
      specVersion = "3.4";
      identifier = { name = "typed-protocols"; version = "1.0.0.0"; };
      license = "Apache-2.0";
      copyright = "2019-2025 Input Output Global Inc (IOG)";
      maintainer = "alex@well-typed.com, duncan@well-typed.com, marcin.szamotulski@iohk.io";
      author = "Alexander Vieth, Duncan Coutts, Marcin Szamotulski";
      homepage = "";
      url = "";
      synopsis = "A framework for strongly typed protocols";
      description = "A robust session type framework which supports protocol pipelining.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."singletons" or (errorHandler.buildDepError "singletons"))
        ];
        buildable = true;
      };
      sublibs = {
        "cborg" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."singletons" or (errorHandler.buildDepError "singletons"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          ];
          buildable = true;
        };
        "stateful" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."singletons" or (errorHandler.buildDepError "singletons"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          ];
          buildable = true;
        };
        "stateful-cborg" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."singletons" or (errorHandler.buildDepError "singletons"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."typed-protocols".components.sublibs.cborg or (errorHandler.buildDepError "typed-protocols:cborg"))
            (hsPkgs."typed-protocols".components.sublibs.stateful or (errorHandler.buildDepError "typed-protocols:stateful"))
          ];
          buildable = true;
        };
        "examples" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."singletons" or (errorHandler.buildDepError "singletons"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-classes".components.sublibs.si-timers or (errorHandler.buildDepError "io-classes:si-timers"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."typed-protocols".components.sublibs.cborg or (errorHandler.buildDepError "typed-protocols:cborg"))
            (hsPkgs."typed-protocols".components.sublibs.stateful or (errorHandler.buildDepError "typed-protocols:stateful"))
          ];
          buildable = true;
        };
      };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."typed-protocols".components.sublibs.cborg or (errorHandler.buildDepError "typed-protocols:cborg"))
            (hsPkgs."typed-protocols".components.sublibs.examples or (errorHandler.buildDepError "typed-protocols:examples"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-classes".components.sublibs.si-timers or (errorHandler.buildDepError "io-classes:si-timers"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          ] ++ pkgs.lib.optionals (!system.isWindows) [
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/typed-protocols-1.0.0.0.tar.gz";
      sha256 = "c210846cf1d293066267993fba4d91f7843a0d4328050d9cbdce6a27f8002581";
    });
  }) // {
    package-description-override = "cabal-version:       3.4\nname:                typed-protocols\nversion:             1.0.0.0\nsynopsis:            A framework for strongly typed protocols\ndescription:         A robust session type framework which supports protocol pipelining.\nlicense:             Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\ncopyright:           2019-2025 Input Output Global Inc (IOG)\nauthor:              Alexander Vieth, Duncan Coutts, Marcin Szamotulski\nmaintainer:          alex@well-typed.com, duncan@well-typed.com, marcin.szamotulski@iohk.io\ncategory:            Control\nbuild-type:          Simple\ntested-with:         GHC == {9.6, 9.8, 9.10, 9.12}\nextra-doc-files:     CHANGELOG.md\n                     README.md\n\nlibrary\n  exposed-modules:   Network.TypedProtocol\n                   , Network.TypedProtocol.Core\n                   , Network.TypedProtocol.Peer\n                   , Network.TypedProtocol.Peer.Client\n                   , Network.TypedProtocol.Peer.Server\n                   , Network.TypedProtocol.Codec\n                   , Network.TypedProtocol.Driver\n                   , Network.TypedProtocol.Proofs\n  other-modules:     Network.TypedProtocol.Lemmas\n  build-depends:     base >=4.12 && <4.22,\n                     io-classes:io-classes ^>= 1.8,\n                     singletons ^>= 3.0\n\n  hs-source-dirs:    src\n  default-language:  GHC2021\n  default-extensions: DataKinds\n                      GADTs\n                      LambdaCase\n  ghc-options:       -Wall\n                     -Wno-unticked-promoted-constructors\n                     -Wcompat\n                     -Wincomplete-uni-patterns\n                     -Wincomplete-record-updates\n                     -Wpartial-fields\n                     -Widentities\n                     -Wredundant-constraints\n\nlibrary cborg\n  visibility:        public\n  exposed-modules:   Network.TypedProtocol.Codec.CBOR\n\n  build-depends:     base,\n                     bytestring      >=0.10  && <0.13,\n                     cborg           >=0.2.1 && <0.3,\n                     singletons,       \n                     primitive,\n\n                     io-classes:io-classes,\n                     typed-protocols:typed-protocols\n\n  hs-source-dirs:    cborg\n  default-language:  GHC2021\n  default-extensions: LambdaCase\n  ghc-options:       -Wall\n                     -Wno-unticked-promoted-constructors\n                     -Wcompat\n                     -Wincomplete-uni-patterns\n                     -Wincomplete-record-updates\n                     -Wpartial-fields\n                     -Widentities\n                     -Wredundant-constraints\n\nlibrary stateful\n  visibility:         public\n  exposed-modules:    Network.TypedProtocol.Stateful.Peer\n                    , Network.TypedProtocol.Stateful.Peer.Client\n                    , Network.TypedProtocol.Stateful.Peer.Server\n                    , Network.TypedProtocol.Stateful.Driver\n                    , Network.TypedProtocol.Stateful.Proofs\n                    , Network.TypedProtocol.Stateful.Codec\n  build-depends:      base,\n                      singletons,\n                      io-classes:io-classes,\n                      typed-protocols:typed-protocols\n\n  hs-source-dirs:     stateful\n  default-language:   GHC2021\n  default-extensions: DataKinds\n                      GADTs\n                      ImportQualifiedPost\n  ghc-options:        -Wall\n                      -Wno-unticked-promoted-constructors\n                      -Wcompat\n                      -Wincomplete-uni-patterns\n                      -Wincomplete-record-updates\n                      -Wpartial-fields\n                      -Widentities\n                      -Wredundant-constraints\n\nlibrary stateful-cborg\n  visibility:        public\n  exposed-modules:   Network.TypedProtocol.Stateful.Codec.CBOR\n\n  build-depends:     base,\n                     bytestring,\n                     cborg,\n                     singletons,       \n\n                     io-classes:io-classes,\n                     typed-protocols:{typed-protocols,cborg,stateful}\n\n  hs-source-dirs:    stateful-cborg\n  default-language:  GHC2021\n  default-extensions: ImportQualifiedPost\n  ghc-options:       -Wall\n                     -Wno-unticked-promoted-constructors\n                     -Wcompat\n                     -Wincomplete-uni-patterns\n                     -Wincomplete-record-updates\n                     -Wpartial-fields\n                     -Widentities\n                     -Wredundant-constraints\n\nlibrary examples\n  visibility:        public\n  exposed-modules:   Network.TypedProtocol.Channel\n                   , Network.TypedProtocol.Driver.Simple\n\n                   , Network.TypedProtocol.PingPong.Type\n                   , Network.TypedProtocol.PingPong.Client\n                   , Network.TypedProtocol.PingPong.Server\n                   , Network.TypedProtocol.PingPong.Codec\n                   , Network.TypedProtocol.PingPong.Codec.CBOR\n                   , Network.TypedProtocol.PingPong.Examples\n\n                   , Network.TypedProtocol.ReqResp.Type\n                   , Network.TypedProtocol.ReqResp.Client\n                   , Network.TypedProtocol.ReqResp.Server\n                   , Network.TypedProtocol.ReqResp.Codec\n                   , Network.TypedProtocol.ReqResp.Codec.CBOR\n                   , Network.TypedProtocol.ReqResp.Examples\n\n                   , Network.TypedProtocol.ReqResp2.Type\n                   , Network.TypedProtocol.ReqResp2.Client\n\n                   , Network.TypedProtocol.Stateful.ReqResp.Type\n                   , Network.TypedProtocol.Stateful.ReqResp.Client\n                   , Network.TypedProtocol.Stateful.ReqResp.Server\n                   , Network.TypedProtocol.Stateful.ReqResp.Codec\n                   , Network.TypedProtocol.Stateful.ReqResp.Examples\n\n                   , Network.TypedProtocol.Trans.Wedge\n  build-depends:     base,\n                     bytestring,\n                     cborg,\n                     serialise,\n                     singletons,\n                     contra-tracer,\n                     io-classes:{io-classes, si-timers},\n                     network,\n                     time,\n                     typed-protocols:{typed-protocols,cborg,stateful}\n\n  hs-source-dirs:    examples\n  default-language:  GHC2021\n  default-extensions: DataKinds\n                      GADTs\n                      LambdaCase\n  ghc-options:       -Wall\n                     -Wno-unticked-promoted-constructors\n                     -Wcompat\n                     -Wincomplete-uni-patterns\n                     -Wincomplete-record-updates\n                     -Wpartial-fields\n                     -Widentities\n                     -Wredundant-constraints\n\ntest-suite test\n  type:              exitcode-stdio-1.0\n  main-is:           Main.hs\n  hs-source-dirs:    test\n  default-language:  GHC2021\n  default-extensions: GADTs\n                      LambdaCase\n  other-modules:     Network.TypedProtocol.PingPong.Tests\n                   , Network.TypedProtocol.ReqResp.Tests\n  build-depends:     base\n                   , bytestring\n                   , contra-tracer\n                   , typed-protocols:{typed-protocols,cborg,examples}\n                   , io-classes:{io-classes,si-timers}\n                   , io-sim\n                   , QuickCheck\n                   , tasty\n                   , tasty-quickcheck\n\n  if !os(windows)\n      build-depends: directory\n                   , network\n                   , unix\n\n  ghc-options:       -rtsopts\n                     -Wall\n                     -Wno-unticked-promoted-constructors\n                     -Wno-orphans\n";
  }