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
      specVersion = "2.4";
      identifier = { name = "hedgehog-extras"; version = "0.10.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Supplemental library for hedgehog";
      description = "Supplemental library for hedgehog.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."Diff" or (errorHandler.buildDepError "Diff"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."http-conduit" or (errorHandler.buildDepError "http-conduit"))
          (hsPkgs."hw-prelude" or (errorHandler.buildDepError "hw-prelude"))
          (hsPkgs."lifted-async" or (errorHandler.buildDepError "lifted-async"))
          (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."tar" or (errorHandler.buildDepError "tar"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."tasty-discover" or (errorHandler.buildDepError "tasty-discover"))
          (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
          (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
        ] ++ pkgs.lib.optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"));
        buildable = true;
      };
      tests = {
        "hedgehog-extras-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hedgehog-extras" or (errorHandler.buildDepError "hedgehog-extras"))
            (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-discover" or (errorHandler.buildDepError "tasty-discover"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ];
          build-tools = [
            (hsPkgs.pkgsBuildBuild.tasty-discover.components.exes.tasty-discover or (pkgs.pkgsBuildBuild.tasty-discover or (errorHandler.buildToolDepError "tasty-discover:tasty-discover")))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hedgehog-extras-0.10.0.0.tar.gz";
      sha256 = "1d1212e573f3079aca7e4ccc934d9a5a45726898cf131abf9147139377dfb582";
    });
  }) // {
    package-description-override = "cabal-version: 2.4\n\nname:                   hedgehog-extras\nversion:                0.10.0.0\nsynopsis:               Supplemental library for hedgehog\ndescription:            Supplemental library for hedgehog.\ncategory:               Test\nauthor:                 IOHK\nmaintainer:             operations@iohk.io\nlicense:                Apache-2.0\nlicense-files:          LICENSE\n                        NOTICE\nbuild-type:             Simple\n\nsource-repository head\n  type:                 git\n  location:             https://github.com/input-output-hk/hedgehog-extras\n\ncommon aeson                        { build-depends: aeson                            >= 2.0.0.0                  }\ncommon aeson-pretty                 { build-depends: aeson-pretty                     >= 0.8.5                    }\ncommon async                        { build-depends: async                                                        }\ncommon base                         { build-depends: base                             >= 4.12       && < 4.22     }\ncommon bytestring                   { build-depends: bytestring                                                   }\ncommon containers                   { build-depends: containers                                                   }\ncommon deepseq                      { build-depends: deepseq                                                      }\ncommon Diff                         { build-depends: Diff                                                         }\ncommon directory                    { build-depends: directory                                                    }\ncommon exceptions                   { build-depends: exceptions                                                   }\ncommon filepath                     { build-depends: filepath                                                     }\ncommon generic-lens                 { build-depends: generic-lens                                                 }\ncommon hedgehog                     { build-depends: hedgehog                                                     }\ncommon hedgehog-quickcheck          { build-depends: hedgehog-quickcheck                                          }\ncommon http-conduit                 { build-depends: http-conduit                                                 }\ncommon hw-prelude                   { build-depends: hw-prelude                                                   }\ncommon lifted-async                 { build-depends: lifted-async                                                 }\ncommon lifted-base                  { build-depends: lifted-base                                                  }\ncommon microlens                    { build-depends: microlens                                                    }\ncommon mmorph                       { build-depends: mmorph                                                       }\ncommon monad-control                { build-depends: monad-control                                                }\ncommon mtl                          { build-depends: mtl                                                          }\ncommon network                      { build-depends: network                                                      }\ncommon process                      { build-depends: process                                                      }\ncommon resourcet                    { build-depends: resourcet                                                    }\ncommon retry                        { build-depends: retry                             >= 0.9                     }\ncommon stm                          { build-depends: stm                                                          }\ncommon tar                          { build-depends: tar                              ^>= 0.6                     }\ncommon tasty                        { build-depends: tasty                                                        }\ncommon tasty-discover               { build-depends: tasty-discover                   >= 5.0.2                    }\ncommon tasty-hedgehog               { build-depends: tasty-hedgehog                                               }\ncommon tasty-quickcheck             { build-depends: tasty-quickcheck                                             }\ncommon temporary                    { build-depends: temporary                                                    }\ncommon text                         { build-depends: text                                                         }\ncommon time                         { build-depends: time                             >= 1.9.1                    }\ncommon transformers                 { build-depends: transformers                                                 }\ncommon transformers-base            { build-depends: transformers-base                                            }\ncommon unliftio                     { build-depends: unliftio                                                     }\ncommon yaml                         { build-depends: yaml                                                         }\ncommon zlib                         { build-depends: zlib                                                         }\n\ncommon hedgehog-extras              { build-depends: hedgehog-extras                                              }\n\ncommon Win32\n  if os(windows)\n    build-depends:      Win32   >= 2.5.4.1\n\ncommon project-config\n  default-language:     Haskell2010\n  default-extensions:   NoImplicitPrelude\n  ghc-options:          -Wall\n                        -Wcompat\n                        -Wincomplete-record-updates\n                        -Wincomplete-uni-patterns\n                        -Wpartial-fields\n                        -Wredundant-constraints\n                        -Wunused-packages\n\nlibrary\n  import:               base, project-config,\n                        aeson-pretty,\n                        aeson,\n                        async,\n                        bytestring,\n                        containers,\n                        deepseq,\n                        Diff,\n                        directory,\n                        exceptions,\n                        filepath,\n                        generic-lens,\n                        hedgehog,\n                        http-conduit,\n                        hw-prelude,\n                        lifted-async,\n                        lifted-base,\n                        microlens,\n                        mmorph,\n                        monad-control,\n                        mtl,\n                        network,\n                        process,\n                        resourcet,\n                        stm,\n                        tar,\n                        tasty,\n                        tasty-discover,\n                        tasty-hedgehog,\n                        temporary,\n                        text,\n                        time,\n                        transformers,\n                        transformers-base,\n                        unliftio,\n                        Win32,\n                        yaml,\n                        zlib,\n  hs-source-dirs:       src\n\n\n  if os(windows)\n    exposed-modules:    Hedgehog.Extras.Internal.Win32.NamedPipes\n  exposed-modules:      Hedgehog.Extras\n                        Hedgehog.Extras.Aeson\n                        Hedgehog.Extras.Internal.Cli\n                        Hedgehog.Extras.Internal.Orphans\n                        Hedgehog.Extras.Internal.Plan\n                        Hedgehog.Extras.Internal.Test.Integration\n                        Hedgehog.Extras.Stock\n                        Hedgehog.Extras.Stock.Aeson\n                        Hedgehog.Extras.Stock.CallStack\n                        Hedgehog.Extras.Stock.IO.File\n                        Hedgehog.Extras.Stock.IO.Network.NamedPipe\n                        Hedgehog.Extras.Stock.IO.Network.Port\n                        Hedgehog.Extras.Stock.IO.Network.Socket\n                        Hedgehog.Extras.Stock.IO.Network.Sprocket\n                        Hedgehog.Extras.Stock.IO.Process\n                        Hedgehog.Extras.Stock.Monad\n                        Hedgehog.Extras.Stock.OS\n                        Hedgehog.Extras.Stock.String\n                        Hedgehog.Extras.Stock.Time\n                        Hedgehog.Extras.Test\n                        Hedgehog.Extras.Test.Base\n                        Hedgehog.Extras.Test.Concurrent\n                        Hedgehog.Extras.Test.File\n                        Hedgehog.Extras.Test.Golden\n                        Hedgehog.Extras.Test.MonadAssertion\n                        Hedgehog.Extras.Test.Network\n                        Hedgehog.Extras.Test.Prim\n                        Hedgehog.Extras.Test.Process\n                        Hedgehog.Extras.Test.TestWatchdog\n                        Hedgehog.Extras.Test.Tripwire\n                        Hedgehog.Extras.Test.Unit\n\ntest-suite hedgehog-extras-test\n  import:               base, project-config,\n                        directory,\n                        hedgehog,\n                        hedgehog-extras,\n                        lifted-base,\n                        network,\n                        process,\n                        resourcet,\n                        tasty,\n                        tasty-discover,\n                        tasty-hedgehog,\n                        transformers,\n                        time,\n  hs-source-dirs:       test\n  main-is:              hedgehog-extras-test.hs\n  type:                 exitcode-stdio-1.0\n\n  other-modules:        Hedgehog.Extras.Stock.IO.Network.PortSpec\n                        Hedgehog.Extras.Test.TestExpectFailure\n                        Hedgehog.Extras.Test.TestWatchdogSpec\n                        Hedgehog.Extras.Test.UnitSpec\n                        Hedgehog.Extras.Test.WorkspaceSpec\n\n  build-tool-depends:   tasty-discover:tasty-discover\n  ghc-options:          -threaded -rtsopts \"-with-rtsopts=-N -T\"\n";
  }