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
      specVersion = "3.0";
      identifier = { name = "kes-agent"; version = "0.2.0.1"; };
      license = "Apache-2.0";
      copyright = "INTERSECT 2024-2025.";
      maintainer = "tobias@well-typed.com";
      author = "Tobias Dammers";
      homepage = "";
      url = "";
      synopsis = "KES agent library and binaries";
      description = "KES agent provides a solution for storing KES keys\n       in-process, in order to allow them to be erased securely and\n       enable forward security in the Cardano blockchain.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."blaze-html" or (errorHandler.buildDepError "blaze-html"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."casing" or (errorHandler.buildDepError "casing"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."kes-agent-crypto" or (errorHandler.buildDepError "kes-agent-crypto"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."serdoc-core" or (errorHandler.buildDepError "serdoc-core"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
        ] ++ pkgs.lib.optionals (!system.isWasm32) [
          (hsPkgs."socket" or (errorHandler.buildDepError "socket"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
        ]) ++ pkgs.lib.optional (!(system.isWindows || system.isWasm32)) (hsPkgs."hsyslog" or (errorHandler.buildDepError "hsyslog"));
        buildable = true;
      };
      exes = {
        "kes-agent" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."kes-agent" or (errorHandler.buildDepError "kes-agent"))
            (hsPkgs."kes-agent-crypto" or (errorHandler.buildDepError "kes-agent-crypto"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."serdoc-core" or (errorHandler.buildDepError "serdoc-core"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."tomland" or (errorHandler.buildDepError "tomland"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
          ] ++ pkgs.lib.optionals (!system.isWindows) [
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."hdaemonize" or (errorHandler.buildDepError "hdaemonize"))
            (hsPkgs."hsyslog" or (errorHandler.buildDepError "hsyslog"))
          ];
          buildable = if system.isWasm32 then false else true;
        };
        "kes-service-client-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."kes-agent" or (errorHandler.buildDepError "kes-agent"))
            (hsPkgs."kes-agent-crypto" or (errorHandler.buildDepError "kes-agent-crypto"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
          ] ++ pkgs.lib.optional (!system.isWindows) (hsPkgs."hsyslog" or (errorHandler.buildDepError "hsyslog"));
          buildable = if system.isWasm32 then false else true;
        };
        "kes-agent-control" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."kes-agent" or (errorHandler.buildDepError "kes-agent"))
            (hsPkgs."kes-agent-crypto" or (errorHandler.buildDepError "kes-agent-crypto"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."serdoc-core" or (errorHandler.buildDepError "serdoc-core"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
          ] ++ pkgs.lib.optional (!system.isWindows) (hsPkgs."hsyslog" or (errorHandler.buildDepError "hsyslog"));
          buildable = if system.isWasm32 then false else true;
        };
      };
      tests = {
        "kes-agent-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."kes-agent" or (errorHandler.buildDepError "kes-agent"))
            (hsPkgs."kes-agent-crypto" or (errorHandler.buildDepError "kes-agent-crypto"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-tests" or (errorHandler.buildDepError "cardano-crypto-tests"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."ouroboros-network-testing" or (errorHandler.buildDepError "ouroboros-network-testing"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."serdoc-core" or (errorHandler.buildDepError "serdoc-core"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
          ] ++ pkgs.lib.optionals (!system.isWindows) [
            (hsPkgs."socket-unix" or (errorHandler.buildDepError "socket-unix"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
          ];
          build-tools = [
            (hsPkgs.pkgsBuildBuild.kes-agent.components.exes.kes-agent or (pkgs.pkgsBuildBuild.kes-agent or (errorHandler.buildToolDepError "kes-agent:kes-agent")))
            (hsPkgs.pkgsBuildBuild.kes-agent.components.exes.kes-agent-control or (pkgs.pkgsBuildBuild.kes-agent-control or (errorHandler.buildToolDepError "kes-agent:kes-agent-control")))
            (hsPkgs.pkgsBuildBuild.kes-agent.components.exes.kes-service-client-demo or (pkgs.pkgsBuildBuild.kes-service-client-demo or (errorHandler.buildToolDepError "kes-agent:kes-service-client-demo")))
          ];
          buildable = if system.isWasm32 then false else true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/kes-agent-0.2.0.1.tar.gz";
      sha256 = "526a851e4900837c316b8bbf033290890597fcca7d47ef498fa12c860580c682";
    });
  }) // {
    package-description-override = "cabal-version: 3.0\nname: kes-agent\nversion: 0.2.0.1\n\nsynopsis: KES agent library and binaries\ndescription: KES agent provides a solution for storing KES keys\n                    in-process, in order to allow them to be erased securely and\n                    enable forward security in the Cardano blockchain.\n\nlicense: Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\nauthor: Tobias Dammers\nmaintainer: tobias@well-typed.com\n\ncopyright: INTERSECT 2024-2025.\ncategory: Network\n\ndata-files: fixtures/*.vkey\n          , fixtures/*.skey\n          , fixtures/opcert.cert\n          , fixtures/opcert.counter\n          , fixtures/mainnet-shelley-genesis.json\n\ncommon project-config\n     if !arch(wasm32)\n         ghc-options:\n             -threaded\n             -rtsopts=all\n     ghc-options:\n         -haddock\n         -Wunused-imports\n\ncommon base-dependencies\n    build-depends: base >=4.14.0.0 && <4.22.0.0\n\nlibrary\n    import: project-config\n    import: base-dependencies\n    exposed-modules:\n                   Cardano.KESAgent.Priority\n                   Cardano.KESAgent.Processes.Agent\n                   Cardano.KESAgent.Processes.Agent.Type\n                   Cardano.KESAgent.Processes.Agent.Context\n                   Cardano.KESAgent.Processes.Agent.ControlActions\n                   Cardano.KESAgent.Processes.Agent.CommonActions\n                   Cardano.KESAgent.Processes.Agent.ControlDrivers\n                   Cardano.KESAgent.Processes.Agent.Monad\n                   Cardano.KESAgent.Processes.Agent.ServiceDrivers\n                   Cardano.KESAgent.Processes.ControlClient\n                   Cardano.KESAgent.Processes.ServiceClient\n                   Cardano.KESAgent.Serialization.CBOR\n                   Cardano.KESAgent.Serialization.TextEnvelope\n                   Cardano.KESAgent.Serialization.RawUtil\n                   Cardano.KESAgent.Serialization.DirectCodec\n                   Cardano.KESAgent.Protocols.Types\n                   Cardano.KESAgent.Protocols.AgentInfo\n                   Cardano.KESAgent.Protocols.BearerUtil\n\n                   Cardano.KESAgent.Protocols.VersionHandshake.Driver\n                   Cardano.KESAgent.Protocols.VersionHandshake.Peers\n                   Cardano.KESAgent.Protocols.VersionHandshake.Protocol\n\n                   Cardano.KESAgent.Protocols.Service.V0.Driver\n                   Cardano.KESAgent.Protocols.Service.V0.Peers\n                   Cardano.KESAgent.Protocols.Service.V0.Protocol\n                   Cardano.KESAgent.Protocols.Service.V1.Driver\n                   Cardano.KESAgent.Protocols.Service.V1.Peers\n                   Cardano.KESAgent.Protocols.Service.V1.Protocol\n                   Cardano.KESAgent.Protocols.Service.V2.Driver\n                   Cardano.KESAgent.Protocols.Service.V2.Peers\n                   Cardano.KESAgent.Protocols.Service.V2.Protocol\n\n                   Cardano.KESAgent.Protocols.Control.V0.Driver\n                   Cardano.KESAgent.Protocols.Control.V0.Peers\n                   Cardano.KESAgent.Protocols.Control.V0.Protocol\n                   Cardano.KESAgent.Protocols.Control.V1.Driver\n                   Cardano.KESAgent.Protocols.Control.V1.Peers\n                   Cardano.KESAgent.Protocols.Control.V1.Protocol\n                   Cardano.KESAgent.Protocols.Control.V2.Driver\n                   Cardano.KESAgent.Protocols.Control.V2.Peers\n                   Cardano.KESAgent.Protocols.Control.V2.Protocol\n                   Cardano.KESAgent.Protocols.Control.V3.Driver\n                   Cardano.KESAgent.Protocols.Control.V3.Peers\n                   Cardano.KESAgent.Protocols.Control.V3.Protocol\n\n                   Cardano.KESAgent.Protocols.RecvResult\n                   Cardano.KESAgent.Util.Formatting\n                   Cardano.KESAgent.Util.RetrySocket\n                   Cardano.KESAgent.Util.PlatformPoison\n                   Cardano.KESAgent.Util.ColoredOutput\n                   Cardano.KESAgent.Util.Version\n                   Cardano.KESAgent.Util.GetVersion\n\n                   Paths_kes_agent\n    autogen-modules:\n                   Paths_kes_agent\n\n    build-depends: async\n                 , aeson >=2.0\n                 , base16-bytestring\n                 , binary\n                 , blaze-html >=0.9 && <0.10\n                 , bytestring >=0.11\n                 , cardano-crypto-class >=2.2\n                 , cardano-binary\n                   -- We need to pin this down to this exact version, because\n                   -- there are two independent packages named contra-tracer:\n                   -- one in CHaP (which is the one we will use), and one in\n                   -- Hackage (which we cannot use due to other cardano\n                   -- packages depending on the one in CHaP). Since the two\n                   -- packages have the same name, we can only disambiguate\n                   -- them by demanding a version that only one of them has.\n                 , contra-tracer ==0.1.0.1 || ==0.1.0.2\n                 , containers >=0.6.5.1 && <0.9\n                 , casing >=0.1.4.1 && <0.2\n                 , extra >=1.7.12 && <1.9\n                 , formatting\n                 , ghc-prim\n                 , io-classes >=1.8.0.0\n                 , kes-agent-crypto ^>= 0.1.0.0\n                 , mtl\n                 , nothunks\n                 , ouroboros-network-framework\n                 , process >=1.6 && <1.7\n                 , quiet\n                 , serdoc-core\n                 , stm>=2.5 && <2.6\n                 , text\n                 , template-haskell >=2.18.0.0\n                 , time >=1.10\n                 , typed-protocols ^>=1.0 || ^>=1.1\n                 -- , typed-protocols-doc\n    if !arch(wasm32)\n        build-depends: socket >= 0.8.3 && <0.9\n                     , network\n                     , network-mux\n\n    hs-source-dirs: src\n    default-language: Haskell2010\n    if os(windows) || arch(wasm32)\n    else\n        build-depends: hsyslog\n\n\nexecutable kes-agent\n    import: project-config\n    import: base-dependencies\n    default-language: Haskell2010\n    hs-source-dirs: cli\n    main-is: AgentMain.hs\n    build-depends: kes-agent\n                 , kes-agent-crypto\n                 , async\n                 , bytestring\n                 , cardano-crypto-class\n                 , contra-tracer\n                 , containers\n                 , directory\n                 , filepath\n                 , io-classes\n                 , ouroboros-network-framework\n                 , optparse-applicative\n                 , serdoc-core\n                 , text\n                 , time\n                 , tomland\n                 , typed-protocols\n                 , Win32-network\n                 , network\n    if arch(wasm32)\n        buildable: False\n    if os(windows)\n    else\n        build-depends: unix\n                     , hdaemonize\n                     , hsyslog\n\nexecutable kes-service-client-demo\n    import: project-config\n    import: base-dependencies\n    default-language: Haskell2010\n    hs-source-dirs: cli\n    main-is: ServiceDemoMain.hs\n    build-depends: kes-agent\n                 , kes-agent-crypto\n                 , bytestring\n                 , cardano-crypto-class\n                 , contra-tracer\n                 , io-classes\n                 , network\n                 , ouroboros-network-framework\n                 , optparse-applicative\n                 , text\n                 , time\n                 , typed-protocols\n                 , Win32-network\n    if arch(wasm32)\n        buildable: False\n    if os(windows)\n    else\n        build-depends: hsyslog\n\nexecutable kes-agent-control\n    import: project-config\n    import: base-dependencies\n    default-language: Haskell2010\n    hs-source-dirs: cli\n    main-is: ControlMain.hs\n    build-depends: kes-agent\n                 , kes-agent-crypto\n                 , aeson >=2.0\n                 , bytestring\n                 , cardano-crypto-class\n                 , contra-tracer\n                 , extra >=1.7.12 && <1.9\n                 , io-classes\n                 , network\n                 , ouroboros-network-framework\n                 , optparse-applicative\n                 , serdoc-core\n                 , text\n                 , time\n                 , typed-protocols\n                 , Win32-network\n    if arch(wasm32)\n        buildable: False\n    if os(windows)\n    else\n        build-depends: hsyslog\n\ntest-suite kes-agent-tests\n    import: project-config\n    import: base-dependencies\n    type: exitcode-stdio-1.0\n    default-language: Haskell2010\n    hs-source-dirs: test\n    main-is: Main.hs\n    other-modules: Cardano.KESAgent.Tests.Simulation\n                 , Cardano.KESAgent.Tests.RefCounting\n                 , Cardano.KESAgent.Tests.OCert\n                 , Cardano.KESAgent.Tests.Serialization\n                 , Cardano.KESAgent.Tests.EndToEnd\n                 , Paths_kes_agent\n    build-tool-depends: kes-agent:kes-agent\n                      , kes-agent:kes-agent-control\n                      , kes-agent:kes-service-client-demo\n    build-depends: kes-agent\n                 , kes-agent-crypto\n                 , aeson >=2.0\n                 , async\n                 , bytestring\n                 , cardano-binary\n                 , cardano-crypto-class\n                 , cardano-crypto-tests\n                 , contra-tracer\n                 , containers\n                 , directory >=1.3.6.1 && <1.4.0.0\n                 , filepath >= 1.4.2 && <1.5\n                 , ghc-prim\n                 , io-classes\n                 , io-sim >=1.5.0\n                 , network\n                 , network-mux\n                 , ouroboros-network-framework\n                 , ouroboros-network-testing >=0.8\n                 , primitive >=0.7.4 && <0.10\n                 , process\n                 , QuickCheck\n                 , random >=1.2.1 && <1.3\n                 , serdoc-core\n                 , tasty\n                 , tasty-hunit\n                 , tasty-quickcheck\n                 , temporary >=1.3 && <1.4\n                 , text >=1.2.5\n                 , time >=1.10\n                 , Win32-network\n    if arch(wasm32)\n        buildable: False\n    if os(windows)\n    else\n        build-depends: socket-unix >=0.2.0.0 && <0.3.0.0\n        build-depends: unix\n";
  }