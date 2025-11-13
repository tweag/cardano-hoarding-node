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
      specVersion = "2.0";
      identifier = { name = "dns"; version = "4.2.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Kazu Yamamoto <kazu@iij.ad.jp>";
      author = "Kazu Yamamoto <kazu@iij.ad.jp>";
      homepage = "";
      url = "";
      synopsis = "DNS library in Haskell";
      description = "A thread-safe DNS library for both clients and servers written\nin pure Haskell.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."dns".components.sublibs.dns-internal or (errorHandler.buildDepError "dns:dns-internal"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."auto-update" or (errorHandler.buildDepError "auto-update"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."crypton" or (errorHandler.buildDepError "crypton"))
          (hsPkgs."hourglass" or (errorHandler.buildDepError "hourglass"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."psqueues" or (errorHandler.buildDepError "psqueues"))
        ];
        buildable = true;
      };
      sublibs = {
        "dns-internal" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."auto-update" or (errorHandler.buildDepError "auto-update"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."crypton" or (errorHandler.buildDepError "crypton"))
            (hsPkgs."hourglass" or (errorHandler.buildDepError "hourglass"))
            (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."psqueues" or (errorHandler.buildDepError "psqueues"))
          ];
          libs = pkgs.lib.optional (system.isWindows) (pkgs."iphlpapi" or (errorHandler.sysDepError "iphlpapi"));
          buildable = true;
        };
      };
      tests = {
        "network-tests" = {
          depends = [
            (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
            (hsPkgs."dns".components.sublibs.dns-internal or (errorHandler.buildDepError "dns:dns-internal"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
          ];
          build-tools = [
            (hsPkgs.pkgsBuildBuild.hspec-discover.components.exes.hspec-discover or (pkgs.pkgsBuildBuild.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
          ];
          buildable = true;
        };
        "spec-tests" = {
          depends = [
            (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
            (hsPkgs."dns".components.sublibs.dns-internal or (errorHandler.buildDepError "dns:dns-internal"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
            (hsPkgs."word8" or (errorHandler.buildDepError "word8"))
          ];
          build-tools = [
            (hsPkgs.pkgsBuildBuild.hspec-discover.components.exes.hspec-discover or (pkgs.pkgsBuildBuild.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/dns-4.2.0.tar.gz";
      sha256 = "c2d3ce5315a89ce2e362877928e5698159c898c29cb97f10506779ba0db497f9";
    });
  }) // {
    package-description-override = "cabal-version:      2.0\nname:               dns\nversion:            4.2.0\nlicense:            BSD3\nlicense-file:       LICENSE\nmaintainer:         Kazu Yamamoto <kazu@iij.ad.jp>\nauthor:             Kazu Yamamoto <kazu@iij.ad.jp>\ntested-with:\n    ghc ==8.0.2 ghc ==8.2.2 ghc ==8.4.4 ghc ==8.6.5 ghc ==8.8.1\n\nsynopsis:           DNS library in Haskell\ndescription:\n    A thread-safe DNS library for both clients and servers written\n    in pure Haskell.\n\ncategory:           Network\nbuild-type:         Simple\nextra-source-files:\n    Changelog.md\n    cabal.project\n    cbits/dns.c\n\nsource-repository head\n    type:     git\n    location: https://github.com/kazu-yamamoto/dns.git\n\nlibrary\n    exposed-modules:\n        Network.DNS\n        Network.DNS.Lookup\n        Network.DNS.LookupRaw\n        Network.DNS.Resolver\n        Network.DNS.Utils\n        Network.DNS.Types\n        Network.DNS.Decode\n        Network.DNS.Encode\n        Network.DNS.IO\n\n    other-modules:    Network.DNS.Transport\n    default-language: Haskell2010\n    ghc-options:      -Wall\n    build-depends:\n        dns-internal,\n        base >=4 && <5,\n        array,\n        async,\n        attoparsec,\n        auto-update,\n        base16-bytestring,\n        base64-bytestring,\n        bytestring,\n        containers,\n        crypton,\n        hourglass,\n        iproute >=1.3.2,\n        mtl,\n        network >=2.3,\n        psqueues\n\nlibrary dns-internal\n    exposed-modules:\n        Network.DNS.Imports\n        Network.DNS.Types.Internal\n        Network.DNS.Types.Resolver\n        Network.DNS.Resolver.Internal\n        Network.DNS.Decode.Parsers\n        Network.DNS.Decode.Internal\n        Network.DNS.Encode.Builders\n        Network.DNS.Encode.Internal\n        Network.DNS.StateBinary\n        Network.DNS.Memo\n        Network.DNS.Base32Hex\n\n    hs-source-dirs:   internal\n    default-language: Haskell2010\n    ghc-options:      -Wall\n    build-depends:\n        base,\n        array,\n        async,\n        attoparsec,\n        auto-update,\n        base16-bytestring,\n        base64-bytestring,\n        bytestring,\n        case-insensitive,\n        containers,\n        crypton,\n        hourglass,\n        iproute,\n        mtl,\n        network,\n        psqueues\n\n    if os(windows)\n        c-sources:       cbits/dns.c\n        extra-libraries: iphlpapi\n\ntest-suite network-tests\n    type:               exitcode-stdio-1.0\n    main-is:            Spec.hs\n    build-tool-depends: hspec-discover:hspec-discover\n    hs-source-dirs:     test2\n    other-modules:\n        LookupSpec\n        IOSpec\n\n    default-language:   Haskell2010\n    ghc-options:        -Wall\n    build-depends:\n        dns,\n        dns-internal,\n        base,\n        hspec,\n        network\n\ntest-suite spec-tests\n    type:               exitcode-stdio-1.0\n    main-is:            Spec.hs\n    build-tool-depends: hspec-discover:hspec-discover\n    hs-source-dirs:     test\n    other-modules:\n        EncodeSpec\n        DecodeSpec\n        RoundTripSpec\n\n    default-language:   Haskell2010\n    ghc-options:        -Wall\n    build-depends:\n        dns,\n        dns-internal,\n        QuickCheck >=2.9,\n        base,\n        bytestring,\n        case-insensitive,\n        hspec,\n        iproute >=1.3.2,\n        word8\n";
  }