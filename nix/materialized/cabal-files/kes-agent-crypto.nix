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
      identifier = { name = "kes-agent-crypto"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2024-2025 Intersect";
      maintainer = "operations@iohk.io";
      author = "Tobias Dammers";
      homepage = "";
      url = "";
      synopsis = "KES agent crypto library";
      description = "";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/kes-agent-crypto-0.1.0.0.tar.gz";
      sha256 = "1f8e381dd1e196ff341e2b374cb6dc129f607837b1606afdbdd499a43479e871";
    });
  }) // {
    package-description-override = "cabal-version:      3.0\nname:               kes-agent-crypto\nversion:            0.1.0.0\nsynopsis: KES agent crypto library\n-- description:\nlicense:            Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\nauthor:             Tobias Dammers\nmaintainer:         operations@iohk.io\ncopyright:          2024-2025 Intersect\nbuild-type:         Simple\nextra-doc-files:    CHANGELOG.md\n\ncommon project-config\n     ghc-options:\n         -threaded\n         -rtsopts=all\n         -haddock\n         -Wunused-imports\n         -Wunused-packages\n\ncommon base-dependencies\n    build-depends: base >=4.14.0.0 && <4.22.0.0\n\n\nlibrary\n    import: project-config\n    import: base-dependencies\n    exposed-modules:\n                   Cardano.KESAgent.Protocols.StandardCrypto\n                   Cardano.KESAgent.Protocols.VersionedProtocol\n                   Cardano.KESAgent.KES.Evolution\n                   Cardano.KESAgent.KES.Crypto\n                   Cardano.KESAgent.KES.OCert\n                   Cardano.KESAgent.KES.Bundle\n                   Cardano.KESAgent.Util.HexBS\n                   Cardano.KESAgent.Util.Pretty\n                   Cardano.KESAgent.Util.RefCounting\n    build-depends: aeson >=2.0\n                 , bytestring >=0.11\n                 , cardano-crypto-class >=2.2\n                 , cardano-binary\n                   -- We need to pin this down to this exact version, because\n                   -- there are two independent packages named contra-tracer:\n                   -- one in CHaP (which is the one we will use), and one in\n                   -- Hackage (which we cannot use due to other cardano\n                   -- packages depending on the one in CHaP). Since the two\n                   -- packages have the same name, we can only disambiguate\n                   -- them by demanding a version that only one of them has.\n                 , contra-tracer ==0.1.0.1 || ==0.1.0.2\n                 , io-classes\n                 , nothunks\n                 , quiet\n                 , text\n                 , time >=1.10\n    hs-source-dirs: src\n    default-language: Haskell2010\n";
  }