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
    flags = { cereal = true; serialise = true; http-api-data = true; };
    package = {
      specVersion = "1.12";
      identifier = { name = "base64-bytestring-type"; version = "1.0.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Erik de Castro Lopo <erikd@mega-nerd.com>";
      author = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      homepage = "https://github.com/erikd/base64-bytestring-type#readme";
      url = "";
      synopsis = "A newtype around ByteString, for base64 encoding";
      description = "A newtype around ByteString, for base64 encoding.\nStrict and lazy, normal and url alphabet variants.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = ((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
        ] ++ pkgs.lib.optional (!(compiler.isGhc && compiler.version.ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ pkgs.lib.optional (flags.cereal) (hsPkgs."cereal" or (errorHandler.buildDepError "cereal"))) ++ pkgs.lib.optional (flags.serialise) (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))) ++ pkgs.lib.optional (flags.http-api-data) (hsPkgs."http-api-data" or (errorHandler.buildDepError "http-api-data"));
        buildable = true;
      };
      tests = {
        "tests" = {
          depends = (([
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64-bytestring-type" or (errorHandler.buildDepError "base64-bytestring-type"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          ] ++ pkgs.lib.optional (flags.cereal) (hsPkgs."cereal" or (errorHandler.buildDepError "cereal"))) ++ pkgs.lib.optional (flags.serialise) (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))) ++ pkgs.lib.optional (flags.http-api-data) (hsPkgs."http-api-data" or (errorHandler.buildDepError "http-api-data"));
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/base64-bytestring-type-1.0.1.tar.gz";
      sha256 = "f607d07c4aab227b4536c495fa7c07b35ddc9c2c013d385c16c02f236526780e";
    });
  }) // {
    package-description-override = "cabal-version:      1.12\r\nname:               base64-bytestring-type\r\nversion:            1.0.1\r\nx-revision: 22\r\nsynopsis:           A newtype around ByteString, for base64 encoding\r\ndescription:\r\n  A newtype around ByteString, for base64 encoding.\r\n  Strict and lazy, normal and url alphabet variants.\r\n\r\ncategory:           Data\r\nhomepage:           https://github.com/erikd/base64-bytestring-type#readme\r\nbug-reports:        https://github.com/erikd/base64-bytestring-type/issues\r\nauthor:             Oleg Grenrus <oleg.grenrus@iki.fi>\r\nmaintainer:         Erik de Castro Lopo <erikd@mega-nerd.com>\r\nlicense:            BSD3\r\nlicense-file:       LICENSE\r\nbuild-type:         Simple\r\ntested-with:\r\n  GHC ==7.8.4\r\n   || ==7.10.3\r\n   || ==8.0.2\r\n   || ==8.2.2\r\n   || ==8.4.4\r\n   || ==8.6.5\r\n   || ==8.8.4\r\n   || ==8.10.7\r\n   || ==9.0.2\r\n   || ==9.2.8\r\n   || ==9.4.8\r\n   || ==9.6.3\r\n   || ==9.8.1\r\n\r\nextra-source-files:\r\n  README.md\r\n  CHANGELOG.md\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/erikd/base64-bytestring-type\r\n\r\nflag cereal\r\n  description: Instances for @Serialize@ from @cereal@ package\r\n  manual:      True\r\n  default:     True\r\n\r\nflag serialise\r\n  description: Instances for @Serialise@ from @serialise@ package\r\n  manual:      True\r\n  default:     True\r\n\r\nflag http-api-data\r\n  description:\r\n    Instances for @To/FromHttpApiData@ from @http-api-data@ package\r\n\r\n  manual:      True\r\n  default:     True\r\n\r\nlibrary\r\n  hs-source-dirs:   src\r\n  ghc-options:      -Wall\r\n\r\n  -- boot libraries\r\n  -- https://ghc.haskell.org/trac/ghc/wiki/Commentary/Libraries/VersionHistory\r\n  build-depends:\r\n      base        >=4.7.0.0  && <4.22\r\n    , binary      >=0.7.1.0  && <0.10\r\n    , bytestring  >=0.10.4.0 && <0.13\r\n    , deepseq     >=1.3.0.2  && <1.6\r\n    , text        >=1.2.3.0  && <1.3 || >=2.0 && <2.2\r\n\r\n  -- other dependencies:\r\n  build-depends:\r\n      aeson              >=1.2.3.0 && <1.6 || >=2.0 && <2.3\r\n    , base-compat        >=0.9.3   && <0.15\r\n    , base64-bytestring  >=1.0.0.1 && <1.3\r\n    , hashable           >=1.2.6.1 && <1.6\r\n    , QuickCheck         >=2.11.3  && <2.17\r\n\r\n  if !impl(ghc >=8.0)\r\n    build-depends: semigroups >=0.18.5 && <0.21\r\n\r\n  if flag(cereal)\r\n    build-depends: cereal >=0.5.5.0 && <0.6\r\n\r\n  if flag(serialise)\r\n    build-depends: serialise >=0.2.1.0 && <0.3\r\n\r\n  if flag(http-api-data)\r\n    build-depends: http-api-data >=0.4 && <0.7\r\n\r\n  exposed-modules:\r\n    Data.ByteString.Base64.Lazy.Type\r\n    Data.ByteString.Base64.Type\r\n    Data.ByteString.Base64.URL.Lazy.Type\r\n    Data.ByteString.Base64.URL.Type\r\n\r\n  default-language: Haskell2010\r\n\r\ntest-suite tests\r\n  default-language: Haskell2010\r\n  type:             exitcode-stdio-1.0\r\n  main-is:          Tests.hs\r\n  hs-source-dirs:   test\r\n  ghc-options:      -Wall\r\n\r\n  -- dependencies with bounds inherited from library:\r\n  build-depends:\r\n      aeson\r\n    , base\r\n    , base64-bytestring-type\r\n    , binary\r\n    , bytestring\r\n\r\n  if flag(cereal)\r\n    build-depends: cereal\r\n\r\n  if flag(serialise)\r\n    build-depends: serialise\r\n\r\n  if flag(http-api-data)\r\n    build-depends: http-api-data\r\n\r\n  -- other dependencies\r\n  build-depends:\r\n      tasty             >=1.2.1 && <1.6\r\n    , tasty-quickcheck  >=0.10  && <0.12\r\n";
  }