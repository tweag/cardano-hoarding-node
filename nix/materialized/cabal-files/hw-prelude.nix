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
    flags = { werror = false; };
    package = {
      specVersion = "3.4";
      identifier = { name = "hw-prelude"; version = "0.0.5.0"; };
      license = "Apache-2.0";
      copyright = "2024 John Ky";
      maintainer = "newhoggy@gmail.com";
      author = "John Ky";
      homepage = "";
      url = "";
      synopsis = "Opinionated prelude library";
      description = "Opinionated prelude library.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
        ] ++ pkgs.lib.optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"));
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hw-prelude-0.0.5.0.tar.gz";
      sha256 = "23ab82027ade1a36c9f72ac21b11b50773613b777364bb1335899d121dac7bc6";
    });
  }) // {
    package-description-override = "cabal-version:          3.4\nname:                   hw-prelude\nversion:                0.0.5.0\nsynopsis:               Opinionated prelude library\ndescription:            Opinionated prelude library.\nlicense:                Apache-2.0\nlicense-file:           LICENSE\nauthor:                 John Ky\nmaintainer:             newhoggy@gmail.com\ncopyright:              2024 John Ky\ncategory:               Development\nbuild-type:             Simple\nextra-doc-files:        CHANGELOG.md\nextra-source-files:     README.md\n\nsource-repository head\n  type:                 git\n  location:             https://github.com/haskell-works/hw-prelude\n\ncommon base                       { build-depends: base                       >= 4.13       && < 5      }\n\ncommon aeson                      { build-depends: aeson                                       < 2.3    }\ncommon async                      { build-depends: async                                       < 2.3    }\ncommon bytestring                 { build-depends: bytestring                                  < 0.13   }\ncommon contravariant              { build-depends: contravariant                               < 1.6    }\ncommon directory                  { build-depends: directory                                   < 1.4    }\ncommon filepath                   { build-depends: filepath                                    < 1.6    }\ncommon generic-lens               { build-depends: generic-lens                                < 2.3    }\ncommon microlens                  { build-depends: microlens                                   < 0.5    }\ncommon network                    { build-depends: network                                     < 3.3    }\ncommon process                    { build-depends: process                                     < 1.7    }\ncommon resourcet                  { build-depends: resourcet                                   < 1.4    }\ncommon text                       { build-depends: text                                        < 3      }\ncommon transformers               { build-depends: transformers                                < 0.7    }\ncommon unliftio                   { build-depends: unliftio                                    < 0.3    }\n\ncommon hw-prelude                 { build-depends: hw-prelude                                           }\n\ncommon Win32\n  if os(windows)\n    build-depends:      Win32   >= 2.5.4.1 && < 3\n\nflag werror\n  description: Enable -Werror\n  manual: True\n  default: False\n\ncommon project-config\n  default-language:     Haskell2010\n  default-extensions:   BlockArguments\n                        DataKinds\n                        DeriveGeneric\n                        DuplicateRecordFields\n                        FlexibleContexts\n                        FlexibleInstances\n                        LambdaCase\n                        MultiWayIf\n                        NoFieldSelectors\n                        NoImplicitPrelude\n                        OverloadedRecordDot\n                        OverloadedStrings\n                        RankNTypes\n                        ScopedTypeVariables\n                        TypeApplications\n                        TypeOperators\n                        TypeSynonymInstances\n  ghc-options:          -Wall\n\n  if flag(werror)\n    ghc-options:        -Werror\n\nlibrary\n  import:               base, project-config,\n                        aeson,\n                        async,\n                        bytestring,\n                        contravariant,\n                        directory,\n                        filepath,\n                        generic-lens,\n                        microlens,\n                        network,\n                        process,\n                        resourcet,\n                        text,\n                        transformers,\n                        unliftio,\n                        Win32,\n\n  if os(windows)\n    exposed-modules:    HaskellWorks.IO.Win32.NamedPipe\n\n  exposed-modules:      HaskellWorks.Control.Monad\n                        HaskellWorks.Data.String\n                        HaskellWorks.Either\n                        HaskellWorks.Error\n                        HaskellWorks.Error.Types\n                        HaskellWorks.Error.Types.GenericError\n                        HaskellWorks.Error.Types.JsonDecodeError\n                        HaskellWorks.Error.Types.TimedOut\n                        HaskellWorks.Error.Types.YamlDecodeError\n                        HaskellWorks.FilePath\n                        HaskellWorks.IO.Network.NamedPipe\n                        HaskellWorks.IO.Network.Port\n                        HaskellWorks.IO.Network.Socket\n                        HaskellWorks.IO.Process\n                        HaskellWorks.OS\n                        HaskellWorks.Prelude\n                        HaskellWorks.Stack\n                        HaskellWorks.String\n                        HaskellWorks.ToText\n                        HaskellWorks.Tuple\n                        HaskellWorks.Unsafe\n  hs-source-dirs:       src\n";
  }