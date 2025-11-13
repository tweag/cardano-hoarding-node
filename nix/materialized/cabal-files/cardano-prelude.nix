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
    flags = { development = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "cardano-prelude"; version = "0.2.1.0"; };
      license = "Apache-2.0";
      copyright = "2018-2023 Input Output Global Inc (IOG)";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "A Prelude replacement for the Cardano project";
      description = "A Prelude replacement for the Cardano project";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."canonical-json" or (errorHandler.buildDepError "canonical-json"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."ghc-heap" or (errorHandler.buildDepError "ghc-heap"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/cardano-prelude-0.2.1.0.tar.gz";
      sha256 = "021413c092137a0785f0cdd662ff0df9705ade3bbb9e381710da19fdb1d2e456";
    });
  }) // {
    package-description-override = "cabal-version: 2.2\n\nname:                 cardano-prelude\nversion:              0.2.1.0\nsynopsis:             A Prelude replacement for the Cardano project\ndescription:          A Prelude replacement for the Cardano project\nauthor:               IOHK\nmaintainer:           operations@iohk.io\ncopyright:            2018-2023 Input Output Global Inc (IOG)\nlicense:              Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\ncategory:             Currency\nbuild-type:           Simple\nextra-source-files:   ChangeLog.md, README.md cbits/hashset.h cbits/worklist.h\ntested-with:          GHC == 9.2.5, GHC == 8.10.7\n\nflag development\n  description: Disable `-Werror`\n  default: False\n  manual: True\n\nlibrary\n  hs-source-dirs:     src\n  exposed-modules:    Cardano.Prelude\n                      Data.Semigroup.Action\n  other-modules:      Cardano.Prelude.Base\n                      Cardano.Prelude.Bool\n                      Cardano.Prelude.ConvertText\n                      Cardano.Prelude.Compat.ByteString.Short\n                      Cardano.Prelude.Either\n                      Cardano.Prelude.Error\n                      Cardano.Prelude.Formatting\n                      Cardano.Prelude.Functor\n                      Cardano.Prelude.GHC.Heap\n                      Cardano.Prelude.GHC.Heap.NormalForm\n                      Cardano.Prelude.GHC.Heap.Size\n                      Cardano.Prelude.GHC.Heap.Tree\n                      Cardano.Prelude.Json.Canonical\n                      Cardano.Prelude.Json.Parse\n                      Cardano.Prelude.Microlens\n                      Cardano.Prelude.Orphans\n                      Cardano.Prelude.Panic\n                      Cardano.Prelude.Read\n                      Cardano.Prelude.Safe\n                      Cardano.Prelude.Show\n                      Cardano.Prelude.Strict\n\n  build-depends:      base                    >= 4.14       && < 5\n                    , aeson                   >= 2.0\n                    , async\n                    , base16-bytestring       >= 1\n                    , bytestring\n                    , canonical-json          >= 0.6.0.1\n                    , cborg\n                    , containers\n                    , deepseq\n                    , extra\n                    , formatting\n                    , ghc-heap\n                    , ghc-prim\n                    , integer-gmp\n                    , microlens               < 0.5\n                    , mtl\n                    , stm\n                    , tagged\n                    , text\n                    , time\n                    , transformers\n  default-language:   Haskell2010\n  c-sources:          cbits/hashset.c\n                      cbits/worklist.c\n                      cbits/closure_size.c\n  ghc-options:        -Wall\n\n  cc-options:         -Wall\n\n  if (!flag(development))\n    ghc-options:      -Werror\n";
  }