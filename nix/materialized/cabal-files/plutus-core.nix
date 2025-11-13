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
    flags = { with-inline-r = false; with-cert = false; };
    package = {
      specVersion = "3.0";
      identifier = { name = "plutus-core"; version = "1.53.1.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "michael.peyton-jones@iohk.io";
      author = "Plutus Core Team";
      homepage = "";
      url = "";
      synopsis = "Language library for Plutus Core";
      description = "Pretty-printer, parser, and typechecker for Plutus Core.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."barbies" or (errorHandler.buildDepError "barbies"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."bytestring-strict-builder" or (errorHandler.buildDepError "bytestring-strict-builder"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."composition-prelude" or (errorHandler.buildDepError "composition-prelude"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."dependent-sum" or (errorHandler.buildDepError "dependent-sum"))
          (hsPkgs."deriving-aeson" or (errorHandler.buildDepError "deriving-aeson"))
          (hsPkgs."deriving-compat" or (errorHandler.buildDepError "deriving-compat"))
          (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
          (hsPkgs."free" or (errorHandler.buildDepError "free"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."plutus-core".components.sublibs.index-envs or (errorHandler.buildDepError "plutus-core:index-envs"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
          (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
          (hsPkgs."mono-traversable" or (errorHandler.buildDepError "mono-traversable"))
          (hsPkgs."monoidal-containers" or (errorHandler.buildDepError "monoidal-containers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."multiset" or (errorHandler.buildDepError "multiset"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."parser-combinators" or (errorHandler.buildDepError "parser-combinators"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."prettyprinter-configurable" or (errorHandler.buildDepError "prettyprinter-configurable"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
          (hsPkgs."recursion-schemes" or (errorHandler.buildDepError "recursion-schemes"))
          (hsPkgs."plutus-core".components.sublibs.satint or (errorHandler.buildDepError "plutus-core:satint"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."some" or (errorHandler.buildDepError "some"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."th-lift" or (errorHandler.buildDepError "th-lift"))
          (hsPkgs."th-lift-instances" or (errorHandler.buildDepError "th-lift-instances"))
          (hsPkgs."th-utilities" or (errorHandler.buildDepError "th-utilities"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."witherable" or (errorHandler.buildDepError "witherable"))
        ];
        buildable = true;
      };
      sublibs = {
        "untyped-plutus-core-testlib" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-core-testlib or (errorHandler.buildDepError "plutus-core:plutus-core-testlib"))
            (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."split" or (errorHandler.buildDepError "split"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
          buildable = if system.isWindows then false else true;
        };
        "plutus-ir" = {
          depends = [
            (hsPkgs."algebraic-graphs" or (errorHandler.buildDepError "algebraic-graphs"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
            (hsPkgs."dom-lt" or (errorHandler.buildDepError "dom-lt"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
            (hsPkgs."monoidal-containers" or (errorHandler.buildDepError "monoidal-containers"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."multiset" or (errorHandler.buildDepError "multiset"))
            (hsPkgs."parser-combinators" or (errorHandler.buildDepError "parser-combinators"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
            (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."witherable" or (errorHandler.buildDepError "witherable"))
          ];
          buildable = true;
        };
        "plutus-core-execlib" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."monoidal-containers" or (errorHandler.buildDepError "monoidal-containers"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-core-testlib or (errorHandler.buildDepError "plutus-core:plutus-core-testlib"))
            (hsPkgs."plutus-core".components.sublibs.plutus-ir or (errorHandler.buildDepError "plutus-core:plutus-ir"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
          buildable = true;
        };
        "plutus-core-testlib" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."dependent-map" or (errorHandler.buildDepError "dependent-map"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."free" or (errorHandler.buildDepError "free"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hedgehog-quickcheck" or (errorHandler.buildDepError "hedgehog-quickcheck"))
            (hsPkgs."lazy-search" or (errorHandler.buildDepError "lazy-search"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."multiset" or (errorHandler.buildDepError "multiset"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-ir or (errorHandler.buildDepError "plutus-core:plutus-ir"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."prettyprinter-configurable" or (errorHandler.buildDepError "prettyprinter-configurable"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."quickcheck-transformer" or (errorHandler.buildDepError "quickcheck-transformer"))
            (hsPkgs."size-based" or (errorHandler.buildDepError "size-based"))
            (hsPkgs."Stream" or (errorHandler.buildDepError "Stream"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-expected-failure" or (errorHandler.buildDepError "tasty-expected-failure"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
          buildable = true;
        };
        "plutus-ir-cert" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-ir or (errorHandler.buildDepError "plutus-core:plutus-ir"))
          ] ++ pkgs.lib.optional (flags.with-cert) (hsPkgs."plutus-cert" or (errorHandler.buildDepError "plutus-cert"));
          buildable = true;
        };
        "satint" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          ];
          buildable = true;
        };
        "index-envs" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."nonempty-vector" or (errorHandler.buildDepError "nonempty-vector"))
            (hsPkgs."ral" or (errorHandler.buildDepError "ral"))
          ];
          buildable = true;
        };
      };
      exes = {
        "plutus" = {
          depends = [
            (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-th" or (errorHandler.buildDepError "microlens-th"))
            (hsPkgs."mono-traversable" or (errorHandler.buildDepError "mono-traversable"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-ir or (errorHandler.buildDepError "plutus-core:plutus-ir"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."singletons" or (errorHandler.buildDepError "singletons"))
            (hsPkgs."singletons-th" or (errorHandler.buildDepError "singletons-th"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."text-zipper" or (errorHandler.buildDepError "text-zipper"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."vty-crossplatform" or (errorHandler.buildDepError "vty-crossplatform"))
          ];
          buildable = if system.isWindows then false else true;
        };
        "traceToStacks" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
          buildable = true;
        };
        "cost-model-budgeting-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."criterion-measurement" or (errorHandler.buildDepError "criterion-measurement"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
          buildable = true;
        };
        "generate-cost-model" = {
          depends = [
            (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
            (hsPkgs."barbies" or (errorHandler.buildDepError "barbies"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."inline-r" or (errorHandler.buildDepError "inline-r"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
          buildable = (if !flags.with-inline-r
            then false
            else true) && (if system.isOsx
            then false
            else true) && (if system.isWindows then false else true);
        };
        "print-cost-model" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
          ];
          buildable = true;
        };
      };
      tests = {
        "plutus-core-test" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-core-testlib or (errorHandler.buildDepError "plutus-core:plutus-core-testlib"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."th-lift-instances" or (errorHandler.buildDepError "th-lift-instances"))
            (hsPkgs."th-utilities" or (errorHandler.buildDepError "th-utilities"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
          buildable = if system.isWindows then false else true;
        };
        "untyped-plutus-core-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-core".components.sublibs.untyped-plutus-core-testlib or (errorHandler.buildDepError "plutus-core:untyped-plutus-core-testlib"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          ];
          buildable = if system.isWindows then false else true;
        };
        "plutus-ir-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-core-testlib or (errorHandler.buildDepError "plutus-core:plutus-core-testlib"))
            (hsPkgs."plutus-core".components.sublibs.plutus-ir or (errorHandler.buildDepError "plutus-core:plutus-ir"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-expected-failure" or (errorHandler.buildDepError "tasty-expected-failure"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ];
          build-tools = [
            (hsPkgs.pkgsBuildBuild.tasty-discover.components.exes.tasty-discover or (pkgs.pkgsBuildBuild.tasty-discover or (errorHandler.buildToolDepError "tasty-discover:tasty-discover")))
          ];
          buildable = if system.isWindows then false else true;
        };
        "traceToStacks-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
          buildable = true;
        };
        "satint-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."plutus-core".components.sublibs.satint or (errorHandler.buildDepError "plutus-core:satint"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
          ];
          buildable = true;
        };
        "index-envs-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-core".components.sublibs.index-envs or (errorHandler.buildDepError "plutus-core:index-envs"))
            (hsPkgs."nonempty-vector" or (errorHandler.buildDepError "nonempty-vector"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          ];
          buildable = true;
        };
      };
      benchmarks = {
        "cost-model-test" = {
          depends = [
            (hsPkgs."barbies" or (errorHandler.buildDepError "barbies"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."inline-r" or (errorHandler.buildDepError "inline-r"))
            (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
          buildable = (if !flags.with-inline-r
            then false
            else true) && (if system.isOsx
            then false
            else true) && (if system.isWindows then false else true);
        };
        "index-envs-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."plutus-core".components.sublibs.index-envs or (errorHandler.buildDepError "plutus-core:index-envs"))
            (hsPkgs."nonempty-vector" or (errorHandler.buildDepError "nonempty-vector"))
            (hsPkgs."ral" or (errorHandler.buildDepError "ral"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/plutus-core-1.53.1.0.tar.gz";
      sha256 = "569f52190277c2447349ad2ea292ddd0b07753179b36dce03b6a07386df2d96c";
    });
  }) // {
    package-description-override = "cabal-version:      3.0\nname:               plutus-core\nversion:            1.53.1.0\nlicense:            Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\nmaintainer:         michael.peyton-jones@iohk.io\nauthor:             Plutus Core Team\nsynopsis:           Language library for Plutus Core\ndescription:        Pretty-printer, parser, and typechecker for Plutus Core.\ncategory:           Language, Plutus\nbuild-type:         Simple\nextra-doc-files:\n  CHANGELOG.md\n  README.md\n\n-- Using `extra-source-files` here means that Cabal can deduce that some of the\n-- Haskell source files depend on the JSON and R files, and it'll rebuild things\n-- as required if the files below are changed.  This doesn't happen if you use\n-- `data-files`.  See https://github.com/haskell/cabal/pull/6889 and the issue\n-- #4746 that it mentions.\nextra-source-files:\n  cost-model/data/*.R\n  cost-model/data/builtinCostModelA.json\n  cost-model/data/builtinCostModelB.json\n  cost-model/data/builtinCostModelC.json\n  cost-model/data/cekMachineCostsA.json\n  cost-model/data/cekMachineCostsB.json\n  cost-model/data/cekMachineCostsC.json\n  plutus-core/test/CostModelInterface/defaultCostModelParams.json\n\nsource-repository head\n  type:     git\n  location: https://github.com/IntersectMBO/plutus\n\n-- inline-r is a problematic dependency. It doesn't build with newer\n-- versions of R, so if we depend on it then people need to install\n-- an old R. This is not so bad for people working on plutus itself\n-- (use Nix or work it out), although we may want to eventually\n-- purge it. However, due to a cabal bug (https://github.com/haskell/cabal/issues/4087),\n-- in some cases cabal will require R to be installed _at solving time_,\n-- even though it never wants to build it. This means that the problem\n-- leaks into our downstream dependencies. So our solution is to guard\n-- the dependency behind a flag, off by default, and turn it on for\n-- ourselves locally.\nflag with-inline-r\n  description: Enable build of packages that use `inline-r`.\n  manual:      True\n  default:     False\n\nflag with-cert\n  description: Enable build of packages that use `plutus-cert`.\n  manual:      True\n  default:     False\n\ncommon lang\n  default-language:   Haskell2010\n  default-extensions:\n    DeriveFoldable\n    DeriveFunctor\n    DeriveGeneric\n    DeriveLift\n    DeriveTraversable\n    DerivingStrategies\n    DerivingVia\n    ExplicitForAll\n    FlexibleContexts\n    GeneralizedNewtypeDeriving\n    ImportQualifiedPost\n    ScopedTypeVariables\n    StandaloneDeriving\n\n  ghc-options:\n    -Wall -Wnoncanonical-monad-instances -Wincomplete-uni-patterns\n    -Wincomplete-record-updates -Wredundant-constraints -Widentities\n    -Wunused-packages -Wmissing-deriving-strategies\n\n  if impl(ghc >=9.8)\n    ghc-options: -Wno-x-partial\n\n-- This contains UPLC+TPLC, PIR must be explicitly included by depending\n-- on the public sub-library.\n-- In due course UPLC and TPLC should be split, with the main library\n-- containing UPLC.\nlibrary\n  import:             lang\n  exposed-modules:\n    Codec.Extras.FlatViaSerialise\n    Codec.Extras.SerialiseViaFlat\n    Data.Aeson.THReader\n    Data.Either.Extras\n    Data.List.Extras\n    Data.MultiSet.Lens\n    Data.Version.Extras\n    PlutusCore\n    PlutusCore.Analysis.Definitions\n    PlutusCore.Annotation\n    PlutusCore.Arity\n    PlutusCore.Bitwise\n    PlutusCore.Builtin\n    PlutusCore.Builtin.Debug\n    PlutusCore.Builtin.Elaborate\n    PlutusCore.Check.Normal\n    PlutusCore.Check.Scoping\n    PlutusCore.Check.Uniques\n    PlutusCore.Check.Value\n    PlutusCore.Compiler\n    PlutusCore.Compiler.Erase\n    PlutusCore.Compiler.Opts\n    PlutusCore.Compiler.Types\n    PlutusCore.Core\n    PlutusCore.Core.Plated\n    PlutusCore.Crypto.BLS12_381.Error\n    PlutusCore.Crypto.BLS12_381.G1\n    PlutusCore.Crypto.BLS12_381.G2\n    PlutusCore.Crypto.BLS12_381.Pairing\n    PlutusCore.Crypto.Ed25519\n    PlutusCore.Crypto.ExpMod\n    PlutusCore.Crypto.Hash\n    PlutusCore.Crypto.Secp256k1\n    PlutusCore.Data\n    PlutusCore.DataFilePaths\n    PlutusCore.DeBruijn\n    PlutusCore.DeBruijn.Internal\n    PlutusCore.Default\n    PlutusCore.Default.Builtins\n    PlutusCore.Error\n    PlutusCore.Evaluation.Error\n    PlutusCore.Evaluation.ErrorWithCause\n    PlutusCore.Evaluation.Machine.BuiltinCostModel\n    PlutusCore.Evaluation.Machine.Ck\n    PlutusCore.Evaluation.Machine.CostingFun.Core\n    PlutusCore.Evaluation.Machine.CostingFun.JSON\n    PlutusCore.Evaluation.Machine.CostingFun.SimpleJSON\n    PlutusCore.Evaluation.Machine.CostModelInterface\n    PlutusCore.Evaluation.Machine.CostStream\n    PlutusCore.Evaluation.Machine.ExBudget\n    PlutusCore.Evaluation.Machine.ExBudgetingDefaults\n    PlutusCore.Evaluation.Machine.ExBudgetStream\n    PlutusCore.Evaluation.Machine.Exception\n    PlutusCore.Evaluation.Machine.ExMemory\n    PlutusCore.Evaluation.Machine.ExMemoryUsage\n    PlutusCore.Evaluation.Machine.MachineParameters\n    PlutusCore.Evaluation.Machine.MachineParameters.Default\n    PlutusCore.Evaluation.Machine.SimpleBuiltinCostModel\n    PlutusCore.Evaluation.Result\n    PlutusCore.Examples.Builtins\n    PlutusCore.Examples.Data.Data\n    PlutusCore.Examples.Data.Function\n    PlutusCore.Examples.Data.InterList\n    PlutusCore.Examples.Data.List\n    PlutusCore.Examples.Data.Pair\n    PlutusCore.Examples.Data.Shad\n    PlutusCore.Examples.Data.TreeForest\n    PlutusCore.Examples.Data.Vec\n    PlutusCore.Examples.Everything\n    PlutusCore.Flat\n    PlutusCore.FsTree\n    PlutusCore.Mark\n    PlutusCore.MkPlc\n    PlutusCore.Name.Unique\n    PlutusCore.Name.UniqueMap\n    PlutusCore.Name.UniqueSet\n    PlutusCore.Normalize\n    PlutusCore.Normalize.Internal\n    PlutusCore.Parser\n    PlutusCore.Pretty\n    PlutusCore.Quote\n    PlutusCore.Rename\n    PlutusCore.Rename.Internal\n    PlutusCore.Rename.Monad\n    PlutusCore.Size\n    PlutusCore.StdLib.Data.Bool\n    PlutusCore.StdLib.Data.ChurchNat\n    PlutusCore.StdLib.Data.Data\n    PlutusCore.StdLib.Data.Function\n    PlutusCore.StdLib.Data.Integer\n    PlutusCore.StdLib.Data.List\n    PlutusCore.StdLib.Data.MatchOption\n    PlutusCore.StdLib.Data.Nat\n    PlutusCore.StdLib.Data.Pair\n    PlutusCore.StdLib.Data.ScottList\n    PlutusCore.StdLib.Data.ScottUnit\n    PlutusCore.StdLib.Data.Sum\n    PlutusCore.StdLib.Data.Unit\n    PlutusCore.StdLib.Everything\n    PlutusCore.StdLib.Meta\n    PlutusCore.StdLib.Meta.Data.Function\n    PlutusCore.StdLib.Meta.Data.Tuple\n    PlutusCore.StdLib.Type\n    PlutusCore.Subst\n    PlutusCore.TypeCheck\n    PlutusCore.TypeCheck.Internal\n    PlutusCore.Version\n    PlutusPrelude\n    Prettyprinter.Custom\n    Universe\n    UntypedPlutusCore\n    UntypedPlutusCore.Check.Scope\n    UntypedPlutusCore.Check.Uniques\n    UntypedPlutusCore.Contexts\n    UntypedPlutusCore.Core\n    UntypedPlutusCore.Core.Instance.Scoping\n    UntypedPlutusCore.Core.Plated\n    UntypedPlutusCore.Core.Type\n    UntypedPlutusCore.Core.Zip\n    UntypedPlutusCore.DeBruijn\n    UntypedPlutusCore.Evaluation.Machine.Cek\n    UntypedPlutusCore.Evaluation.Machine.Cek.CekMachineCosts\n    UntypedPlutusCore.Evaluation.Machine.Cek.Internal\n    UntypedPlutusCore.Evaluation.Machine.Cek.StepCounter\n    UntypedPlutusCore.Evaluation.Machine.SteppableCek\n    UntypedPlutusCore.Evaluation.Machine.SteppableCek.DebugDriver\n    UntypedPlutusCore.Evaluation.Machine.SteppableCek.Internal\n    UntypedPlutusCore.Mark\n    UntypedPlutusCore.MkUPlc\n    UntypedPlutusCore.Parser\n    UntypedPlutusCore.Purity\n    UntypedPlutusCore.Rename\n    UntypedPlutusCore.Rename.Internal\n    UntypedPlutusCore.Size\n    UntypedPlutusCore.Transform.CaseOfCase\n    UntypedPlutusCore.Transform.CaseReduce\n    UntypedPlutusCore.Transform.Cse\n    UntypedPlutusCore.Transform.FloatDelay\n    UntypedPlutusCore.Transform.ForceCaseDelay\n    UntypedPlutusCore.Transform.ForceDelay\n    UntypedPlutusCore.Transform.Inline\n    UntypedPlutusCore.Transform.Simplifier\n\n  other-modules:\n    Data.Aeson.Flatten\n    Data.Functor.Foldable.Monadic\n    Data.Vector.Orphans\n    PlutusCore.Builtin.Case\n    PlutusCore.Builtin.HasConstant\n    PlutusCore.Builtin.KnownKind\n    PlutusCore.Builtin.KnownType\n    PlutusCore.Builtin.KnownTypeAst\n    PlutusCore.Builtin.Meaning\n    PlutusCore.Builtin.Polymorphism\n    PlutusCore.Builtin.Result\n    PlutusCore.Builtin.Runtime\n    PlutusCore.Builtin.TestKnown\n    PlutusCore.Builtin.TypeScheme\n    PlutusCore.Core.Instance\n    PlutusCore.Core.Instance.Eq\n    PlutusCore.Core.Instance.Pretty\n    PlutusCore.Core.Instance.Pretty.Classic\n    PlutusCore.Core.Instance.Pretty.Default\n    PlutusCore.Core.Instance.Pretty.Plc\n    PlutusCore.Core.Instance.Pretty.Readable\n    PlutusCore.Core.Instance.Scoping\n    PlutusCore.Core.Type\n    PlutusCore.Crypto.Utils\n    PlutusCore.Default.Universe\n    PlutusCore.Eq\n    PlutusCore.Parser.Builtin\n    PlutusCore.Parser.ParserCommon\n    PlutusCore.Parser.Type\n    PlutusCore.Pretty.Classic\n    PlutusCore.Pretty.ConfigName\n    PlutusCore.Pretty.Default\n    PlutusCore.Pretty.Extra\n    PlutusCore.Pretty.Plc\n    PlutusCore.Pretty.PrettyConst\n    PlutusCore.Pretty.Readable\n    PlutusCore.Pretty.Utils\n    Universe.Core\n    UntypedPlutusCore.Analysis.Definitions\n    UntypedPlutusCore.Analysis.Usages\n    UntypedPlutusCore.Core.Instance\n    UntypedPlutusCore.Core.Instance.Eq\n    UntypedPlutusCore.Core.Instance.Flat\n    UntypedPlutusCore.Core.Instance.Pretty\n    UntypedPlutusCore.Core.Instance.Pretty.Classic\n    UntypedPlutusCore.Core.Instance.Pretty.Default\n    UntypedPlutusCore.Core.Instance.Pretty.Plc\n    UntypedPlutusCore.Core.Instance.Pretty.Readable\n    UntypedPlutusCore.Evaluation.Machine.Cek.EmitterMode\n    UntypedPlutusCore.Evaluation.Machine.Cek.ExBudgetMode\n    UntypedPlutusCore.Evaluation.Machine.CommonAPI\n    UntypedPlutusCore.Simplify\n    UntypedPlutusCore.Simplify.Opts\n    UntypedPlutusCore.Subst\n\n  reexported-modules: Data.SatInt\n  hs-source-dirs:\n    plutus-core/src plutus-core/stdlib plutus-core/examples\n    untyped-plutus-core/src prelude\n\n  -- Notes on dependencies:\n  -- * Bound on cardano-crypto-class for the fixed SECP primitives and 9.6 support\n  -- * The bound on 'dependent-sum' is needed to avoid https://github.com/obsidiansystems/dependent-sum/issues/72\n  build-depends:\n    , aeson\n    , array\n    , barbies\n    , base                        >=4.9     && <5\n    , base64-bytestring\n    , bimap\n    , bytestring\n    , bytestring-strict-builder\n    , cardano-crypto-class        ^>=2.2\n    , cassava\n    , cborg\n    , composition-prelude         >=1.1.0.1\n    , containers\n    , data-default-class\n    , deepseq\n    , dependent-sum               >=0.7.1.0\n    , deriving-aeson              >=0.2.3\n    , deriving-compat\n    , dlist\n    , exceptions\n    , extra\n    , filepath\n    , flat                        ^>=0.6\n    , free\n    , ghc-prim\n    , hashable                    >=1.4\n    , hedgehog                    >=1.0\n    , index-envs\n    , lens\n    , megaparsec\n    , mmorph\n    , mono-traversable\n    , monoidal-containers\n    , mtl\n    , multiset\n    , nothunks                    ^>=0.2\n    , parser-combinators          >=0.4.0\n    , prettyprinter               >=1.1.0.1\n    , prettyprinter-configurable\n    , primitive\n    , profunctors\n    , recursion-schemes\n    , satint\n    , semigroups                  >=0.19.1\n    , serialise\n    , some\n    , template-haskell\n    , text\n    , th-lift\n    , th-lift-instances\n    , th-utilities\n    , time\n    , transformers\n    , unordered-containers\n    , vector                      ^>=0.13.2\n    , witherable\n\ntest-suite plutus-core-test\n  import:           lang\n\n  -- needs linux 'diff' available\n  if os(windows)\n    buildable: False\n\n  type:             exitcode-stdio-1.0\n  main-is:          Spec.hs\n  hs-source-dirs:   plutus-core/test\n  ghc-options:      -threaded -rtsopts -with-rtsopts=-N\n  other-modules:\n    CBOR.DataStability\n    Check.Spec\n    CostModelInterface.Spec\n    CostModelSafety.Spec\n    Evaluation.Machines\n    Evaluation.Spec\n    Generators.QuickCheck.Utils\n    Names.Spec\n    Normalization.Check\n    Normalization.Type\n    Parser.Spec\n    Pretty.Readable\n    TypeSynthesis.Spec\n\n  default-language: Haskell2010\n  build-depends:\n    , aeson\n    , base                             >=4.9       && <5\n    , base16-bytestring                ^>=1.0\n    , bytestring\n    , containers\n    , data-default-class\n    , extra\n    , filepath\n    , flat                             ^>=0.6\n    , hedgehog\n    , mmorph\n    , mtl\n    , plutus-core                      ^>=1.53.1.0\n    , plutus-core:plutus-core-testlib\n    , prettyprinter\n    , serialise\n    , tasty\n    , tasty-golden\n    , tasty-hedgehog\n    , tasty-hunit\n    , tasty-quickcheck\n    , template-haskell\n    , text\n    , th-lift-instances\n    , th-utilities\n    , vector                           ^>=0.13.2\n\ntest-suite untyped-plutus-core-test\n  import:         lang\n\n  -- needs linux 'diff' available\n  if os(windows)\n    buildable: False\n\n  type:           exitcode-stdio-1.0\n  main-is:        Spec.hs\n  hs-source-dirs: untyped-plutus-core/test\n  ghc-options:    -O2 -threaded -rtsopts -with-rtsopts=-N\n  build-depends:\n    , base                                     >=4.9 && <5\n    , plutus-core:untyped-plutus-core-testlib\n    , tasty\n\nlibrary untyped-plutus-core-testlib\n  import:             lang\n  visibility:         public\n  default-language:   Haskell2010\n  default-extensions: CPP\n  hs-source-dirs:     untyped-plutus-core/testlib\n\n  -- needs linux 'diff' available\n  if os(windows)\n    buildable: False\n\n  exposed-modules:\n    Analysis.Lib\n    Analysis.Spec\n    DeBruijn.FlatNatWord\n    DeBruijn.Scope\n    DeBruijn.Spec\n    DeBruijn.UnDeBruijnify\n    Evaluation.Builtins\n    Evaluation.Builtins.Bitwise.CIP0122\n    Evaluation.Builtins.Bitwise.CIP0123\n    Evaluation.Builtins.BLS12_381\n    Evaluation.Builtins.BLS12_381.TestClasses\n    Evaluation.Builtins.BLS12_381.Utils\n    Evaluation.Builtins.Common\n    Evaluation.Builtins.Conversion\n    Evaluation.Builtins.Costing\n    Evaluation.Builtins.Definition\n    Evaluation.Builtins.Integer.Common\n    Evaluation.Builtins.Integer.DivModProperties\n    Evaluation.Builtins.Integer.ExpModIntegerProperties\n    Evaluation.Builtins.Integer.OrderProperties\n    Evaluation.Builtins.Integer.QuotRemProperties\n    Evaluation.Builtins.Integer.RingProperties\n    Evaluation.Builtins.MakeRead\n    Evaluation.Builtins.SignatureVerification\n    Evaluation.Debug\n    Evaluation.FreeVars\n    Evaluation.Golden\n    Evaluation.Helpers\n    Evaluation.Machines\n    Evaluation.Regressions\n    Flat.Spec\n    Generators.Spec\n    Scoping.Spec\n    Transform.CaseOfCase.Spec\n    Transform.Inline.Spec\n    Transform.Simplify.Lib\n    Transform.Simplify.Spec\n\n  build-depends:\n    , base                             >=4.9       && <5\n    , base16-bytestring\n    , bytestring\n    , cardano-crypto-class\n    , data-default-class\n    , dlist\n    , flat                             ^>=0.6\n    , hedgehog\n    , lens\n    , mtl\n    , plutus-core                      ^>=1.53.1.0\n    , plutus-core:plutus-core-testlib\n    , pretty-show\n    , prettyprinter\n    , QuickCheck\n    , serialise\n    , split\n    , tasty\n    , tasty-golden\n    , tasty-hedgehog\n    , tasty-hunit\n    , tasty-quickcheck\n    , text\n    , vector\n\n----------------------------------------------\n-- plutus-ir\n----------------------------------------------\n\nlibrary plutus-ir\n  import:          lang\n  visibility:      public\n  hs-source-dirs:  plutus-ir/src\n  exposed-modules:\n    PlutusIR\n    PlutusIR.Analysis.Builtins\n    PlutusIR.Analysis.Dependencies\n    PlutusIR.Analysis.RetainedSize\n    PlutusIR.Analysis.Size\n    PlutusIR.Analysis.VarInfo\n    PlutusIR.Check.Uniques\n    PlutusIR.Compiler\n    PlutusIR.Compiler.Datatype\n    PlutusIR.Compiler.Definitions\n    PlutusIR.Compiler.Let\n    PlutusIR.Compiler.Names\n    PlutusIR.Compiler.Provenance\n    PlutusIR.Compiler.Types\n    PlutusIR.Contexts\n    PlutusIR.Core\n    PlutusIR.Core.Instance\n    PlutusIR.Core.Instance.Flat\n    PlutusIR.Core.Instance.Pretty\n    PlutusIR.Core.Instance.Pretty.Readable\n    PlutusIR.Core.Instance.Scoping\n    PlutusIR.Core.Plated\n    PlutusIR.Core.Type\n    PlutusIR.Error\n    PlutusIR.Mark\n    PlutusIR.MkPir\n    PlutusIR.Parser\n    PlutusIR.Pass\n    PlutusIR.Purity\n    PlutusIR.Strictness\n    PlutusIR.Subst\n    PlutusIR.Transform.Beta\n    PlutusIR.Transform.CaseOfCase\n    PlutusIR.Transform.CaseReduce\n    PlutusIR.Transform.DeadCode\n    PlutusIR.Transform.EvaluateBuiltins\n    PlutusIR.Transform.Inline.CallSiteInline\n    PlutusIR.Transform.Inline.Inline\n    PlutusIR.Transform.Inline.Utils\n    PlutusIR.Transform.KnownCon\n    PlutusIR.Transform.LetFloatIn\n    PlutusIR.Transform.LetFloatOut\n    PlutusIR.Transform.LetMerge\n    PlutusIR.Transform.NonStrict\n    PlutusIR.Transform.RecSplit\n    PlutusIR.Transform.Rename\n    PlutusIR.Transform.RewriteRules\n    PlutusIR.Transform.RewriteRules.CommuteFnWithConst\n    PlutusIR.Transform.RewriteRules.RemoveTrace\n    PlutusIR.Transform.StrictifyBindings\n    PlutusIR.Transform.ThunkRecursions\n    PlutusIR.Transform.Unwrap\n    PlutusIR.TypeCheck\n    PlutusIR.TypeCheck.Internal\n\n  other-modules:\n    PlutusIR.Analysis.Definitions\n    PlutusIR.Analysis.Usages\n    PlutusIR.Compiler.Error\n    PlutusIR.Compiler.Lower\n    PlutusIR.Compiler.Recursion\n    PlutusIR.Normalize\n    PlutusIR.Transform.RewriteRules.Common\n    PlutusIR.Transform.RewriteRules.Internal\n    PlutusIR.Transform.RewriteRules.UnConstrConstrData\n\n  build-depends:\n    , algebraic-graphs     >=0.7\n    , base                 >=4.9       && <5\n    , containers\n    , data-default-class\n    , dlist\n    , dom-lt\n    , extra\n    , flat                 ^>=0.6\n    , hashable\n    , lens\n    , megaparsec\n    , mmorph\n    , monoidal-containers\n    , mtl\n    , multiset\n    , parser-combinators   >=0.4.0\n    , plutus-core          ^>=1.53.1.0\n    , prettyprinter        >=1.1.0.1\n    , profunctors\n    , semigroupoids\n    , semigroups           >=0.19.1\n    , text\n    , transformers\n    , witherable\n\ntest-suite plutus-ir-test\n  import:             lang\n\n  -- needs linux 'diff' available\n  if os(windows)\n    buildable: False\n\n  type:               exitcode-stdio-1.0\n  main-is:            Driver.hs\n  hs-source-dirs:     plutus-ir/test\n  ghc-options:        -threaded -rtsopts -with-rtsopts=-N\n  other-modules:\n    PlutusCore.Generators.QuickCheck.BuiltinsTests\n    PlutusCore.Generators.QuickCheck.SubstitutionTests\n    PlutusCore.Generators.QuickCheck.TypesTests\n    PlutusIR.Analysis.RetainedSize.Tests\n    PlutusIR.Check.Uniques.Tests\n    PlutusIR.Compiler.Datatype.Tests\n    PlutusIR.Compiler.Error.Tests\n    PlutusIR.Compiler.Let.Tests\n    PlutusIR.Compiler.Recursion.Tests\n    PlutusIR.Contexts.Tests\n    PlutusIR.Core.Tests\n    PlutusIR.Generators.QuickCheck.Tests\n    PlutusIR.Parser.Tests\n    PlutusIR.Purity.Tests\n    PlutusIR.Scoping.Tests\n    PlutusIR.Transform.Beta.Tests\n    PlutusIR.Transform.CaseOfCase.Tests\n    PlutusIR.Transform.CaseReduce.Tests\n    PlutusIR.Transform.DeadCode.Tests\n    PlutusIR.Transform.EvaluateBuiltins.Tests\n    PlutusIR.Transform.Inline.Tests\n    PlutusIR.Transform.KnownCon.Tests\n    PlutusIR.Transform.LetFloatIn.Tests\n    PlutusIR.Transform.LetFloatOut.Tests\n    PlutusIR.Transform.NonStrict.Tests\n    PlutusIR.Transform.RecSplit.Tests\n    PlutusIR.Transform.Rename.Tests\n    PlutusIR.Transform.RewriteRules.Tests\n    PlutusIR.Transform.StrictifyBindings.Tests\n    PlutusIR.Transform.StrictLetRec.Tests\n    PlutusIR.Transform.StrictLetRec.Tests.Lib\n    PlutusIR.Transform.ThunkRecursions.Tests\n    PlutusIR.Transform.Unwrap.Tests\n    PlutusIR.TypeCheck.Tests\n\n  build-tool-depends: tasty-discover:tasty-discover\n  build-depends:\n    , base                             >=4.9       && <5\n    , containers\n    , filepath\n    , flat                             ^>=0.6\n    , hashable\n    , hedgehog\n    , lens\n    , mtl\n    , plutus-core                      ^>=1.53.1.0\n    , plutus-core:plutus-core-testlib\n    , plutus-core:plutus-ir\n    , QuickCheck\n    , serialise\n    , tasty\n    , tasty-expected-failure\n    , tasty-hedgehog\n    , tasty-hunit\n    , tasty-quickcheck\n    , text\n    , unordered-containers\n\nexecutable plutus\n  import:             lang\n  main-is:            Main.hs\n  hs-source-dirs:     executables/plutus\n\n  -- Hydra complains that this is not buildable on mingw32 because of brick.\n  -- Strange, because I thought vty added support for windows.\n  if os(windows)\n    buildable: False\n\n  other-modules:\n    AnyProgram.Apply\n    AnyProgram.Bench\n    AnyProgram.Compile\n    AnyProgram.Debug\n    AnyProgram.Example\n    AnyProgram.IO\n    AnyProgram.Parse\n    AnyProgram.Run\n    AnyProgram.With\n    Common\n    Debugger.TUI.Draw\n    Debugger.TUI.Event\n    Debugger.TUI.Main\n    Debugger.TUI.Types\n    GetOpt\n    Mode.Compile\n    Mode.HelpVersion\n    Mode.ListExamples\n    Mode.PrintBuiltins\n    Mode.PrintCostModel\n    Paths_plutus_core\n    Types\n\n  build-depends:\n    , aeson-pretty\n    , base                   >=4.9       && <5\n    , brick\n    , bytestring\n    , containers\n    , exceptions\n    , filepath\n    , flat\n    , lens\n    , megaparsec\n    , microlens\n    , microlens-th           ^>=0.4\n    , mono-traversable\n    , mtl\n    , plutus-core            ^>=1.53.1.0\n    , plutus-core:plutus-ir\n    , prettyprinter\n    , primitive\n    , serialise\n    , singletons\n    , singletons-th\n    , text\n    , text-zipper\n    , vty                    ^>=6.2\n    , vty-crossplatform      ^>=0.4\n\n  ghc-options:        -O2 -threaded -rtsopts -with-rtsopts=-N\n  default-extensions:\n    GADTs\n    TypeApplications\n\n----------------------------------------------\n-- support libs\n----------------------------------------------\n\nlibrary plutus-core-execlib\n  import:          lang\n  visibility:      public\n  hs-source-dirs:  executables/src\n  exposed-modules:\n    PlutusCore.Executable.AstIO\n    PlutusCore.Executable.Common\n    PlutusCore.Executable.Parsers\n    PlutusCore.Executable.Types\n\n  build-depends:\n    , aeson\n    , base                             >=4.9       && <5\n    , bytestring\n    , flat                             ^>=0.6\n    , lens\n    , megaparsec\n    , monoidal-containers\n    , mtl\n    , optparse-applicative\n    , plutus-core                      ^>=1.53.1.0\n    , plutus-core:plutus-core-testlib\n    , plutus-core:plutus-ir\n    , prettyprinter\n    , text\n\n-- could split this up if we split up the main library for UPLC/PLC/PIR\nlibrary plutus-core-testlib\n  import:          lang\n  visibility:      public\n  hs-source-dirs:  testlib\n  exposed-modules:\n    PlutusCore.Generators.Hedgehog\n    PlutusCore.Generators.Hedgehog.AST\n    PlutusCore.Generators.Hedgehog.Builtin\n    PlutusCore.Generators.Hedgehog.Denotation\n    PlutusCore.Generators.Hedgehog.Entity\n    PlutusCore.Generators.Hedgehog.Interesting\n    PlutusCore.Generators.Hedgehog.Test\n    PlutusCore.Generators.Hedgehog.TypedBuiltinGen\n    PlutusCore.Generators.Hedgehog.TypeEvalCheck\n    PlutusCore.Generators.Hedgehog.Utils\n    PlutusCore.Generators.NEAT.Common\n    PlutusCore.Generators.NEAT.Spec\n    PlutusCore.Generators.NEAT.Term\n    PlutusCore.Generators.NEAT.Type\n    PlutusCore.Generators.QuickCheck\n    PlutusCore.Generators.QuickCheck.Builtin\n    PlutusCore.Generators.QuickCheck.Common\n    PlutusCore.Generators.QuickCheck.GenerateKinds\n    PlutusCore.Generators.QuickCheck.GenerateTypes\n    PlutusCore.Generators.QuickCheck.GenTm\n    PlutusCore.Generators.QuickCheck.ShrinkTypes\n    PlutusCore.Generators.QuickCheck.Split\n    PlutusCore.Generators.QuickCheck.Substitutions\n    PlutusCore.Generators.QuickCheck.Unification\n    PlutusCore.Generators.QuickCheck.Utils\n    PlutusCore.Test\n    PlutusIR.Generators.AST\n    PlutusIR.Generators.QuickCheck\n    PlutusIR.Generators.QuickCheck.Common\n    PlutusIR.Generators.QuickCheck.GenerateTerms\n    PlutusIR.Generators.QuickCheck.ShrinkTerms\n    PlutusIR.Pass.Test\n    PlutusIR.Test\n    Test.Tasty.Extras\n    UntypedPlutusCore.Generators.Hedgehog.AST\n    UntypedPlutusCore.Test.DeBruijn.Bad\n    UntypedPlutusCore.Test.DeBruijn.Good\n\n  build-depends:\n    , base                        >=4.9       && <5\n    , bifunctors\n    , bytestring\n    , containers\n    , data-default-class\n    , dependent-map               >=0.4.0.0\n    , filepath\n    , free\n    , hashable\n    , hedgehog                    >=1.0\n    , hedgehog-quickcheck\n    , lazy-search\n    , lens\n    , mmorph\n    , mtl\n    , multiset\n    , plutus-core                 ^>=1.53.1.0\n    , plutus-core:plutus-ir\n    , pretty-simple\n    , prettyprinter               >=1.1.0.1\n    , prettyprinter-configurable\n    , QuickCheck\n    , quickcheck-instances\n    , quickcheck-transformer\n    , size-based\n    , Stream\n    , tagged\n    , tasty\n    , tasty-expected-failure\n    , tasty-golden\n    , tasty-hedgehog\n    , tasty-hunit\n    , text\n    , vector\n\n-- This wraps up the use of the certifier library\n-- so we can present a consistent inteface whether we\n-- are building with it or not. If we aren't building\n-- with it, we present a conservative stub implementation\n-- that just always says everything is fine.\nlibrary plutus-ir-cert\n  import:           lang\n\n  if flag(with-cert)\n    hs-source-dirs: plutus-ir/cert\n    build-depends:  plutus-cert\n\n  else\n    hs-source-dirs: plutus-ir/cert-stub\n\n  default-language: Haskell2010\n  exposed-modules:  PlutusIR.Certifier\n  build-depends:\n    , base\n    , plutus-core            ^>=1.53.1.0\n    , plutus-core:plutus-ir\n\n----------------------------------------------\n-- profiling\n----------------------------------------------\n\nexecutable traceToStacks\n  import:         lang\n  main-is:        Main.hs\n  hs-source-dirs: executables/traceToStacks\n  other-modules:  Common\n  build-depends:\n    , base                  >=4.9 && <5\n    , bytestring\n    , cassava\n    , optparse-applicative\n    , text\n    , vector\n\n-- Tests for functions called by @traceToStacks@.\ntest-suite traceToStacks-test\n  import:           lang\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   executables/traceToStacks\n  default-language: Haskell2010\n  ghc-options:      -threaded -rtsopts -with-rtsopts=-N\n  main-is:          TestGetStacks.hs\n  other-modules:    Common\n  build-depends:\n    , base         >=4.9 && <5\n    , bytestring\n    , cassava\n    , tasty\n    , tasty-hunit\n    , text\n    , vector\n\n----------------------------------------------\n-- cost-model\n----------------------------------------------\n\n-- This runs the microbenchmarks used to generate the cost models for built-in\n-- functions, saving the results in a CSV file which must be specified on the\n-- commmand line.  It will take several hours.\nexecutable cost-model-budgeting-bench\n  import:         lang\n  main-is:        Main.hs\n  other-modules:\n    Benchmarks.Arrays\n    Benchmarks.Bitwise\n    Benchmarks.Bool\n    Benchmarks.ByteStrings\n    Benchmarks.Crypto\n    Benchmarks.Data\n    Benchmarks.Integers\n    Benchmarks.Lists\n    Benchmarks.Misc\n    Benchmarks.Nops\n    Benchmarks.Pairs\n    Benchmarks.Strings\n    Benchmarks.Tracing\n    Benchmarks.Unit\n    Common\n    CriterionExtensions\n    Generators\n\n  hs-source-dirs: cost-model/budgeting-bench\n  build-depends:\n    , base                   >=4.9       && <5\n    , bytestring\n    , cardano-crypto-class\n    , criterion\n    , criterion-measurement\n    , deepseq\n    , directory\n    , filepath\n    , hedgehog\n    , mtl\n    , optparse-applicative\n    , plutus-core            ^>=1.53.1.0\n    , QuickCheck\n    , quickcheck-instances\n    , random\n    , text\n    , time\n    , vector\n\n-- This reads CSV data generated by cost-model-budgeting-bench, uses R to build\n-- the cost models for built-in functions, and saves them in a specified\n-- JSON file (see the help).  The 'official' cost model should be checked in\n-- in plutus-core/cost-model/data/builtinCostModel.json.\nexecutable generate-cost-model\n  import:         lang\n  main-is:        Main.hs\n  hs-source-dirs: cost-model/create-cost-model\n  ghc-options:    -threaded -rtsopts -with-rtsopts=-N\n\n  if !flag(with-inline-r)\n    buildable: False\n\n  -- This fails on Darwin with strange errors and I don't know why\n  -- > Error: C stack usage  17556409549320 is too close to the limit\n  -- > Fatal error: unable to initialize the JI\n  if os(osx)\n    buildable: False\n\n  -- Can't build on windows as it depends on R.\n  if os(windows)\n    buildable: False\n\n  build-depends:\n    , aeson-pretty\n    , barbies\n    , base                  >=4.9       && <5\n    , bytestring\n    , directory\n    , inline-r              >=1.0.1\n    , optparse-applicative\n    , plutus-core           ^>=1.53.1.0\n    , text\n\n  --    , exceptions\n  other-modules:\n    BuiltinMemoryModels\n    CreateBuiltinCostModel\n\n-- The cost models for builtins are generated using R and converted into a JSON\n-- form that can later be used to construct Haskell functions.  This tests that\n-- the predictions of the Haskell version are (approximately) identical to the R\n-- ones. This test is problematic in CI: pretending that it's a benchmark will\n-- prevent it from being run automatically but will still allow us to run it\n-- manually; `cabal bench` also sets the working directory to the root of the\n-- relevant package, which makes it easier to find the cost model data files\n-- (unlike `cabal run` for executables, which sets the working directory to the\n-- current shell directory).\nbenchmark cost-model-test\n  import:         lang\n  type:           exitcode-stdio-1.0\n  main-is:        TestCostModels.hs\n  other-modules:  TH\n  hs-source-dirs: cost-model/test cost-model/create-cost-model\n\n  if !flag(with-inline-r)\n    buildable: False\n\n  -- This fails on Darwin with strange errors and I don't know why\n  -- > Error: C stack usage  17556409549320 is too close to the limit\n  -- > Fatal error: unable to initialize the JI\n  if os(osx)\n    buildable: False\n\n  -- Can't build on windows as it depends on R.\n  if os(windows)\n    buildable: False\n\n  build-depends:\n    , barbies\n    , base              >=4.9       && <5\n    , bytestring\n    , hedgehog\n    , inline-r          >=1.0.1\n    , mmorph\n    , plutus-core       ^>=1.53.1.0\n    , template-haskell\n    , text\n\n  other-modules:\n    BuiltinMemoryModels\n    CreateBuiltinCostModel\n\nexecutable print-cost-model\n  import:         lang\n  main-is:        Main.hs\n  hs-source-dirs: cost-model/print-cost-model\n  other-modules:  Paths_plutus_core\n  build-depends:\n    , aeson\n    , base         >=4.9       && <5\n    , bytestring\n    , plutus-core  ^>=1.53.1.0\n\n----------------------------------------------\n-- satint\n----------------------------------------------\n\nlibrary satint\n  import:          lang\n  exposed-modules: Data.SatInt\n  hs-source-dirs:  satint/src\n  build-depends:\n    , aeson\n    , base              >=4.9 && <5\n    , cassava\n    , deepseq\n    , nothunks\n    , primitive\n    , serialise\n    , template-haskell\n\ntest-suite satint-test\n  import:           lang\n  type:             exitcode-stdio-1.0\n  main-is:          TestSatInt.hs\n  build-depends:\n    , base                        >=4.9 && <5\n    , HUnit\n    , QuickCheck\n    , satint\n    , test-framework\n    , test-framework-hunit\n    , test-framework-quickcheck2\n\n  default-language: Haskell2010\n  hs-source-dirs:   satint/test\n\n----------------------------------------------\n-- index-envs\n----------------------------------------------\n\nlibrary index-envs\n  import:           lang\n  visibility:       public\n  hs-source-dirs:   index-envs/src\n  default-language: Haskell2010\n  exposed-modules:\n    Data.RandomAccessList.Class\n    Data.RandomAccessList.RelativizedMap\n    Data.RandomAccessList.SkewBinary\n    Data.RandomAccessList.SkewBinarySlab\n\n  build-depends:\n    , base             >=4.9  && <5\n    , containers\n    , extra\n    , nonempty-vector\n    , ral              ^>=0.2\n\n-- broken for ral-0.2 conflicts with cardano-binary:recursion-schemes\nbenchmark index-envs-bench\n  import:           lang\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   index-envs/bench\n  default-language: Haskell2010\n  main-is:          Main.hs\n  build-depends:\n    , base             >=4.9     && <5\n    , criterion        >=1.5.9.0\n    , index-envs\n    , nonempty-vector\n    , ral              ^>=0.2\n    , random           >=1.2.0\n\n-- broken for ral-0.2 conflicts with cardano-binary:recursion-schemes\ntest-suite index-envs-test\n  import:           lang\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   index-envs/test\n  default-language: Haskell2010\n  main-is:          Spec.hs\n  other-modules:    RAList.Spec\n  ghc-options:      -threaded -rtsopts -with-rtsopts=-N\n  build-depends:\n    , base                  >=4.9 && <5\n    , index-envs\n    , nonempty-vector\n    , QuickCheck\n    , quickcheck-instances\n    , tasty\n    , tasty-quickcheck\n";
  }