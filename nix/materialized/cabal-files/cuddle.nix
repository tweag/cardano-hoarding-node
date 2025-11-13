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
    flags = { example = false; };
    package = {
      specVersion = "3.4";
      identifier = { name = "cuddle"; version = "1.0.0.0"; };
      license = "Apache-2.0";
      copyright = "2025 Input Output Global Inc (IOG)";
      maintainer = "hackage@iohk.io";
      author = "IOG Ledger Team";
      homepage = "";
      url = "";
      synopsis = "CDDL Generator and test utilities";
      description = "Cuddle is a library for generating and manipulating [CDDL](https://datatracker.ietf.org/doc/html/rfc8610).\n\nIncluded in this package is a command line tool for working with CDDL files. It\ncurrently supports four functions:\n\n- Formatting of CDDL files\n- Validating that a CDDL file is legal\n- Generating random CBOR terms matching CDDL productions\n- Testing compliance of a CBOR file against a CDDL spec.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."boxes" or (errorHandler.buildDepError "boxes"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."capability" or (errorHandler.buildDepError "capability"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."foldable1-classes-compat" or (errorHandler.buildDepError "foldable1-classes-compat"))
          (hsPkgs."generic-optics" or (errorHandler.buildDepError "generic-optics"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."mutable-containers" or (errorHandler.buildDepError "mutable-containers"))
          (hsPkgs."optics-core" or (errorHandler.buildDepError "optics-core"))
          (hsPkgs."ordered-containers" or (errorHandler.buildDepError "ordered-containers"))
          (hsPkgs."parser-combinators" or (errorHandler.buildDepError "parser-combinators"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."regex-tdfa" or (errorHandler.buildDepError "regex-tdfa"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
        ];
        buildable = true;
      };
      exes = {
        "example" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cuddle" or (errorHandler.buildDepError "cuddle"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
          buildable = if flags.example then true else false;
        };
        "cuddle" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."cuddle" or (errorHandler.buildDepError "cuddle"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
          buildable = true;
        };
      };
      tests = {
        "cuddle-test" = {
          depends = [
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cuddle" or (errorHandler.buildDepError "cuddle"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-megaparsec" or (errorHandler.buildDepError "hspec-megaparsec"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."string-qq" or (errorHandler.buildDepError "string-qq"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/cuddle-1.0.0.0.tar.gz";
      sha256 = "8d596ba4155531cc9af705c30d1b32c5dfb7e7bf066e3b9a5fa8d61c6ab1a813";
    });
  }) // {
    package-description-override = "cabal-version: 3.4\nname: cuddle\nversion: 1.0.0.0\nsynopsis: CDDL Generator and test utilities\ndescription:\n  Cuddle is a library for generating and manipulating [CDDL](https://datatracker.ietf.org/doc/html/rfc8610).\n\n  Included in this package is a command line tool for working with CDDL files. It\n  currently supports four functions:\n\n  - Formatting of CDDL files\n  - Validating that a CDDL file is legal\n  - Generating random CBOR terms matching CDDL productions\n  - Testing compliance of a CBOR file against a CDDL spec.\n\nlicense: Apache-2.0\nlicense-file: LICENSE\nauthor: IOG Ledger Team\nmaintainer: hackage@iohk.io\ncopyright: 2025 Input Output Global Inc (IOG)\ncategory: Codec\nbuild-type: Simple\nextra-doc-files: CHANGELOG.md\ntested-with: ghc =={9.6, 9.8, 9.10, 9.12}\n\nsource-repository head\n  type: git\n  location: https://github.com/input-output-hk/cuddle\n\nsource-repository this\n  type: git\n  location: https://github.com/input-output-hk/cuddle\n  tag: cuddle-0.5.0.0\n\nflag example\n  description: Enable the example executable\n  manual: True\n  default: False\n\ncommon warnings\n  ghc-options:\n    -Wall\n\nlibrary\n  import: warnings\n  exposed-modules:\n    Codec.CBOR.Cuddle.CBOR.Gen\n    Codec.CBOR.Cuddle.CBOR.Validator\n    Codec.CBOR.Cuddle.CDDL\n    Codec.CBOR.Cuddle.CDDL.CTree\n    Codec.CBOR.Cuddle.CDDL.CtlOp\n    Codec.CBOR.Cuddle.CDDL.Postlude\n    Codec.CBOR.Cuddle.CDDL.Prelude\n    Codec.CBOR.Cuddle.CDDL.Resolve\n    Codec.CBOR.Cuddle.Comments\n    Codec.CBOR.Cuddle.Huddle\n    Codec.CBOR.Cuddle.Huddle.HuddleM\n    Codec.CBOR.Cuddle.Huddle.Optics\n    Codec.CBOR.Cuddle.Parser\n    Codec.CBOR.Cuddle.Parser.Lexer\n    Codec.CBOR.Cuddle.Pretty\n    Codec.CBOR.Cuddle.Pretty.Columnar\n    Codec.CBOR.Cuddle.Pretty.Utils\n\n  build-depends:\n    base >=4.18 && <5,\n    base16-bytestring >=1.0.2,\n    boxes >=0.1.5,\n    bytestring >=0.11.4,\n    capability >=0.5,\n    cborg >=0.2.10,\n    containers >=0.6.7,\n    data-default-class >=0.2,\n    foldable1-classes-compat >=0.1.1,\n    generic-optics >=2.2.1,\n    hashable >=1.4,\n    megaparsec >=9.5,\n    mtl >=2.3.1,\n    mutable-containers >=0.3.4,\n    optics-core >=0.4.1,\n    ordered-containers >=0.2.4,\n    parser-combinators >=1.3,\n    prettyprinter >=1.7.1,\n    random >=1.2,\n    regex-tdfa >=1.3.2,\n    scientific >=0.3.7,\n    text >=2.0.2,\n    tree-diff >=0.3,\n\n  hs-source-dirs: src\n  default-language: GHC2021\n\nexecutable example\n  import: warnings\n\n  if flag(example)\n    buildable: True\n  else\n    buildable: False\n\n  default-language: GHC2021\n  other-modules:\n    Conway\n    Monad\n\n  hs-source-dirs: example\n  main-is: Main.hs\n  build-depends:\n    base,\n    cuddle,\n    megaparsec,\n    prettyprinter,\n    random,\n    text,\n\nexecutable cuddle\n  import: warnings\n  default-language: GHC2021\n  hs-source-dirs: ./bin/\n  main-is: Main.hs\n  build-depends:\n    base,\n    base16-bytestring,\n    bytestring,\n    cborg,\n    cuddle,\n    megaparsec,\n    mtl,\n    optparse-applicative >=0.18,\n    prettyprinter,\n    random,\n    text,\n\ntest-suite cuddle-test\n  import: warnings\n  default-language: GHC2021\n  other-modules:\n    Test.Codec.CBOR.Cuddle.CDDL.Examples\n    Test.Codec.CBOR.Cuddle.CDDL.Gen\n    Test.Codec.CBOR.Cuddle.CDDL.Parser\n    Test.Codec.CBOR.Cuddle.CDDL.Pretty\n    Test.Codec.CBOR.Cuddle.Huddle\n\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test\n  main-is: Main.hs\n  build-depends:\n    HUnit >=1.6.2,\n    QuickCheck >=2.14,\n    base,\n    bytestring,\n    cuddle,\n    data-default-class,\n    hspec >=2.11,\n    hspec-megaparsec >=2.2,\n    megaparsec,\n    prettyprinter,\n    string-qq >=0.0.6,\n    text,\n    tree-diff,\n";
  }