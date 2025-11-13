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
      specVersion = "2.2";
      identifier = { name = "fin"; version = "0.3.2"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2017-2021 Oleg Grenrus";
      maintainer = "Oleg.Grenrus <oleg.grenrus@iki.fi>";
      author = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      homepage = "https://github.com/phadej/vec";
      url = "";
      synopsis = "Nat and Fin: peano naturals and finite numbers";
      description = "This package provides two simple types, and some tools to work with them.\nAlso on type level as @DataKinds@.\n\n@\n\\-- Peano naturals\ndata Nat = Z | S Nat\n\n\\-- Finite naturals\ndata Fin (n :: Nat) where\n\\    Z :: Fin ('S n)\n\\    S :: Fin n -> Fin ('Nat.S n)\n@\n\n[vec](https://hackage.haskell.org/package/vec) implements length-indexed\n(sized) lists using this package for indexes.\n\nThe \"Data.Fin.Enum\" module let's work generically with enumerations.\n\nSee [Hasochism: the pleasure and pain of dependently typed haskell programming](https://doi.org/10.1145/2503778.2503786)\nby Sam Lindley and Conor McBride for answers to /how/ and /why/.\nRead [APLicative Programming with Naperian Functors](https://doi.org/10.1007/978-3-662-54434-1_21)\nby Jeremy Gibbons for (not so) different ones.\n\n=== Similar packages\n\n* [finite-typelits](https://hackage.haskell.org/package/finite-typelits)\n. Is a great package, but uses @GHC.TypeLits@.\n\n* [type-natural](https://hackage.haskell.org/package/type-natural) depends\non @singletons@ package. @fin@ will try to stay light on the dependencies,\nand support as many GHC versions as practical.\n\n* [peano](https://hackage.haskell.org/package/peano) is very incomplete\n\n* [nat](https://hackage.haskell.org/package/nat) as well.\n\n* [PeanoWitnesses](https://hackage.haskell.org/package/PeanoWitnesses)\ndoesn't use @DataKinds@.\n\n* [type-combinators](https://hackage.haskell.org/package/type-combinators)\nis big package too.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."boring" or (errorHandler.buildDepError "boring"))
          (hsPkgs."dec" or (errorHandler.buildDepError "dec"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."some" or (errorHandler.buildDepError "some"))
          (hsPkgs."universe-base" or (errorHandler.buildDepError "universe-base"))
        ];
        buildable = true;
      };
      tests = {
        "inspection" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."fin" or (errorHandler.buildDepError "fin"))
            (hsPkgs."inspection-testing" or (errorHandler.buildDepError "inspection-testing"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          ];
          buildable = if !(compiler.isGhc && compiler.version.ge "8.0")
            then false
            else true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/fin-0.3.2.tar.gz";
      sha256 = "b92559549f4f03ee1acbe6aa93f13ad856213d37b887e603502b4fe159162141";
    });
  }) // {
    package-description-override = "cabal-version:      2.2\nname:               fin\nversion:            0.3.2\nx-revision:         1\nsynopsis:           Nat and Fin: peano naturals and finite numbers\ncategory:           Data, Dependent Types, Singletons, Math\ndescription:\n  This package provides two simple types, and some tools to work with them.\n  Also on type level as @DataKinds@.\n  .\n  @\n  \\-- Peano naturals\n  data Nat = Z | S Nat\n  .\n  \\-- Finite naturals\n  data Fin (n :: Nat) where\n  \\    Z :: Fin ('S n)\n  \\    S :: Fin n -> Fin ('Nat.S n)\n  @\n  .\n  [vec](https://hackage.haskell.org/package/vec) implements length-indexed\n  (sized) lists using this package for indexes.\n  .\n  The \"Data.Fin.Enum\" module let's work generically with enumerations.\n  .\n  See [Hasochism: the pleasure and pain of dependently typed haskell programming](https://doi.org/10.1145/2503778.2503786)\n  by Sam Lindley and Conor McBride for answers to /how/ and /why/.\n  Read [APLicative Programming with Naperian Functors](https://doi.org/10.1007/978-3-662-54434-1_21)\n  by Jeremy Gibbons for (not so) different ones.\n  .\n  === Similar packages\n  .\n  * [finite-typelits](https://hackage.haskell.org/package/finite-typelits)\n  . Is a great package, but uses @GHC.TypeLits@.\n  .\n  * [type-natural](https://hackage.haskell.org/package/type-natural) depends\n  on @singletons@ package. @fin@ will try to stay light on the dependencies,\n  and support as many GHC versions as practical.\n  .\n  * [peano](https://hackage.haskell.org/package/peano) is very incomplete\n  .\n  * [nat](https://hackage.haskell.org/package/nat) as well.\n  .\n  * [PeanoWitnesses](https://hackage.haskell.org/package/PeanoWitnesses)\n  doesn't use @DataKinds@.\n  .\n  * [type-combinators](https://hackage.haskell.org/package/type-combinators)\n  is big package too.\n\nhomepage:           https://github.com/phadej/vec\nbug-reports:        https://github.com/phadej/vec/issues\nlicense:            BSD-3-Clause\nlicense-file:       LICENSE\nauthor:             Oleg Grenrus <oleg.grenrus@iki.fi>\nmaintainer:         Oleg.Grenrus <oleg.grenrus@iki.fi>\ncopyright:          (c) 2017-2021 Oleg Grenrus\nbuild-type:         Simple\nextra-source-files: ChangeLog.md\ntested-with:\n  GHC ==8.6.5\n   || ==8.8.4\n   || ==8.10.7\n   || ==9.0.2\n   || ==9.2.8\n   || ==9.4.8\n   || ==9.6.6\n   || ==9.8.4\n   || ==9.10.1\n   || ==9.12.2\n\nsource-repository head\n  type:     git\n  location: https://github.com/phadej/vec.git\n  subdir:   fin\n\nlibrary\n  default-language: Haskell2010\n  ghc-options:      -Wall -fprint-explicit-kinds\n  hs-source-dirs:   src\n  exposed-modules:\n    Data.Fin\n    Data.Nat\n    Data.Type.Nat\n    Data.Type.Nat.LE\n    Data.Type.Nat.LE.ReflStep\n    Data.Type.Nat.LT\n\n  other-modules:    TrustworthyCompat\n\n  -- GHC boot libs\n  build-depends:\n    , base     >=4.12.0.0 && <4.22\n    , deepseq  >=1.4.4.0  && <1.6\n\n  -- other dependencies\n  build-depends:\n    , boring         ^>=0.2.2\n    , dec            ^>=0.0.6\n    , hashable       ^>=1.4.4.0 || ^>=1.5.0.0\n    , QuickCheck     ^>=2.14.2  || ^>=2.15\n    , some           ^>=1.0.6\n    , universe-base  ^>=1.1.4\n\n  if impl(ghc >=9.0)\n    -- these flags may abort compilation with GHC-8.10\n    -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3295\n    ghc-options: -Winferred-safe-imports -Wmissing-safe-haskell-mode\n\n-- dump-core\n--  build-depends: dump-core\n--  ghc-options: -fplugin=DumpCore -fplugin-opt DumpCore:core-html\n\ntest-suite inspection\n  type:             exitcode-stdio-1.0\n  main-is:          Inspection.hs\n  ghc-options:      -Wall -fprint-explicit-kinds\n  hs-source-dirs:   test\n  default-language: Haskell2010\n  build-depends:\n    , base\n    , fin\n    , inspection-testing  >=0.2.0.1 && <0.7\n    , tagged\n\n  if !impl(ghc >=8.0)\n    buildable: False\n";
  }