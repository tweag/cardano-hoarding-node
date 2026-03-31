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
    flags = { pkg-config = true; };
    package = {
      specVersion = "2.4";
      identifier = { name = "digest"; version = "0.0.2.1.0.0.0.1"; };
      license = "BSD-2-Clause";
      copyright = "(c) 2009 Eugene Kirpichov";
      maintainer = "Eugene Kirpichov <ekirpichov@gmail.com>";
      author = "Eugene Kirpichov <ekirpichov@gmail.com>";
      homepage = "";
      url = "";
      synopsis = "CRC32 and Adler32 hashes for bytestrings";
      description = "This package provides efficient hash implementations for\nstrict and lazy bytestrings. For now, CRC32 and Adler32 are supported;\nthey are implemented as FFI bindings to efficient code from zlib.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
        ] ++ pkgs.lib.optional (compiler.isGhcjs && true || system.isGhcjs || system.isWasm32 || !flags.pkg-config && system.isWindows) (hsPkgs."zlib-clib" or (errorHandler.buildDepError "zlib-clib"));
        libs = pkgs.lib.optionals (!(compiler.isGhcjs && true || system.isGhcjs || system.isWasm32 || !flags.pkg-config && system.isWindows)) (pkgs.lib.optional (!flags.pkg-config) (pkgs."z" or (errorHandler.sysDepError "z")));
        pkgconfig = pkgs.lib.optionals (!(compiler.isGhcjs && true || system.isGhcjs || system.isWasm32 || !flags.pkg-config && system.isWindows)) (pkgs.lib.optional (flags.pkg-config) (pkgconfPkgs."zlib" or (errorHandler.pkgConfDepError "zlib")));
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "https://chap.intersectmbo.org/package/digest-0.0.2.1.0.0.0.1.tar.gz";
      sha256 = "11faa5e05c68e80543f7318331e8f0381dae4aba372c63971ad0a6017007667a";
    });
  }) // {
    package-description-override = "cabal-version:      2.4\nname:               digest\nversion:            0.0.2.1.0.0.0.1\ncopyright:          (c) 2009 Eugene Kirpichov\nlicense:            BSD-2-Clause\nlicense-file:       LICENSE\nauthor:             Eugene Kirpichov <ekirpichov@gmail.com>\nmaintainer:         Eugene Kirpichov <ekirpichov@gmail.com>\ncategory:           Cryptography\nsynopsis:           CRC32 and Adler32 hashes for bytestrings\ndescription:\n  This package provides efficient hash implementations for\n  strict and lazy bytestrings. For now, CRC32 and Adler32 are supported;\n  they are implemented as FFI bindings to efficient code from zlib.\n\nstability:          provisional\nbuild-type:         Simple\ntested-with:        GHC ==8.10.7 || ==9.0.2 || ==9.2.8 || ==9.4.8 || ==9.6.3 || ==9.8.1\nextra-source-files:\n  CHANGELOG.md\n  testing/trivial-reference.c\n  testing/trivial.expected\n  testing/trivial.hs\n\nflag pkg-config\n  default:     True\n  manual:      False\n  description: Use @pkg-config(1)@ to locate @zlib@ library.\n\nsource-repository head\n  type:     git\n  location: https://github.com/TeofilC/digest\n\nlibrary\n  exposed-modules:\n    Data.Digest.Adler32\n    Data.Digest.CRC32\n\n  default-extensions:\n    CPP\n    ForeignFunctionInterface\n    PackageImports\n\n  default-language:   Haskell2010\n  build-depends:\n    , base        >=4.12 && <5\n    , bytestring  >=0.10 && <0.13\n\n  includes:           zlib.h\n\n  ghc-options:        -Wall\n\n  if impl(ghcjs) || os(ghcjs) || arch(wasm32) || (!flag(pkg-config) && os(windows))\n    build-depends: zlib-clib < 2\n  else\n    if flag(pkg-config)\n      pkgconfig-depends: zlib\n    else\n      extra-libraries: z\n";
  }