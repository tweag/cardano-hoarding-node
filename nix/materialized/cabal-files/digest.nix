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
    flags = {
      pkg-config = true;
      have_builtin_prefetch = false;
      have_mm_prefetch = false;
      have_sse42 = false;
      have_arm64_crc32c = false;
      have_strong_getauxval = false;
      have_weak_getauxval = false;
    };
    package = {
      specVersion = "2.4";
      identifier = { name = "digest"; version = "0.0.2.1"; };
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
        ] ++ pkgs.lib.optional (!(flags.pkg-config && !system.isWindows && !system.isFreebsd)) (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"));
        pkgconfig = pkgs.lib.optional (flags.pkg-config && !system.isWindows && !system.isFreebsd) (pkgconfPkgs."zlib" or (errorHandler.pkgConfDepError "zlib"));
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/digest-0.0.2.1.tar.gz";
      sha256 = "cbf87b02e01f97dfe6d2d7f08065826cf552c8e8467ea990ed0d2f502e0a6498";
    });
  }) // {
    package-description-override = "cabal-version:      2.4\nname:               digest\nversion:            0.0.2.1\ncopyright:          (c) 2009 Eugene Kirpichov\nlicense:            BSD-2-Clause\nlicense-file:       LICENSE\nauthor:             Eugene Kirpichov <ekirpichov@gmail.com>\nmaintainer:         Eugene Kirpichov <ekirpichov@gmail.com>\ncategory:           Cryptography\nsynopsis:           CRC32 and Adler32 hashes for bytestrings\ndescription:\n  This package provides efficient hash implementations for\n  strict and lazy bytestrings. For now, CRC32 and Adler32 are supported;\n  they are implemented as FFI bindings to efficient code from zlib.\n\nstability:          provisional\nbuild-type:         Simple\ntested-with:        GHC ==8.10.7 || ==9.0.2 || ==9.2.8 || ==9.4.8 || ==9.6.3 || ==9.8.1\nextra-source-files:\n  CHANGELOG.md\n  external/crc32c/include/crc32c/crc32c.h\n  external/crc32c/LICENSE\n  external/crc32c/src/*.h\n  include/crc32c/crc32c_config.h\n  testing/trivial-reference.c\n  testing/trivial.expected\n  testing/trivial.hs\n\nflag pkg-config\n  default:     True\n  manual:      False\n  description: Use @pkg-config(1)@ to locate @zlib@ library.\n\n-- TODO: auto detect\nflag have_builtin_prefetch\n  default:     False\n  manual:      True\n  description: The cxx compiler has the __builtin_prefetch intrinsic.\n\n-- TODO: auto detect\nflag have_mm_prefetch\n  default:     False\n  manual:      True\n  description:\n    Targeting X86 and the compiler has the _mm_prefetch intrinsic.\n\n-- TODO: auto detect\nflag have_sse42\n  default:     False\n  manual:      True\n  description:\n    Can be enabled to improve performance of CRC32C if targeting X86 and\n    the compiler has the _mm_crc32_u{8,32,64} intrinsics.\n\n-- TODO: auto detect\nflag have_arm64_crc32c\n  default:     False\n  manual:      True\n  description:\n    Targeting ARM and the compiler has the __crc32c{b,h,w,d} and the\n    vmull_p64 intrinsics.\n\n-- TODO: auto detect\nflag have_strong_getauxval\n  default:     False\n  manual:      True\n  description:\n    The system libraries have the getauxval function in the <sys/auxv.h> header.\n    Should be true on Linux and Android API level 20+.\n\n-- TODO: auto detect\nflag have_weak_getauxval\n  default:     False\n  manual:      True\n  description:\n    The compiler supports defining getauxval as a weak symbol.\n    Should be true for any compiler that supports __attribute__((weak)).\n\nsource-repository head\n  type:     git\n  location: https://github.com/TeofilC/digest\n\nlibrary\n  exposed-modules:\n    Data.Digest.Adler32\n    Data.Digest.CRC32\n    Data.Digest.CRC32C\n\n  default-extensions:\n    CPP\n    ForeignFunctionInterface\n\n  default-language:   Haskell2010\n  build-depends:\n    , base        >=4.12 && <5\n    , bytestring  >=0.10 && <0.13\n\n  includes:           zlib.h\n  include-dirs:       include external/crc32c/include\n  cxx-options:        -std=c++11\n  cxx-sources:\n    external/crc32c/src/crc32c.cc\n    external/crc32c/src/crc32c_portable.cc\n\n  if flag(have_builtin_prefetch)\n    cxx-options: -DHAVE_BUILTIN_PREFETCH\n\n  if flag(have_mm_prefetch)\n    cxx-options: -DHAVE_MM_PREFETCH\n\n  if (arch(x86_64) && flag(have_sse42))\n    cxx-options: -DHAVE_SSE42 -msse4.2\n    cxx-sources: external/crc32c/src/crc32c_sse42.cc\n\n  if (arch(aarch64) && flag(have_arm64_crc32c))\n    cxx-options: -DHAVE_ARM64_CRC32C\n    cxx-sources: external/crc32c/src/crc32c_arm64.cc\n\n  if flag(have_strong_getauxval)\n    cxx-options: -DHAVE_STRONG_GETAUXVAL\n\n  if flag(have_weak_getauxval)\n    cxx-options: -DHAVE_WEAK_GETAUXVAL\n\n  ghc-options:        -Wall\n\n  if ((flag(pkg-config) && !os(windows)) && !os(freebsd))\n    pkgconfig-depends: zlib\n\n  else\n    build-depends: zlib\n";
  }