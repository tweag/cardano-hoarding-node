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
    flags = { developer = false; embedded_double_conversion = true; };
    package = {
      specVersion = "2.2";
      identifier = { name = "double-conversion"; version = "2.0.5.0"; };
      license = "BSD-2-Clause";
      copyright = "";
      maintainer = "Bryan O'Sullivan <bos@serpentine.com>";
      author = "Bryan O'Sullivan <bos@serpentine.com>";
      homepage = "https://github.com/haskell/double-conversion";
      url = "";
      synopsis = "Fast conversion between single and double precision floating point and text";
      description = "A library that performs fast, accurate conversion between\nfloating point and text.\n\nThis library is implemented as bindings to the C++\n@double-conversion@ library written by Florian Loitsch at Google:\n<https://github.com/floitsch/double-conversion>.\n\nNow it can convert single precision numbers, and also it can create\nBuilder, instead of bytestring or text.\n\nThe 'Text' versions of these functions are about 30 times faster\nthan the default 'show' implementation for the 'Double' type.\n\nThe 'ByteString' versions are have very close speed to the 'Text' versions;\n\nBuilder versions (both for Text and Bytestring) are slower on single value,\nbut they are much faster on large number of values\n(up to 20x faster on list with 20000 doubles).\n\nAs a final note, be aware that the @bytestring-show@ package is\nabout 50% slower than simply using 'show'.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
        ] ++ pkgs.lib.optional (compiler.isGhc && compiler.version.ge "9.4") (hsPkgs."system-cxx-std-lib" or (errorHandler.buildDepError "system-cxx-std-lib"));
        libs = pkgs.lib.optionals (!(compiler.isGhc && compiler.version.ge "9.4")) (if system.isOsx || system.isFreebsd
          then [ (pkgs."c++" or (errorHandler.sysDepError "c++")) ]
          else if system.isWindows
            then if system.isX86_64 && (compiler.isGhc && compiler.version.lt "8.6.5")
              then [
                (pkgs."stdc++-6" or (errorHandler.sysDepError "stdc++-6"))
                (pkgs."gcc_s_seh-1" or (errorHandler.sysDepError "gcc_s_seh-1"))
              ]
              else if system.isX86_64
                then [
                  (pkgs."stdc++" or (errorHandler.sysDepError "stdc++"))
                  (pkgs."gcc_s_seh-1" or (errorHandler.sysDepError "gcc_s_seh-1"))
                ]
                else if compiler.isGhc && compiler.version.ge "8.6.5"
                  then [
                    (pkgs."stdc++" or (errorHandler.sysDepError "stdc++"))
                    (pkgs."gcc_s_dw2-1" or (errorHandler.sysDepError "gcc_s_dw2-1"))
                  ]
                  else [
                    (pkgs."stdc++-6" or (errorHandler.sysDepError "stdc++-6"))
                    (pkgs."gcc_s_dw2-1" or (errorHandler.sysDepError "gcc_s_dw2-1"))
                  ]
            else [
              (pkgs."stdc++" or (errorHandler.sysDepError "stdc++"))
            ]) ++ pkgs.lib.optional (!flags.embedded_double_conversion) (pkgs."double-conversion" or (errorHandler.sysDepError "double-conversion"));
        buildable = true;
      };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."double-conversion" or (errorHandler.buildDepError "double-conversion"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/double-conversion-2.0.5.0.tar.gz";
      sha256 = "98c699b6e47b257dff85d49d59e39858462598008e074460c8bfacaa3e2a43ba";
    });
  }) // {
    package-description-override = "cabal-version:  2.2\nname:           double-conversion\nversion:        2.0.5.0\nlicense:        BSD-2-Clause\nlicense-file:   LICENSE\nhomepage:       https://github.com/haskell/double-conversion\nbug-reports:    https://github.com/haskell/double-conversion/issues\ncategory:       Text\nauthor:         Bryan O'Sullivan <bos@serpentine.com>\nmaintainer:     Bryan O'Sullivan <bos@serpentine.com>\nstability:      experimental\nsynopsis:       Fast conversion between single and double precision floating point and text\nbuild-type:     Simple\ntested-with:    GHC ==9.0.2 || ==9.2.8 || ==9.4.7 || ==9.6.2\ndescription:\n    A library that performs fast, accurate conversion between \n    floating point and text.\n    .\n    This library is implemented as bindings to the C++\n    @double-conversion@ library written by Florian Loitsch at Google:\n    <https://github.com/floitsch/double-conversion>.\n    .\n    Now it can convert single precision numbers, and also it can create \n    Builder, instead of bytestring or text. \n    .\n    The 'Text' versions of these functions are about 30 times faster\n    than the default 'show' implementation for the 'Double' type.\n    .\n    The 'ByteString' versions are have very close speed to the 'Text' versions;\n    .\n    Builder versions (both for Text and Bytestring) are slower on single value,\n    but they are much faster on large number of values \n    (up to 20x faster on list with 20000 doubles).\n    .\n    As a final note, be aware that the @bytestring-show@ package is\n    about 50% slower than simply using 'show'.\n\nextra-source-files:\n    README.markdown\n    benchmarks/*.cabal\n    benchmarks/*.hs\n    double-conversion/*.cmake.in\n    double-conversion/AUTHORS\n    double-conversion/CMakeLists.txt\n    double-conversion/COPYING\n    double-conversion/Changelog\n    double-conversion/LICENSE\n    double-conversion/Makefile\n    double-conversion/README\n    double-conversion/SConstruct\n    double-conversion/src/*.cc\n    double-conversion/src/*.h\n    double-conversion/src/CMakeLists.txt\n    double-conversion/src/SConscript\n    double-conversion/test/CMakeLists.txt\n    double-conversion/test/cctest/*.cc\n    double-conversion/test/cctest/*.h\n    double-conversion/test/cctest/CMakeLists.txt\n    double-conversion/test/cctest/SConscript\n    include/*.h\n    tests/*.hs\n\nflag developer\n  description: operate in developer mode\n  default: False\n  manual: True\n\nflag embedded_double_conversion\n  description: embed the C++ double_conversion library\n  default: True\n\nlibrary\n  if impl(ghc >= 9.4)\n    build-depends: system-cxx-std-lib == 1.0\n\n  elif os(darwin) || os(freebsd)\n    extra-libraries: c++\n  elif os(windows)\n    if arch(x86_64) && impl(ghc < 8.6.5)\n      extra-libraries: stdc++-6 gcc_s_seh-1\n    elif arch(x86_64)\n      extra-libraries: stdc++ gcc_s_seh-1\n    elif impl(ghc >= 8.6.5)\n      extra-libraries: stdc++ gcc_s_dw2-1\n    else \n      extra-libraries: stdc++-6 gcc_s_dw2-1\n  else\n    extra-libraries: stdc++\n\n  if flag(embedded_double_conversion)\n    cxx-sources:\n      cbits/hs-double-conversion-embed.cc\n\n      double-conversion/src/bignum.cc\n      double-conversion/src/bignum-dtoa.cc\n      double-conversion/src/cached-powers.cc\n      double-conversion/src/diy-fp.cc\n      double-conversion/src/double-conversion.cc\n      double-conversion/src/fast-dtoa.cc\n      double-conversion/src/fixed-dtoa.cc\n      double-conversion/src/strtod.cc\n    include-dirs: double-conversion/src\n  else\n    extra-libraries: double-conversion\n    cxx-sources:\n      cbits/hs-double-conversion.cc\n\n  include-dirs:\n    include\n\n  exposed-modules:\n    Data.Double.Conversion.Convertable\n    Data.Double.Conversion.ByteString\n    Data.Double.Conversion.Text\n\n  other-modules:\n    Data.Double.Conversion.Internal.FFI\n    Data.Double.Conversion.Internal.ByteString\n    Data.Double.Conversion.Internal.ByteStringBuilder\n    Data.Double.Conversion.Internal.Text\n    Data.Double.Conversion.Internal.TextBuilder\n\n  build-depends:\n    base == 4.*,\n    bytestring,\n    ghc-prim,\n    text >= 0.11.0.8\n\n  if flag(developer)\n    ghc-options: -Werror \n    ghc-prof-options: -auto-all\n  else\n    cc-options: -DNDEBUG \n\n  ghc-options: -Wall \n\n  default-language: Haskell2010\n\ntest-suite tests\n  type: exitcode-stdio-1.0\n  hs-source-dirs: tests\n  main-is: Properties.hs\n  other-modules: Regressions\n  ghc-options: -Wall\n  build-depends:\n    HUnit,\n    base,\n    bytestring,\n    double-conversion,\n    test-framework,\n    test-framework-hunit,\n    test-framework-quickcheck2,\n    text\n  default-language: Haskell2010\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskell/double-conversion\n";
  }