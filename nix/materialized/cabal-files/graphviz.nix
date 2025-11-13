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
    flags = { test-parsing = false; };
    package = {
      specVersion = "1.18";
      identifier = { name = "graphviz"; version = "2999.20.2.1"; };
      license = "BSD-3-Clause";
      copyright = "Matthew Sackman, Ivan Lazar Miljenovic";
      maintainer = "Daniel Casanueva (coding `at` danielcasanueva.eu)";
      author = "Matthew Sackman, Ivan Lazar Miljenovic";
      homepage = "";
      url = "";
      synopsis = "Bindings to Graphviz for graph visualisation.";
      description = "This library provides bindings for the Dot language used by the\nGraphviz (<http://graphviz.org/>) suite of programs for visualising\ngraphs, as well as functions to call those programs.\n\nMain features of the graphviz library include:\n\n* Almost complete coverage of all Graphviz attributes and syntax.\n\n* Support for specifying clusters.\n\n* The ability to use a custom node type.\n\n* Functions for running a Graphviz layout tool with all specified\noutput types.\n\n* The ability to not only generate but also parse Dot code with two\noptions: strict and liberal (in terms of ordering of statements).\n\n* Functions to convert FGL graphs and other graph-like data structures\nto Dot code - including support to group them into clusters - with a\nhigh degree of customisation by specifying which attributes to use\nand limited support for the inverse operation.\n\n* Round-trip support for passing an FGL graph through Graphviz to\naugment node and edge labels with positional information, etc.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          (hsPkgs."fgl" or (errorHandler.buildDepError "fgl"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."polyparse" or (errorHandler.buildDepError "polyparse"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."colour" or (errorHandler.buildDepError "colour"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."wl-pprint-text" or (errorHandler.buildDepError "wl-pprint-text"))
          (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
        ];
        buildable = true;
      };
      exes = {
        "graphviz-testparsing" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."graphviz" or (errorHandler.buildDepError "graphviz"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
          buildable = if flags.test-parsing then true else false;
        };
      };
      tests = {
        "graphviz-testsuite" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."graphviz" or (errorHandler.buildDepError "graphviz"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."fgl" or (errorHandler.buildDepError "fgl"))
            (hsPkgs."fgl-arbitrary" or (errorHandler.buildDepError "fgl-arbitrary"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          ];
          build-tools = [
            (hsPkgs.pkgsBuildBuild.hspec-discover.components.exes.hspec-discover or (pkgs.pkgsBuildBuild.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
          ];
          buildable = true;
        };
      };
      benchmarks = {
        "graphviz-printparse" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."graphviz" or (errorHandler.buildDepError "graphviz"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/graphviz-2999.20.2.1.tar.gz";
      sha256 = "8ffc0fa59e01de47f9802ed1ca486c6d000babb51d7da7cd17c85ef068d7b382";
    });
  }) // {
    package-description-override = "Name:               graphviz\nVersion:            2999.20.2.1\nSynopsis:           Bindings to Graphviz for graph visualisation.\nDescription: {\nThis library provides bindings for the Dot language used by the\nGraphviz (<http://graphviz.org/>) suite of programs for visualising\ngraphs, as well as functions to call those programs.\n.\nMain features of the graphviz library include:\n.\n* Almost complete coverage of all Graphviz attributes and syntax.\n.\n* Support for specifying clusters.\n.\n* The ability to use a custom node type.\n.\n* Functions for running a Graphviz layout tool with all specified\n  output types.\n.\n* The ability to not only generate but also parse Dot code with two\n  options: strict and liberal (in terms of ordering of statements).\n.\n* Functions to convert FGL graphs and other graph-like data structures\n  to Dot code - including support to group them into clusters - with a\n  high degree of customisation by specifying which attributes to use\n  and limited support for the inverse operation.\n.\n* Round-trip support for passing an FGL graph through Graphviz to\n  augment node and edge labels with positional information, etc.\n}\n\nCategory:           Graphs, Graphics\nLicense:            BSD3\nLicense-File:       LICENSE.md\nCopyright:          Matthew Sackman, Ivan Lazar Miljenovic\nAuthor:             Matthew Sackman, Ivan Lazar Miljenovic\nMaintainer:         Daniel Casanueva (coding `at` danielcasanueva.eu)\nBuild-Type:         Simple\nCabal-Version:      1.18\nExtra-Doc-Files:    TODO.md, Changelog.md, README.md, FAQ.md\nExtra-Source-Files: utils/AttributeGenerator.hs\nBug-Reports:        https://codeberg.org/daniel-casanueva/graphviz/issues\n\nFlag test-parsing\n     Description: Build a utility to test parsing of available Dot code.\n     Default:     False\n\nLibrary {\n        Default-Language:  Haskell2010\n\n        Build-Depends:     base >=4.5.0.0 && <5,\n                           containers,\n                           process,\n                           directory,\n                           temporary >=1.1 && <1.4,\n                           fgl >= 5.4 && < 5.9,\n                           filepath,\n                           polyparse >=1.9 && <1.14,\n                           bytestring >= 0.9,\n                           colour == 2.3.*,\n                           mtl == 2.*,\n                           text,\n                           wl-pprint-text == 1.2.*,\n                           dlist >= 0.5 && < 1.1\n\n        Exposed-Modules:   Data.GraphViz\n                           Data.GraphViz.Types\n                           Data.GraphViz.Types.Canonical\n                           Data.GraphViz.Types.Generalised\n                           Data.GraphViz.Types.Graph\n                           Data.GraphViz.Types.Monadic\n                           Data.GraphViz.Parsing\n                           Data.GraphViz.Printing\n                           Data.GraphViz.Commands\n                           Data.GraphViz.Commands.IO\n                           Data.GraphViz.Attributes\n                           Data.GraphViz.Attributes.Complete\n                           Data.GraphViz.Attributes.Colors\n                           Data.GraphViz.Attributes.Colors.X11\n                           Data.GraphViz.Attributes.Colors.Brewer\n                           Data.GraphViz.Attributes.Colors.SVG\n                           Data.GraphViz.Attributes.HTML\n                           Data.GraphViz.PreProcessing\n                           Data.GraphViz.Exception\n                           Data.GraphViz.Algorithms\n\n                           Data.GraphViz.Attributes.Internal\n                           Data.GraphViz.Internal.Util\n                           Data.GraphViz.Internal.State\n                           Data.GraphViz.Types.Internal.Common\n\n        Other-Modules:     Data.GraphViz.Algorithms.Clustering\n                           Data.GraphViz.Attributes.Arrows\n                           Data.GraphViz.Attributes.ColorScheme\n                           Data.GraphViz.Attributes.Same\n                           Data.GraphViz.Attributes.Values\n                           Data.GraphViz.Commands.Available\n                           Data.GraphViz.Types.State\n\n        Ghc-Options: -Wall\n}\n\nTest-Suite graphviz-testsuite {\n        Default-Language:  Haskell2010\n\n        Type:              exitcode-stdio-1.0\n\n        -- Versions controlled by library section\n        Build-Depends:     base,\n                           graphviz,\n                           containers,\n                           fgl >= 5.5.0.0,\n                           fgl-arbitrary == 0.2.*,\n                           filepath,\n                           hspec >= 2.1 && < 3,\n                           text,\n                           QuickCheck >= 2.3 && < 2.16\n        Build-Tool-Depends: hspec-discover:hspec-discover == 2.*\n\n        hs-Source-Dirs:    tests\n\n        Main-Is:           Main.hs\n\n\n        Other-Modules:       Data.GraphViz.Testing.Instances\n                             Data.GraphViz.Testing.Properties\n                             Data.GraphViz.Testing.Instances.Helpers\n                             Data.GraphViz.Testing.Instances.Attributes\n                             Data.GraphViz.Testing.Instances.Common\n                             Data.GraphViz.Testing.Instances.Canonical\n                             Data.GraphViz.Testing.Instances.Generalised\n                             Data.GraphViz.Testing.Instances.Graph\n                             Data.GraphViz.Testing.Proxy\n\n                             Data.GraphVizSpec\n                             Data.GraphViz.AlgorithmsSpec\n                             Data.GraphViz.Attributes.CompleteSpec\n                             Data.GraphViz.Attributes.HTMLSpec\n                             Data.GraphViz.PreProcessingSpec\n                             Data.GraphViz.Types.CanonicalSpec\n                             Data.GraphViz.Types.GeneralisedSpec\n                             Data.GraphViz.Types.GraphSpec\n\n                             Spec\n\n        if True\n           Ghc-Options: -Wall\n\n        if impl(ghc >= 6.12.1)\n           Ghc-Options: -fno-warn-unused-do-bind\n\n        GHC-Prof-Options: -rtsopts\n}\n\nBenchmark graphviz-printparse {\n        Default-Language: Haskell2010\n\n        Type:             exitcode-stdio-1.0\n\n        Build-Depends:    base,\n                          deepseq,\n                          text,\n                          graphviz,\n                          criterion >= 0.5 && < 1.7\n\n        hs-Source-Dirs:   utils\n\n        Main-Is:          Benchmark.hs\n\n        Ghc-Options: -Wall\n\n        GHC-Prof-Options: -rtsopts\n}\n\nExecutable graphviz-testparsing {\n        Default-Language: Haskell2010\n\n        if flag(test-parsing)\n           Buildable:     True\n        else\n           Buildable:     False\n\n        hs-Source-Dirs:   utils\n\n        Main-Is:          TestParsing.hs\n\n        Build-Depends:    base,\n                          graphviz,\n                          bytestring,\n                          directory,\n                          filepath,\n                          text\n\n        Ghc-Options: -Wall\n\n        GHC-Prof-Options: -rtsopts\n}\n";
  }