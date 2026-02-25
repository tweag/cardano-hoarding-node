{
  inputs,
  pkgs,
  profiling ? false,
}:
let
  # Common package configuration
  commonPackages = {
    # Disable tests for tmp-postgres
    tmp-postgres.doCheck = false;

    # Configure hoard package
    hoard = {
      # Treat warnings as errors in Nix builds (CI)
      ghcOptions = [ "-Werror" ];

      # Configure test suite component
      components.tests.hoard-test = {
        # Add build-time tools needed for tests
        build-tools = [
          pkgs.postgresql
          pkgs.sqitchPg
        ];

        # Set HOME for initdb in tests
        preCheck = ''
          export HOME="$TMPDIR"
        '';
      };
    };
  };

  # Profiling-specific configuration
  profilingModules = {
    # Enable profiling for ALL packages (including dependencies)
    enableLibraryProfiling = true;
    profilingDetail = "all-functions";

    packages = {
      # Configure hoard package for profiling
      hoard = {
        # Enable executable profiling
        configureFlags = [ "--enable-profiling" ];

        # Add GHC profiling flags for detailed cost centres
        components.library.ghcOptions = [
          "-fprof-auto"
          "-fprof-cafs"
        ];
        components.exes.hoard-exe.ghcOptions = [
          "-fprof-auto"
          "-fprof-cafs"
        ];
      };
    };
  };
in
pkgs.haskell-nix.cabalProject' {
  src = ../.;
  compiler-nix-name = "ghc966";

  # Enable materialization for deterministic builds and better CI caching
  materialized = ./materialized;
  checkMaterialization = true;

  # Enable CHaP (Cardano Haskell Packages) repository
  inputMap = {
    "https://chap.intersectmbo.org/" = inputs.CHaP;
  };

  # Add tmp-postgres from flake input
  cabalProjectLocal = ''
    source-repository-package
      type: git
      location: https://github.com/jfischoff/tmp-postgres
      tag: ${inputs.tmp-postgres.rev}
      --sha256: 0l1gdx5s8ximgawd3yzfy47pv5pgwqmjqp8hx5rbrq68vr04wkbl
  '';

  modules = [
    { packages = commonPackages; }
    (if profiling then profilingModules else { })
  ];
}
