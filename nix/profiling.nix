{ inputs, pkgs }:
let
  # Build project with profiling enabled
  profilingProject = import ./project.nix {
    inherit inputs pkgs;
    profiling = true;
  };

  profilingFlake = profilingProject.flake { };

  # Helper script to run with heap profiling (by cost centre)
  profileHeap = pkgs.writeShellScriptBin "profile-heap" ''
    set -euo pipefail

    echo "Starting hoard with heap profiling (by cost centre)..."
    echo ""
    echo "Profiling flags:"
    echo "  -hc: Heap profile by cost centre"
    echo "  -p:  Time and allocation profile"
    echo "  -T:  RTS statistics"
    echo ""
    echo "NOTE: This can produce cluttered graphs. Try 'profile-heap-module' for cleaner output."
    echo ""

    ${profilingFlake.packages."hoard:exe:hoard-exe"}/bin/hoard-exe \
      +RTS \
      -hc \
      -p \
      -T \
      -RTS \
      "$@"

    echo ""
    echo "Profiling complete! Output files:"
    echo "  hoard-exe.prof: Text profile with time and allocation"
    echo "  hoard-exe.hp:   Heap profile data"
    echo ""
    echo "To visualize: nix run .#visualize-heap"
  '';

  # Helper script for module-level heap profiling (cleaner)
  profileHeapModule = pkgs.writeShellScriptBin "profile-heap-module" ''
    set -euo pipefail

    echo "Starting hoard with heap profiling (by module)..."
    echo ""
    echo "Profiling flags:"
    echo "  -hm: Heap profile by MODULE (cleaner than -hc)"
    echo "  -p:  Time and allocation profile"
    echo "  -T:  RTS statistics"
    echo ""

    ${profilingFlake.packages."hoard:exe:hoard-exe"}/bin/hoard-exe \
      +RTS \
      -hm \
      -p \
      -T \
      -RTS \
      "$@"

    echo ""
    echo "Profiling complete! Output files:"
    echo "  hoard-exe.prof: Text profile with time and allocation"
    echo "  hoard-exe.hp:   Heap profile data"
    echo ""
    echo "To visualize: nix run .#visualize-heap"
  '';

  # Helper script for time profiling only (lighter weight)
  profileTime = pkgs.writeShellScriptBin "profile-time" ''
    set -euo pipefail

    echo "Starting hoard with time/allocation profiling..."
    echo "Output: hoard-exe.prof"
    echo ""

    ${profilingFlake.packages."hoard:exe:hoard-exe"}/bin/hoard-exe \
      +RTS \
      -p \
      -T \
      -RTS \
      "$@"

    echo ""
    echo "Profiling complete! View with: cat hoard-exe.prof"
  '';

  # Helper script for detailed allocation profiling
  profileAlloc = pkgs.writeShellScriptBin "profile-alloc" ''
    set -euo pipefail

    OUTDIR="''${1:-./profiling-output}"
    mkdir -p "$OUTDIR"

    echo "Starting hoard with allocation profiling..."
    echo "Output directory: $OUTDIR"
    echo ""

    ${profilingFlake.packages."hoard:exe:hoard-exe"}/bin/hoard-exe \
      +RTS \
      -hy \
      -p \
      -T \
      -L100 \
      -RTS \
      "$@"

    echo ""
    echo "Profiling complete!"
    echo "Heap profile by type: hoard-exe.hp"
    echo ""
    echo "To visualize:"
    echo "  hp2ps -c hoard-exe.hp && ps2pdf hoard-exe.ps"
  '';

  # Helper script for eventlog profiling (for ThreadScope, etc.)
  profileEventlog = pkgs.writeShellScriptBin "profile-eventlog" ''
    set -euo pipefail

    echo "Starting hoard with eventlog profiling..."
    echo "Output: hoard-exe.eventlog"
    echo ""
    echo "This captures detailed runtime events for analysis with:"
    echo "  - ThreadScope (visualize concurrency)"
    echo "  - ghc-events (event statistics)"
    echo "  - speedscope (flame graphs)"
    echo ""

    ${profilingFlake.packages."hoard:exe:hoard-exe"}/bin/hoard-exe \
      +RTS \
      -l \
      -T \
      -RTS \
      "$@"

    echo ""
    echo "Eventlog complete: hoard-exe.eventlog"
    echo ""
    echo "Analyze with:"
    echo "  ghc-events show hoard-exe.eventlog | less"
    echo "  # Or use ThreadScope GUI"
  '';

  # Heap analysis script
  analyzeHeap = pkgs.writeShellScriptBin "analyze-heap" ''
    ${builtins.readFile ../scripts/analyze-heap.sh}
  '';

  # Visualization helper script
  # Get GHC from the haskell.nix project
  ghc = profilingProject.pkg-set.compiler.nix-name;
  hp2ps = "${profilingProject.pkg-set.config.ghc.package}/bin/hp2ps";

  visualizeHeap = pkgs.writeShellScriptBin "visualize-heap" ''
    set -euo pipefail

    HP_FILE="''${1:-hoard-exe.hp}"

    if [ ! -f "$HP_FILE" ]; then
      echo "Error: $HP_FILE not found!"
      echo "Usage: visualize-heap [heap-profile.hp]"
      exit 1
    fi

    echo "Converting $HP_FILE to PDF..."
    # hp2ps from the same GHC used to build the profiling binary
    ${hp2ps} -c "$HP_FILE"
    ${pkgs.ghostscript}/bin/ps2pdf "''${HP_FILE%.hp}.ps"

    PDF_FILE="''${HP_FILE%.hp}.pdf"
    echo "Created: $PDF_FILE"

    if command -v xdg-open >/dev/null; then
      echo "Opening in viewer..."
      xdg-open "$PDF_FILE"
    else
      echo "Open manually: $PDF_FILE"
    fi
  '';
in
{
  # Expose the profiling package
  packages = {
    hoard-profiling = profilingFlake.packages."hoard:exe:hoard-exe";
  };

  # Expose profiling apps
  apps = {
    # Main profiling app: run profiling-enabled executable
    profiling = {
      type = "app";
      program = "${profilingFlake.packages."hoard:exe:hoard-exe"}/bin/hoard-exe";
    };
    profile-heap = {
      type = "app";
      program = "${profileHeap}/bin/profile-heap";
    };
    profile-heap-module = {
      type = "app";
      program = "${profileHeapModule}/bin/profile-heap-module";
    };
    profile-time = {
      type = "app";
      program = "${profileTime}/bin/profile-time";
    };
    profile-alloc = {
      type = "app";
      program = "${profileAlloc}/bin/profile-alloc";
    };
    profile-eventlog = {
      type = "app";
      program = "${profileEventlog}/bin/profile-eventlog";
    };
    analyze-heap = {
      type = "app";
      program = "${analyzeHeap}/bin/analyze-heap";
    };
    visualize-heap = {
      type = "app";
      program = "${visualizeHeap}/bin/visualize-heap";
    };
  };
}
