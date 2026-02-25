# Profiling Guide

Quick guide for profiling hoard using GHC's built-in profiling tools.

## Quick Start

### 1. Run with Profiling

```bash
# Run hoard with profiling enabled
nix run .#profiling -- +RTS -p -hc -T -RTS your-args-here
```

This creates:
- `hoard-exe.prof` - Time and allocation profile
- `hoard-exe.hp` - Heap profile data

### 2. Visualize Heap Profile

```bash
# Convert heap profile to PDF and open it
nix run .#visualize-heap
```

Opens a PDF showing heap usage over time by cost centre.

## Common Profiling Workflows

### Heap Profiling by Module

For cleaner heap profiles grouped by module instead of cost centre:

```bash
nix run .#profile-heap-module
nix run .#visualize-heap
```

### Time/Allocation Profiling Only

For lightweight profiling without heap data:

```bash
nix run .#profile-time
cat hoard-exe.prof
```

### Custom RTS Flags

Run profiling build directly with your own flags:

```bash
# Heap profile by type
nix run .#profiling -- +RTS -hy -p -T -RTS

# Heap profile by closure description
nix run .#profiling -- +RTS -hd -p -T -RTS

# See all RTS options
nix run .#profiling -- +RTS --help -RTS
```

## Available Profiling Apps

| Command | Description |
|---------|-------------|
| `nix run .#profiling` | Profiling-enabled executable (pass custom RTS flags) |
| `nix run .#profile-heap` | Heap profiling by cost centre (detailed but cluttered) |
| `nix run .#profile-heap-module` | Heap profiling by module (cleaner) |
| `nix run .#profile-time` | Time and allocation profiling only |
| `nix run .#visualize-heap` | Convert .hp file to PDF |

## Understanding the Output

### Time Profile (`hoard-exe.prof`)

Shows which functions consume the most time and allocations:

```
COST CENTRE          %time  %alloc
fetchBlocks           45.2    38.1
processBlock          28.3    42.5
```

### Heap Profile (`hoard-exe.pdf`)

Visual graph showing heap usage over time. Look for:
- **Steady growth** = possible memory leak
- **Spikes** = temporary allocations (usually fine)
- **Flat plateaus** = stable memory usage

## Tips

- Use `-hm` (module) for cleaner graphs than `-hc` (cost centre)
- Add `-L100` for more detailed heap sampling
- For concurrency analysis, use `nix run .#profile-eventlog` instead
- Profile with realistic workloads - toy data won't show real issues

## Further Reading

- [GHC Profiling Guide](https://downloads.haskell.org/ghc/latest/docs/users_guide/profiling.html)
- [Real World Haskell: Profiling](http://book.realworldhaskell.org/read/profiling-and-optimization.html)
