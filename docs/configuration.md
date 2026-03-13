# Configuration

Configuration is loaded from three sources, merged in order of increasing priority:

1. `config/{env}.yaml` — base configuration
2. `secrets/{env}.yaml` — sensitive values (see [secrets.md](secrets.md))
3. Environment variables — runtime overrides

## Environment variable overrides

Any config value can be overridden at runtime via an environment variable. The naming convention is:

```
HOARD__<SEGMENT>__<SEGMENT>...=value
```

- The prefix is `HOARD`
- Path segments are separated by **double underscores** (`__`)
- Single underscores within a segment are preserved (field names use `quiet_snake_case`)
- Keys are case-insensitive; `HOARD__SERVER__PORT` and `hoard__server__port` are equivalent

### Examples

```bash
# Override server port
HOARD__SERVER__PORT=8080

# Override database host
HOARD__DATABASE__HOST=db.internal

# Override a deeply nested value
HOARD__CARDANO_PROTOCOLS__BLOCK_FETCH__BATCH_SIZE=20

# Override logging severity
HOARD__LOGGING__MINIMUM_SEVERITY=INFO
```

### Type coercion

Values are parsed as their natural JSON type when unambiguous:

| Env var value | JSON type |
|---|---|
| `3000` | `Number` |
| `true` / `false` | `Bool` |
| anything else | `String` |

### Why double underscores?

Field names in the config use `quiet_snake_case` (e.g. `minimum_severity`, `batch_size`). A single underscore would be ambiguous as both separator and part of the field name. Double underscores unambiguously delimit path segments.
