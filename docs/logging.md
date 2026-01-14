# Logging

## Description

The program should emit log messages that:

1. are easy to filter.
2. provide information about the origin of the logged message.
3. contain a logged message.
4. inform about the urgency of the logged message.

Logging utilities should be simple to use, requiring just a message and an
urgency, but should also allow the user to provide more information in the
logged message as necessary.

## Implementation

Logging within the `Eff es` monad should mainly utilize the
[`Hoard.Effects.Log.Log`](../src/Hoard/Effects/Log.hs) effect.

See [`Hoard.Effects.Log`](../src/Hoard/Effects/Log.hs) for information on the
current implementation of logging.

## Filtering

Utilize external programs like `grep` for log filtering.

```bash
./hoard-exe | grep 'BlockFetch: '
./hoard-exe | grep -v 'ChainSync'
./hoard-exe | grep '^\[processor.validation\]'
```
