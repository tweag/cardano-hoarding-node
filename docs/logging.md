# Logging

Logging should mainly utilize the
[`Hoard.Effects.Log.Log`](../src/Hoard/Effects/Log.hs) effect.

`Log`'s configuration, which is necessary when using the `runLogWith`
interpreter, is read from the configuration files and the environment in
[`Hoard.Config.Loader`](../src/Hoard/Config/Loader.hs).

Minimum log level can be configured either in the configuration file itself,
under `logging.minimumSeverity`, or through the environment variables `LOG`,
`LOGGING` or `DEBUG`.

- `DEBUG=1` sets the minimum log level to `DEBUG`.
- `LOG=<level>` or `LOGGING=<level>` sets the minimum log level to the
  corresponding level.
- Minimum logging level is prioritised as follows:
  `DEBUG` > `LOGGING` > `LOG` > configuration file.

The available logging levels are:

- `DEBUG`
- `INFO`
- `WARN`
- `ERROR`

A given minimum logging level prevents log messages with a level above it in
this list from being logged.

## Example

Invoking the program like so:

```bash
LOG=WARN ./hoard-exe
```

makes it so that only `WARN` and `ERROR` messages are logged.

```bash
DEBUG=1 ./hoard-exe
```
