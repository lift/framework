# Running the MongoDB-backed tests

`lift-mongodb` and `lift-mongodb-record` test against a real MongoDB server.
This page explains how to give them one without installing anything on your
machine.

## Quick start

```bash
scripts/mongodb-docker.sh run lift-mongodb/test lift-mongodb-record/test
```

That starts a MongoDB container, waits until it is answering commands, runs sbt,
and stops the container again — including when sbt fails.

To keep the server up while you work:

```bash
scripts/mongodb-docker.sh start
sbt -batch "lift-mongodb/test"
scripts/mongodb-docker.sh stop
```

## Commands

| Command | What it does |
| --- | --- |
| `start` | Start the container and wait until MongoDB answers `ping`. Safe to run twice. |
| `stop` | Stop and remove the container. |
| `status` | Say whether the server is ready. Exits non-zero if it is not. |
| `logs` | Print the server log. |
| `run <sbt args...>` | `start`, run `sbt -batch <args>`, then `stop`. Propagates sbt's exit status. |
| `--help` | Print usage. |

## Why MongoDB 4.0

`LegacyMongoDirectSpec` calls `db.eval(...)`, which is a wrapper around the
server-side `$eval` command. MongoDB removed `$eval` in 4.2, so on 4.2 or newer
that spec fails. **4.0 is the newest server this test suite supports**, and the
script pins `mongo:4.0` for that reason. Do not change the image without dealing
with that spec first.

## Why the container always publishes port 27017

The test kits read their address as:

```scala
Props.get("mongo.test.uri", "127.0.0.1:27017")
```

`Props.get` looks in a **classpath properties file**, not a JVM system property —
see `core/util/src/main/scala/net/liftweb/util/Props.scala`. So `sbt
-Dmongo.test.uri=…` does nothing, and the tests will only find a server on 27017
unless a `props` file is added to the test classpath.

For that reason the script refuses to be clever: if 27017 is already occupied it
stops with an explanation rather than starting a container on a port the tests
would ignore.

## Environment variables

| Variable | Default | Meaning |
| --- | --- | --- |
| `MONGO_IMAGE` | `mongo:4.0` | Image to run. |
| `MONGO_CONTAINER` | `lift-mongod` | Container name. |
| `MONGO_PORT` | `27017` | Host port to publish. See the caveat above. |
| `MONGO_READY_TIMEOUT` | `60` | Seconds to wait for the server to answer. |
| `MONGO_KEEP` | unset | Set to `1` so `run` leaves the container up afterwards. |

## The database is not persisted

No host directory is mounted. The database lives inside the container and is
discarded when it stops, so every run starts from an empty server. This is
deliberate: it keeps runs reproducible and avoids the ownership mismatch you get
when mongod (uid 999 in the image) writes into a directory owned by you. If you
need data to survive, export it with `docker exec lift-mongod mongoexport`.

## Testing the script itself

```bash
scripts/mongodb-docker-selftest.sh
```

This drives the script as a black box and checks the behaviour claimed above.
It starts and stops a real container, so do not run it while another test run is
using the same container.

Claims on this page and the check that covers each:

| Claim | Covered by |
| --- | --- |
| `--help` prints usage and exits 0 | selftest check 1 |
| `status` exits non-zero when no container is running | selftest check 2 |
| `start` exits 0 | selftest check 3 |
| `status` reports ready after `start` | selftest check 4 |
| The server is MongoDB 4.0.x | selftest check 5 |
| `start` is idempotent (one container, not two) | selftest check 6 |
| The published port accepts a TCP connection | selftest check 7 |
| `stop` exits 0 | selftest check 8 |
| `status` exits non-zero after `stop` | selftest check 9 |
| The Mongo test modules pass against this container | `scripts/mongodb-docker.sh run lift-mongodb/test lift-mongodb-record/test` — see below |
| `MongoRecordAsyncSpec` no longer depends on a one-second await budget | isolated `lift-mongodb-record/testOnly net.liftweb.mongodb.record.MongoRecordAsyncSpec` with the container up: 3 examples, 0 failure, 0 error, where the same isolated run failed 3/3 before the change |

Run on 2026-10-17: `9 checks, 0 failed`.

## Known result at the time of writing

Against the container, on JDK 21 with Scala 2.12.18:

- `lift-mongodb/test`: 60 tests, 60 passed, 0 failed — including
  `LegacyMongoDirect Specification`, the spec that needs `$eval`.
- `lift-mongodb-record/test`: 335 examples, 0 failed, 0 errors.

`MongoRecordAsyncSpec` used to fail intermittently on specs2's one-second
`await` budget. That is fixed: the spec now states an explicit five-second
budget (`.awaitFor(5.seconds)`) on all four of its awaits, because every example
awaits a MongoDB round trip and the async driver's first write after a cold start
was measured exceeding one second. The assertions were not weakened — each
example still awaits the operation and then reads the record back to check the
stored values.

## Troubleshooting

**`MongoAsyncTestKit.after` errors, and the examples are skipped** — MongoDB is
not running, and `checkMongoIsRunning` skipped the examples (`orSkip`) while the
`after` hook still tried to drop the databases. This is what you see if you run
the tests after `scripts/mongodb-docker-selftest.sh`, which stops the container
on exit by design. Start it again with `scripts/mongodb-docker.sh start`, or use
`scripts/mongodb-docker.sh run <sbt args…>`, which handles both ends.

**`something is already listening on 127.0.0.1:27017`** — another MongoDB, or a
container from an earlier run, is using the port. Stop it, or run
`scripts/mongodb-docker.sh status` to see whether it is already this script's
container.

**`MongoDB did not answer within 60s`** — the script prints the container log
before giving up. On a cold machine the first run also has to pull the image.
Raise `MONGO_READY_TIMEOUT` if the machine is slow.

**`the docker daemon is not reachable`** — start Docker.

**Tests hang or fail to connect** — check `scripts/mongodb-docker.sh status`
first. `docker ps` should show `lift-mongod` publishing
`127.0.0.1:27017->27017/tcp`.
