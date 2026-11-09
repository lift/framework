#!/usr/bin/env bash
#
# mongodb-docker.sh - run MongoDB in a Docker container for Lift's
# MongoDB-backed tests.
#
# WHY THIS EXISTS
#
#   `lift-mongodb` and `lift-mongodb-record` talk to a real MongoDB server on
#   127.0.0.1:27017. Their test kits read that address as
#
#       Props.get("mongo.test.uri", "127.0.0.1:27017")
#
#   (persistence/mongodb/src/test/scala/net/liftweb/mongodb/MongoTestKit.scala
#   and the equivalent in lift-mongodb-record). Note that `Props.get` reads a
#   *classpath properties file*, not a JVM system property, so `-Dmongo.test.uri`
#   on the sbt command line has NO effect. The container is therefore published
#   on 27017, which is the value the tests default to.
#
# WHY MONGODB 4.0 SPECIFICALLY
#
#   LegacyMongoDirectSpec.scala:100 calls
#
#       db.eval(" function() { return 3+3; } ")
#
#   `db.eval` is a wrapper around the server-side `$eval` command, which MongoDB
#   removed in 4.2. On 4.2 or newer that test fails. 4.0 is the newest server the
#   test suite supports.
#
# USAGE
#
#   scripts/mongodb-docker.sh start              start the container and wait
#   scripts/mongodb-docker.sh stop               stop and remove the container
#   scripts/mongodb-docker.sh status             report whether it is ready
#   scripts/mongodb-docker.sh logs               print the server log
#   scripts/mongodb-docker.sh run <sbt args...>  start, run sbt, then stop
#
# EXAMPLES
#
#   scripts/mongodb-docker.sh run lift-mongodb/test
#   scripts/mongodb-docker.sh run \
#       "++2.12.18" "lift-mongodb/test" "lift-mongodb-record/test"
#   scripts/mongodb-docker.sh start && sbt -batch "lift-mongodb/test" && \
#       scripts/mongodb-docker.sh stop
#
# ENVIRONMENT
#
#   MONGO_IMAGE          image to run          (default: mongo:4.0)
#   MONGO_CONTAINER      container name        (default: lift-mongod)
#   MONGO_PORT           host port to publish  (default: 27017)
#   MONGO_READY_TIMEOUT  seconds to wait for the server to accept commands
#                                             (default: 60)
#   MONGO_KEEP=1         with `run`, leave the container running afterwards
#
# NOTE ON VOLUMES
#
#   No host directory is mounted. The database lives in the container's own
#   filesystem and is discarded with the container, so every run starts from an
#   empty server and there is no bind-mounted directory whose ownership could
#   drift from the invoking user. See
#   docs/architecture/decisions/0002-mongodb-in-docker-for-tests.md.
#
set -euo pipefail

CONTAINER="${MONGO_CONTAINER:-lift-mongod}"
IMAGE="${MONGO_IMAGE:-mongo:4.0}"
PORT="${MONGO_PORT:-27017}"
READY_TIMEOUT="${MONGO_READY_TIMEOUT:-60}"

die() {
  printf 'mongodb-docker: %s\n' "$*" >&2
  exit 1
}

info() {
  printf 'mongodb-docker: %s\n' "$*"
}

require_docker() {
  command -v docker >/dev/null 2>&1 ||
    die "docker is not installed or not on PATH; this script requires docker"
  docker info >/dev/null 2>&1 ||
    die "the docker daemon is not reachable; start it and try again"
}

container_exists() {
  docker container inspect "$CONTAINER" >/dev/null 2>&1
}

container_running() {
  [ "$(docker container inspect -f '{{.State.Running}}' "$CONTAINER" 2>/dev/null)" = "true" ]
}

port_in_use() {
  (exec 3<>"/dev/tcp/127.0.0.1/$PORT") >/dev/null 2>&1
}

# Ask the server to execute a command. Prints "1" on success.
server_answers() {
  docker exec "$CONTAINER" mongo --quiet --eval 'db.adminCommand({ping:1}).ok' 2>/dev/null |
    tr -d '[:space:]'
}

wait_ready() {
  local waited=0
  while [ "$waited" -lt "$READY_TIMEOUT" ]; do
    if [ "$(server_answers)" = "1" ]; then
      return 0
    fi
    if ! container_running; then
      printf '\n' >&2
      docker logs "$CONTAINER" >&2 || true
      die "the container exited before MongoDB became ready (image: $IMAGE)"
    fi
    sleep 1
    waited=$((waited + 1))
  done

  printf '\n' >&2
  docker logs "$CONTAINER" >&2 || true
  die "MongoDB did not answer within ${READY_TIMEOUT}s (image: $IMAGE)"
}

cmd_start() {
  require_docker

  if container_running; then
    if [ "$(server_answers)" = "1" ]; then
      info "already running and ready on 127.0.0.1:$PORT (container: $CONTAINER)"
      return 0
    fi
    info "container $CONTAINER exists but is not answering yet; waiting"
    wait_ready
    info "ready on 127.0.0.1:$PORT (container: $CONTAINER)"
    return 0
  fi

  if container_exists; then
    info "removing stopped container $CONTAINER"
    docker rm "$CONTAINER" >/dev/null
  fi

  if port_in_use; then
    die "something is already listening on 127.0.0.1:$PORT. Stop it, or run with
       MONGO_PORT set to a free port *and* give the tests a props file that sets
       mongo.test.uri to that port: the test kits read the address from
       Props.get(\"mongo.test.uri\", ...), which consults a classpath props file and
       not a system property."
  fi

  info "starting $IMAGE as container $CONTAINER on 127.0.0.1:$PORT"
  # --bind_ip_all is required: mongod would otherwise listen on the container's
  # own 127.0.0.1 only, and Docker's port forwarding arrives from the bridge
  # gateway address instead.
  docker run --detach --rm \
    --name "$CONTAINER" \
    --publish "127.0.0.1:$PORT:27017" \
    "$IMAGE" --bind_ip_all >/dev/null

  wait_ready
  info "ready on 127.0.0.1:$PORT (container: $CONTAINER)"
}

cmd_stop() {
  require_docker
  if container_exists; then
    info "stopping container $CONTAINER"
    docker stop "$CONTAINER" >/dev/null
  else
    info "no container named $CONTAINER"
  fi
}

cmd_status() {
  require_docker
  if ! container_exists; then
    info "not running (no container named $CONTAINER)"
    return 1
  fi
  if ! container_running; then
    info "not running (container $CONTAINER exists but is stopped)"
    return 1
  fi
  if [ "$(server_answers)" = "1" ]; then
    info "ready on 127.0.0.1:$PORT (container: $CONTAINER, image: $IMAGE)"
    return 0
  fi
  info "container $CONTAINER is running but MongoDB is not answering"
  return 1
}

cmd_logs() {
  require_docker
  container_exists || die "no container named $CONTAINER"
  docker logs "$CONTAINER"
}

cmd_run() {
  [ "$#" -gt 0 ] || die "usage: mongodb-docker.sh run <sbt args...>"
  require_docker

  cmd_start

  local status=0
  info "sbt -batch $*"
  sbt -batch "$@" || status=$?

  if [ "${MONGO_KEEP:-0}" = "1" ]; then
    info "leaving container $CONTAINER running (MONGO_KEEP=1)"
  else
    cmd_stop || true
  fi

  return "$status"
}

usage() {
  sed -n '/^# USAGE/,/^# ENVIRONMENT/p' "$0" | sed 's/^# \{0,1\}//'
}

main() {
  local command="${1:-}"
  [ "$#" -gt 0 ] && shift || true

  case "$command" in
    start) cmd_start "$@" ;;
    stop) cmd_stop "$@" ;;
    status) cmd_status "$@" ;;
    logs) cmd_logs "$@" ;;
    run) cmd_run "$@" ;;
    "" | -h | --help | help) usage ;;
    *) die "unknown command: $command (try --help)" ;;
  esac
}

main "$@"
