#!/usr/bin/env bash
#
# mongodb-docker-selftest.sh - verify the behaviour that
# docs/mongodb-docker.md and docs/llm/mongodb-docker_llm.md claim for
# scripts/mongodb-docker.sh.
#
# This is the test behind those documents' claims. It exercises the script as a
# black box through its command line only, and it starts and stops a real
# container, so it must not be run while another test run is using that
# container.
#
# Claims checked, in order:
#
#   1. `--help` prints usage and exits 0.
#   2. `status` exits non-zero when no container is running.
#   3. `start` exits 0, and afterwards `status` exits 0 and reports "ready".
#   4. The server is MongoDB 4.0.x - the newest series whose `$eval` command
#      exists, which LegacyMongoDirectSpec depends on.
#   5. `start` is idempotent: a second `start` exits 0 without creating a
#      second container.
#   6. The published port accepts a TCP connection on 127.0.0.1.
#   7. `stop` exits 0 and `status` then exits non-zero.
#
# Usage: scripts/mongodb-docker-selftest.sh
#
set -euo pipefail

cd "$(dirname "$0")/.."

SCRIPT=scripts/mongodb-docker.sh
CONTAINER="${MONGO_CONTAINER:-lift-mongod}"
PORT="${MONGO_PORT:-27017}"
failures=0
checks=0

pass() { checks=$((checks + 1)); printf 'ok   %2d - %s\n' "$checks" "$1"; }
fail() {
  checks=$((checks + 1))
  failures=$((failures + 1))
  printf 'FAIL %2d - %s\n' "$checks" "$1"
}

cleanup() {
  "$SCRIPT" stop >/dev/null 2>&1 || true
}
trap cleanup EXIT

printf 'mongodb-docker selftest\n'

# 1
if "$SCRIPT" --help >/dev/null 2>&1; then
  pass "--help exits 0"
else
  fail "--help exits 0"
fi

# 2 - start from a known-clean state
"$SCRIPT" stop >/dev/null 2>&1 || true
if "$SCRIPT" status >/dev/null 2>&1; then
  fail "status exits non-zero when no container is running"
else
  pass "status exits non-zero when no container is running"
fi

# 3
if "$SCRIPT" start >/dev/null 2>&1; then
  pass "start exits 0"
else
  fail "start exits 0"
fi

if "$SCRIPT" status 2>&1 | grep -q 'ready'; then
  pass "status reports ready after start"
else
  fail "status reports ready after start"
fi

# 4
version="$(docker exec "$CONTAINER" mongo --quiet --eval 'db.version()' 2>/dev/null | tr -d '[:space:]')"
case "$version" in
  4.0.*) pass "server is MongoDB 4.0.x (got $version)" ;;
  *) fail "server is MongoDB 4.0.x (got '${version:-nothing}')" ;;
esac

# 5
if "$SCRIPT" start >/dev/null 2>&1; then
  count="$(docker ps --filter "name=^${CONTAINER}$" --format '{{.Names}}' | wc -l | tr -d '[:space:]')"
  if [ "$count" = "1" ]; then
    pass "start is idempotent (still exactly one container)"
  else
    fail "start is idempotent (found $count containers named $CONTAINER)"
  fi
else
  fail "start is idempotent (second start exited non-zero)"
fi

# 6
if (exec 3<>"/dev/tcp/127.0.0.1/$PORT") >/dev/null 2>&1; then
  pass "127.0.0.1:$PORT accepts connections"
else
  fail "127.0.0.1:$PORT accepts connections"
fi

# 7
if "$SCRIPT" stop >/dev/null 2>&1; then
  pass "stop exits 0"
else
  fail "stop exits 0"
fi

if "$SCRIPT" status >/dev/null 2>&1; then
  fail "status exits non-zero after stop"
else
  pass "status exits non-zero after stop"
fi

printf '\n%d checks, %d failed\n' "$checks" "$failures"
[ "$failures" -eq 0 ]
