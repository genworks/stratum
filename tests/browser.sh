#!/bin/bash
# Copyright © 2026 Genworks International
# AGPL-3.0-or-later; see LICENSE.
#
# The browser half of the regression suite: STRATUM.selfTest() on each
# document named, in headless Chromium (SwiftShader).  BASE is where the
# page is published.  Exits nonzero when any document fails.
#
#   tests/browser.sh                       # parity and demo
#   BASE=http://engine-room:9090 tests/browser.sh parity stress
BASE=${BASE:-http://engine-room:9090}
DIR=$(cd "$(dirname "$0")" && pwd)
DOCS=("$@")
[ ${#DOCS[@]} -eq 0 ] && DOCS=(parity demo)
status=0
for doc in "${DOCS[@]}"; do
  echo "== $doc"
  timeout 600 node "$DIR/probe.js" "$BASE/stratum/?doc=$doc" "${SHOT_DIR:+$SHOT_DIR/selftest-$doc.png}"
  rc=$?
  echo "== $doc exit $rc"
  [ $rc -ne 0 ] && status=1
done
exit $status
