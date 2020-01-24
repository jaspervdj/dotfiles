#!/bin/bash
set -o nounset -o errexit -o pipefail

CONFIG="$(mktemp)"
DATADIR="$(mktemp -d)"

echo "media_dir=$PWD" >>"$CONFIG"
echo "db_dir=$DATADIR" >>"$CONFIG"
echo "log_dir=$DATADIR" >>"$CONFIG"
cat $CONFIG

function cleanup {
  rm -r "$DATADIR"
  rm "$CONFIG"
}
trap cleanup exit

minidlnad -f "$CONFIG" -P "$PWD/minidlnad.pid" -dRS
