#!/usr/bin/env bash
set -o nounset -o errexit -o pipefail

# Create temporary locations for the configuration and data directories.
CONFIG="$(mktemp)"
DATADIR="$(mktemp -d)"

# Write the configuration to the temporary location.
echo "media_dir=$PWD" >>"$CONFIG"
echo "db_dir=$DATADIR" >>"$CONFIG"
echo "log_dir=$DATADIR" >>"$CONFIG"
echo 'force_sort_criteria=+upnp:class,+upnp:originalTrackNumber,+dc:title' >>"$CONFIG"
cat $CONFIG

# Make sure everything is cleaned up when this process is killed.
function cleanup {
  rm -r "$DATADIR"
  rm "$CONFIG"
}
trap cleanup exit

# Run minidlnad with the following flags:
#
#  -  `-f "$CONFIG"`: use the configuration we wrote.
#  -  `-f "$PWD/minidlnad.pid"`: store the `.pid` in the current directory.
#  -  `-d`: don't daemonize, we'll kill this when we're done.  This also
#     enabled "debug" mode; but I haven't seen any considerable slowdown
#     from this.
minidlnad -f "$CONFIG" -P "$PWD/minidlnad.pid" -d
