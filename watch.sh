#!/usr/bin/env bash

APP=$1
PID="nope"
inotifywait -q -m -e create _esy/default |
while read -r filename event; do
    if [[ "$event" == "CREATE build" ]]; then
    if [[ "$(ps -ao '%p' | grep ${PID})" != "" ]]; then
        kill -9 ${PID}
    fi
    _esy/default/build/default/bin/${APP}/${APP}.exe &
    PID=$!
    fi
done
