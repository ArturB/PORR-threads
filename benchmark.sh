#!/bin/bash

# Generate build ID
let "TODAY_SEC = $( date +%s ) % 86400"
BUILD_ID="$( date +%y%j ).$TODAY_SEC"

# Build all benchmartks and push changes to git if successful
# If build failed, don't go on, but exit
if stack bench --no-run-benchmarks ; then
    echo -e "Build successful!\nPushing changes to git..."
    ( git add -A > /dev/null && git commit -qm "Benchmark build $BUILD_ID" && git pull -q && git push -q ) &
    echo -e "Running all benchmarks..."
else
    exit 1
fi

# Perform all benchmarks in background and move on
stack bench 
