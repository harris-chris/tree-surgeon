#!/bin/bash

# Check starting state
if [[ $(basename $(pwd)) != "tree-surgeon" ]]; then
    echo "Please start shell tests from tree-surgeon base directory"
    exit 1
fi
cabal install --installdir="./test"

PWD=$(pwd)
EXEC="$PWD/test/tree-surgeon"

echo "Test that tree-surgeon can be used to filter a directory after copying"
cp -rf "test/test-data" "test/test-data-temp"
cd "test/test-data-temp"
EXEC_STRING="$EXEC to-bash -f 'nameEndsWith ".cpp"' -s ./ | xargs rm"
echo "Executing $EXEC_STRING"
eval $EXEC_STRING
if

