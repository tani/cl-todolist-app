#!/usr/bin/env bash
set -eu -o pipefail
OLDNAME="fibonacci"
NEWNAME="todolist"
find . -type f -not -name "rename.sh" -exec sed -i "s/$OLDNAME/$NEWNAME/g" {} \;
find . -type f -exec rename "$OLDNAME" "$NEWNAME" {} \;
