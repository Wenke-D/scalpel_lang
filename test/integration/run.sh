set -e

EXE='dune exec scalpel'

BASEDIR=$(dirname $0)

DATA_DIR="${BASEDIR}/data"

find "$DATA_DIR" -type f -name "*.scpl" -exec echo Compiling '{}' \; -exec $EXE '{}' \;