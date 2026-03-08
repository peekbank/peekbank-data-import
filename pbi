#!/bin/bash

set -e

COMPOSE_SERVICE="import"

run() {
  docker compose run --rm "$COMPOSE_SERVICE" Rscript -e "$1"
}

if [ $# -eq 0 ]; then
  echo "Usage: ./pbi <command> [args...]"
  echo ""
  echo "Commands:"
  echo "  pipeline [--down] [--resume] [--up] [--no-subprocess]"
  echo "      Run the full import pipeline"
  echo "        --down        Redownload all raw data from OSF"
  echo "        --resume      Keep previous processed_data"
  echo "        --up          Upload processed data to OSF"
  echo "        --no-subprocess  Run all imports in a single R process (uses more memory)"
  echo ""
  echo "  import <dataset> [--down] [--up]"
  echo "      Run a single dataset import"
  echo "        --down    (Re)download raw data from OSF before importing"
  echo "        --up      Upload processed data to OSF"
  exit 0
fi

CMD="$1"
shift

case "$CMD" in
  pipeline)
    NOCACHE="FALSE"
    CLEAN="TRUE"
    UPLOAD="FALSE"
    SUBPROCESS="TRUE"
    for arg in "$@"; do
      case "$arg" in
        --down) NOCACHE="TRUE" ;;
        --resume) CLEAN="FALSE" ;;
        --up) UPLOAD="TRUE" ;;
        --no-subprocess) SUBPROCESS="FALSE" ;;
      esac
    done
    run "global_block_peekbank_summary <- TRUE; source('helper_functions/pipeline.R'); run_all(nocache=${NOCACHE}, clean=${CLEAN}, upload=${UPLOAD}, subprocess=${SUBPROCESS})"
    ;;
  import)
    if [ -z "$1" ]; then
      echo "Usage: ./pbi import <dataset_name> [--down] [--up]"
      exit 1
    fi
    DATASET="$1"
    shift
    PRECMDS=""
    POSTCMDS=""
    for arg in "$@"; do
      case "$arg" in
        --down) PRECMDS="source('helper_functions/osf.R'); unlink(here::here('data','${DATASET}','raw_data'), recursive=TRUE); get_raw_data_fixed('${DATASET}'); " ;;
        --up) POSTCMDS="; source('helper_functions/osf.R'); upload_osf('${DATASET}')" ;;
      esac
    done
    run "${PRECMDS}source('data/${DATASET}/import.R')${POSTCMDS}"
    ;;
  *)
    docker compose run --rm "$COMPOSE_SERVICE" "$CMD" "$@"
    ;;
esac
