#!/bin/bash

set -e

COMPOSE_SERVICE="import"

# Builds the image, so the container runs the code as it is in the folder, and
# removes the images of this project that the build replaced
build() {
  docker compose build
  docker image prune -f --filter "label=org.peekbank.image=data-import" > /dev/null
}

run() {
  build
  docker compose run --rm "$COMPOSE_SERVICE" Rscript -e "$1"
}

if [ $# -eq 0 ]; then
  echo "Usage: ./pbi <command> [args...]"
  echo ""
  echo "Commands:"
  echo "  new [--legacy]"
  echo "      Set up an import script for a new dataset"
  echo "        --legacy  Use the legacy template"
  echo ""
  echo "  pipeline [--down] [--resume] [--up] [--no-subprocess]"
  echo "      Run the full import pipeline (docker needed)"
  echo "        --down        Redownload all raw data from Redivis"
  echo "        --resume      Keep previous processed_data"
  echo "        --up          Upload the processed data of the datasets that pass validation"
  echo "        --no-subprocess  Run all imports in a single R process (uses more memory)"
  echo ""
  echo "  import <dataset> [--down] [--up]"
  echo "      Run a single dataset import (docker needed)"
  echo "        --down    (Re)download raw data from Redivis before importing"
  echo "        --up      Upload the processed data if it passes validation"
  echo ""
  echo "  ingest <release-name> [--cache] [--dry-run]"
  echo "      Rerun every import and stage the release tables in the Redivis draft (docker needed)"
  echo "        --cache     Use the processed data uploaded to Redivis instead of rerunning the imports"
  echo "        --dry-run   For testing: Build the tables, but does not stage/upload"
  echo ""
  echo "  rename-release <release-name>"
  echo "      Change the name of the release staged in the Redivis draft (docker needed)"
  echo ""
  echo "  release <release-name>"
  echo "      Release the Redivis draft, if a finished full build staged it under this name (docker needed)"
  exit 0
fi

CMD="$1"
shift

case "$CMD" in
  new)
    Rscript start_import.R "$@"
    ;;
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
    for arg in "$@"; do
      case "$arg" in
        --down) PRECMDS="${PRECMDS}source('helper_functions/redivis.R'); unlink(here::here('data','${DATASET}','raw_data'), recursive=TRUE); download_raw_data_redivis('${DATASET}'); " ;;
        --up) PRECMDS="${PRECMDS}external_peekbank_upload <- TRUE; " ;;
      esac
    done
    run "${PRECMDS}source('data/${DATASET}/import.R')"
    ;;
  ingest)
    if [ -z "$1" ]; then
      echo "Usage: ./pbi ingest <release-name> [--cache] [--dry-run]"
      exit 1
    fi
    RELEASE="$1"
    shift
    CACHE="FALSE"
    DRYRUN="FALSE"
    for arg in "$@"; do
      case "$arg" in
        --cache) CACHE="TRUE" ;;
        --dry-run) DRYRUN="TRUE" ;;
      esac
    done
    run "source('ingest/ingest.R'); ingest('${RELEASE}', cache = ${CACHE}, dry_run = ${DRYRUN})"
    ;;
  rename-release)
    if [ -z "$1" ]; then
      echo "Usage: ./pbi rename-release <release-name>"
      exit 1
    fi
    run "source('ingest/ingest.R'); rename_release('$1')"
    ;;
  release)
    if [ -z "$1" ]; then
      echo "Usage: ./pbi release <release-name>"
      exit 1
    fi
    run "source('ingest/ingest.R'); release_draft('$1')"
    ;;
  *)
    build
    docker compose run --rm "$COMPOSE_SERVICE" "$CMD" "$@"
    ;;
esac
