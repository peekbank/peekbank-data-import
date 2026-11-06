# Ingestion

`ingest.R` builds a release of the Peekbank database from every dataset that has been reviewed, i.e. whose folder holds no `ignored_by_pipeline_remove_after_review.txt`, and stages it in the draft of the [Peekbank dataset](https://stanford.redivis.com/datasets/a3v0-3drqa89kg) on Redivis. `./pbi release` then releases the draft.

## You need

- Docker and a Redivis token with the `data.edit` scope, set up as described in the [main README](../README.md#docker-optional)
- edit access to the [Peekbank dataset](https://stanford.redivis.com/datasets/a3v0-3drqa89kg) on Redivis
- edit access to [`peekbank_files`](https://stanford.redivis.com/datasets/frvk-5c922jwkx) on Redivis
- read access to [`peekbank_private_raw`](https://stanford.redivis.com/datasets/2vqs-2cfz9magg) on Redivis

Releases are deliberately only built in Docker to pin the R version and packages they are built with.

## Building a release

From the root of the repository:

```bash
./pbi ingest <release-name>              # reruns every import and then ingests and stages the data
./pbi ingest <release-name> --dry-run    # build the tables, but stage and upload nothing
./pbi ingest <release-name> --cache      # use the processed data uploaded to Redivis instead of rerunning the imports
```

We name releases `<YEAR>.<Incrementing Number>`. A name can only be used once, and `current` and `next` are reserved to refer to the latest and draft releases.

The full build reruns every import and records the commit it was built from, which has to be the latest commit on master. Afterwards it uploads the processed data that changed to the `_processed_data` folder of `peekbank_files` for easier dev iteration.

`--cache` builds from the processed data in `_processed_data` instead. It is quicker for testing, but `./pbi release` specifically refuses its draft, as we want releases to be linked to a state of the import framework and raw data for reproducibility and the ability to trace errors in releases.

Both stage the release tables in the draft on Redivis, along with the file table `dataset_files`, which holds every dataset's `README.md` and raw data as `<dataset>/README.md` and `<dataset>/raw_data/<path>`. Redivis can't delete single files, so when a file of the previous release is not part of the new one (a dataset was dropped or renamed, or a raw file removed or renamed), the build creates `dataset_files` anew and uploads all files again, which takes about an hour and a half.

`--dry-run` stops before staging and does not have the fixed commit requirement of the regular ingestion. Use it to test a change to the imports, the helpers or peekbankr before committing it.

## Private raw data

Raw data that may not be shared publicly is in [`peekbank_private_raw`](https://stanford.redivis.com/datasets/2vqs-2cfz9magg) instead. For these datasets, the release holds only the `README.md` and a `raw_data/raw_data_in_private_repo.txt` naming the version of `peekbank_private_raw` with their raw data. `release_info` records that version in `private_raw_data_version`, so the release points at fixed files for (internal) reproducability. That is why the build stops while `peekbank_private_raw` has a draft. To unblock, release the draft of the private data on Redivis first.

## Releasing

```bash
./pbi release <release-name>
```

This only releases a draft that a finished full build staged under this name. The `Review and release` button on Redivis is discouraged as it skips this check.

To rename the staged release without building it again:

```bash
./pbi rename-release <new-release-name>
```
