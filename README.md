# Peekbank Data Import

This repo contains the first part of the peekbank pipeline: A collection of `import.R` scripts that process various looking-while-listening datasets into the Peekbank format.

## Setup

To begin the setup, clone the repository and move into it:

```bash
git clone https://github.com/peekbank/peekbank-data-import.git
cd peekbank-data-import
```

### Redivis authentication

Raw and processed data live in the [`datapages.peekbank_files`](https://stanford.redivis.com/datasets/frvk-5c922jwkx) dataset on [Redivis](https://redivis.com). Downloading raw data needs a Redivis login, and uploading processed data additionally requires write access to the dataset. When you run an import from R, it opens a browser login for you to authenticate. Docker and server runs need a token instead (see [Docker](#docker-optional)).

### Installing R dependencies

This project uses [renv](https://rstudio.github.io/renv/) to manage R package dependencies. All required packages (including `peekbankr` from GitHub) are pinned in `renv.lock`.

To set up your environment:

1. Open the project in RStudio (renv activates automatically via `.Rprofile`)
2. Install all dependencies from the lockfile:
```r
renv::restore()
```

### Docker (optional)

Docker is only needed for `./pbi import` and `./pbi pipeline`, which run the imports inside a container. Install [Docker](https://docs.docker.com/get-started/get-docker/) and build the image once (and rebuild whenever the code changes):

```bash
docker compose build
```

The container can't open the Redivis browser login, so it needs an API token. The same goes for any run where nobody can complete the login, e.g. on a server.

1. On Redivis, go to Workspace → Settings → API tokens and create a token. Give it the `data.edit` scope if you want to upload.
2. Copy `.env.template` to `.env` in the root of the repository and paste the token after `REDIVIS_API_TOKEN=`.

`.env` is gitignored. Local R sessions pick it up too and use it instead of the browser login.

## Adding a new dataset

The following steps will get you up and running with creating an import script for a new dataset.

### Gathering and uploading the raw data

The section ["Add raw data to be imported"](https://peekbank.github.io/peekbank-website/contribute/tasks/#add-raw-data-to-be-imported) in the Peekbank docs describes how to add the raw data to our data source repository.

### Generating the import template

To generate the import script template, run
```
./pbi new
```

in the root of the repository.

Without bash (e.g. on Windows without WSL), run `Rscript start_import.R` instead.


If you specifically know that you want to create a legacy import script (unlikely when starting out), instead use

```
./pbi new --legacy
```

(or `Rscript start_import.R --legacy` if you can't use bash).


### Writing the import script

The underlying <a href="https://peekbank.github.io/peekbank-website/peekbank/dataschema/" target="_blank">Peekbank data format</a> stores the eyetracking gazepoints and metadata in a set of tables where relationships are expressed through linked IDs. As it can be unwieldy to directly turn your data into this format, we have created a set of templates and functions to streamline the data importing process. The next few paragraphs will go over how to prepare your data for import using these helpers.

If you run into issues with the provided methods (e.g. memory limitations, edge cases not sufficiently mapped in our helper functions), you can also manually generate the tables and IDs in a "legacy" import script (create a new template with the command above). We do provide [sparse instructions and help](#import-guide-legacy-method).

After running the `./pbi new` command and choosing a name, check out your newly created `import.R`. The template can be divided into 5 steps. 

#### 1. Initial Setup

The first part of the script loads the required libraries/helper functions and downloads the raw dataset from Redivis (if not already downloaded). If you want to redownload the raw_data, delete the `raw_data/` folder and rerun the `init()` function. Naturally, put the libraries you use at the top.

#### 2. Creating a wide.table

First, read in all relevant data from `raw_data` (the folder path is specified in `data_path`). 
The main portion of the import script will now be transforming this raw data into a big table that we will later feed into the digestion function.


This table should have one row per recorded gazepoint, with all of the associated metadata present (subject, trial information, stimuli, general metadata) in each row. This format has a high level of redundancy but is easy to reason about and can be easily split by our helper function.


Refer to the generated template for an overview of the columns that need to be present in this table. This [specification of the wide.table fields](https://docs.google.com/spreadsheets/d/1Z24n9vfrAmEk6_HpoTSh58LnkmSGmNJagguRpI1jwdo/edit?gid=516411759#gid=516411759) explains the contents of each column. Some columns are optional, so depending on your dataset, you might not have to specify all of them.

This is the part where you have to wrangle the original data, which might not always be obvious. 
Fill in the `README.md` in the datasets folder with any oddities/ambiguities that you encounter during import and document the decisions you make.


#### 3. Digest the wide.table

Pass the `wide.table` to the digestion function along with the `dataset_name`.
Also specify:
*  `lab_dataset_id`: internal name of the dataset by the original authors, can be `NA`
*  `cite`: Full length citation to the original publication containing the dataset (APA-Format)
*  `shortcite`: author (year) - APA-style in-text citation
*  `rezero`: `TRUE`, except if the t=0 already marks the start of trials or t is centered around the target onset
*  `normalize`: `TRUE`, except if the t=0 is already centered on the target word onset
*  `resample`: `TRUE` (until we find an edge case where this breaks things)

Explanation for rezero/normalize/resample:
Our final data format expects every trial to have t values centered around the point of disambiguation (target word onset) and timesteps to be resampled to 40Hz. To achieve this, the digest function runs the looking data through three manipulations:
* *Rezeroing*: Resets t to zero at the beginning of each trial
* *Normalization*: Subtracts the point of disambiguation from each timepoint
* *Resampling*: Resamples the timepoints to 40Hz

Normally you should leave all of these flags as `TRUE`, but they give you control over the time manipulation process in edge cases (e.g. your data is centered around the target onset already).

The function returns a list of 7 or 9 datasets containing the tables in `peekbankr` format. The specific tables can be accessed using
`dataset_list[["TABLE_NAME"]]`.

#### 4. Aux Data

To attach arbitrarily shaped data to the peekbankr data tables, tables have special "*_aux_data" columns that house json strings. The output of the digestion function has `NA` values for all *_aux_data columns of the tables. If you have any aux data to add (CDI Data, Language Exposures, Other Language Measures), this is the place to do it. 

As CDI is the most commonly imported aux data, there is a specific helper to ingest this type of data, which expects a long format table with one cdi rawscore per row. The template contains code that specifies the usage of the helper function and the exact shape of the input table. If you have more data or data other than cdi, refer to the next section.

##### Passing in other aux data

A more generalized helper function to turn nested data into the correct json output is on the roadmap, but you will have to manually populate the json strings for the aux data columns for now.

The required shape for the aux data json objects can be found [here](https://docs.google.com/spreadsheets/d/1Z24n9vfrAmEk6_HpoTSh58LnkmSGmNJagguRpI1jwdo/edit?gid=556307379#gid=556307379). For reference, you can check other `import.R` scripts that imported aux data or the `idless_draft.R` cdi helper function at the bottom.

When joining data together, be mindful that the meanings of some fields have changed in the output, e.g. what you entered as `subject_id` is now called `lab_subject_id`, while the new `subject_id` refers to the internal IDs of the `peekbankr` format (the renamings were done for clarity on the digestion side).

#### 5. Write and Validate the Data (+ Upload)

In `write_and_validate_list()`, set `cdi_expected` to indicate whether you are importing any cdi data.

After you have finished all of the steps, run the entire import script. Some automated validation steps will be run, so check if the validator complains about any aspect of your data - adjust your import script accordingly. If some error messages are confusing, make sure to open a github issue so we can improve the feedback provided by the validator.

After the validation, a summary along with multiple plots about the dataset will be printed. Use these to sanity-check the data and cross-reference it with the original paper.

Once you are happy with the state of the data, upload the data by adding a `upload=TRUE` flag to the arguments of `write_and_validate_list()` and run it again.

## Managing dependencies

### Adding or updating packages

If you need a new package for an import script:
```r
install.packages("name_of_new_package")  # or renv::install("user/repo") for GitHub packages
renv::snapshot()                 # update renv.lock, then commit the updated lockfile
```

### Updating peekbankr

`peekbankr` is also pinned to a specific git commit in `renv.lock`. After making changes to the [peekbankr repo](https://github.com/peekbank/peekbankr), run the following to pull the latest version:
```r
renv::install("peekbank/peekbankr")
renv::snapshot()
```
Then commit the updated `renv.lock`.

## Running existing imports

### Locally

You can (re-)run singular import scripts inside their respective folders. It will automatically download the specific dataset from Redivis, run the import, run validation and (if the flag at the bottom is set) upload the processed data to Redivis.

To run the full pipeline, source `pipeline.R` and call `run_all()`. You can also uncomment the call at the bottom of `pipeline.R` and run the file directly.
```r
source("helper_functions/pipeline.R")
run_all(nocache = FALSE, clean = TRUE, upload = FALSE)
```
- `nocache=TRUE`: redownload all raw data from Redivis
- `clean=TRUE`: delete previous processed_data before running
- `upload=TRUE`: upload results to Redivis after validation passes

### With Docker (best when running the pipeline end to end)

With the image built and the token set (see [Docker](#docker-optional)), use the `pbi` wrapper script:

```bash
./pbi pipeline                    # run all imports
./pbi pipeline --down             # redownload all raw data
./pbi pipeline --resume           # keep previous processed_data
./pbi pipeline --up               # upload results to Redivis
./pbi pipeline --no-subprocess    # run all imports in a single R process
./pbi import adams_marchman_2018  # run a single dataset
./pbi import adams_marchman_2018 --down --up  # redownload + upload
```

## Import Guide (Legacy Method) 

Here's [an old in-progress google doc guide](https://docs.google.com/document/d/1hQrbV33Zdl3SmbJAdTuzyfwCNF9nYpouQ7lQ8U0dQSw/edit) for importing.

Further information about the database schema is also linked in [Peekbank - START HERE](https://docs.google.com/document/d/1PrIrLg_A9VTITIp--ucf_wMN-C0VPODtirz8jowUL1Y/edit).

## Repo structure reference

```
peekbank-data-import
│   ...
│   pbi - command line wrapper (./pbi new, ./pbi import, ./pbi pipeline)
│   start_import.R - creates the import template, run through ./pbi new
│   pipeline_registry.csv - auto-generated, tracks per-dataset validation settings (gitignored)
└───data
│   │   ...
│   └───[DATASET_NAME] - one folder per dataset to import
│       │   raw_data/ - stores raw data from Redivis
│       │   processed_data/ - stores processed output of import
│       │   import.R - script to turn raw data into processed_data
│       │   README.md - info and import decisions for the dataset
│       │
│       └───legacy
│           └───import_legacy.R - used to regression test the idless import helper
│   
└───helper_functions
    │   pipeline.R - code to run all import scripts
    │   pipeline_ignore.txt - list of dataset to skip in pipeline
    │   common.R - helpers to ensure repo structure
    │   idless_draft.R - functions to power idless importing
    │   redivis.R - download and upload helpers for Redivis
```
