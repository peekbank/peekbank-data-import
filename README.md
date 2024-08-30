# Peekbank Data Import

This repo contains the first part of the peekbank pipeline: A collection of `import.R` scripts for various looking-while-listening datasets, allowing them to be preprocessed into the `peekds` format.

## Repo structure reference

```
peekbank-data-import
│   ...
│   start_import.R - cli tool to create import template
└───data
│   │   ...
│   └───[DATASET_NAME] - one folder per dataset to import
│       │   raw_data/ - stores raw data from osf
│       │   processed_data/ - stores processed output of import
│       │   import.R - script to turn raw data into processed_data
│       │   README.md - info and import decisions for the dataset
│       │   [no_]cdi_indicated.txt - auto-generated file for pipeline
│       └───legacy
│           └───import_legacy.R - used to regression test the idless import helper
│   
└───helper_functions
    │   pipeline.R - code to run all import scripts
    │   pipeline_ignore.txt - list of dataset to skip in pipeline
    │   common.R - helpers to ensure repo structure
    │   idless_draft.R - functions to power idless importing
    │   osf.R - custom osfr replacements
```

## Prerequisites for running/creating imports

Two of the packages needed to work with this repo are not on CRAN, so you need to get them via GitHub:

```
remotes::install_github("peekbank/peekbankr")
```
```
remotes::install_github("peekbank/peekds")
```

You can install other packages that are needed as you go using `install.packages()` (renv has issues installing some of our dependecies, so this process is manual for now).

If you want to upload data directly to osf using this pipeline, you need to authenticate with osf.

1. Go to the [OSF token settings page](https://accounts.osf.io/login?service=https%3A%2F%2Fosf.io%2Fsettings%2Ftokens%2F)
2. Generate a token, name it anything you like, and make sure to give it `osf.full_write` and `osf.full_read`
3. Create a file called `osf_token.txt` in the root of the repository and paste you token into it

Having done that, you can now set a `upload=TRUE` flag when calling `write_and_validate` to automatically upload the processed files after successful validation.

## Running the existing import scripts

You can (re-)run singular import scripts inside their respective folders. It will automatically download the specific dataset from OSF, run the import, run validation and (if the flag at the bottom is set) upload the processed data to OSF.

To run and validate the entirety of the import scripts (& download/upload all data), you can run the `run_all()` function in `pipeline.R`. It has parameters to control various aspects of the process (e.g. should all data be re-downloaded, should previous processed data be wiped, should the data be uploaded).

## Creating a new import script

The following steps will get you up and running with creating an import script for a new dataset.

### Gathering and uploading the raw data

We use an [OSF repository](https://osf.io/pr6wu/) for storing both the raw input data as well as the processed output data. 

When starting to import a new dataset, create a folder at the top level of the OSF repository named after the dataset identifier (e.g. `weaver_zettersten_2024`) - if no identifier exists yet, you can refer to the other datasets to get an idea about the format.

Next, create a `raw_data` subdirectory in the OSF folder you just created and put the raw dataset into this folder. Usually, the corresponding paper has a GitHub or OSF repository with pointers to where the data can be found. 

### Generating the import template

Move your command line to your local copy of this repository (or clone it and cd into it).
To generate the import script template, run
```
Rscript start_import.R
```

in the root of the repository. If you know that you want to create a legacy import script, instead use

```
Rscript start_import.R --legacy
```


#### Creating an import script manually:

1. Create a folder named after your dataset in the `data/` directors (e.g. `data/weaver_zettersten_2024/`)

2. Copy over the `idless_template.R` from `helper_functions/` to the directory you just created and rename it to `import.R` (use `legacy_template.R` if you need to create a legacy import script)

3. If your data is already in a OSF folder names after the dataset, run the `import.R` script up to (&including) the `init()` function call to automatically download it. Otherwise, create a `raw_data` folder within your dataset-specific folder and move your raw data there.

## Import Guide

The underlying `peekds` data format stores the eyetracking gazepoints and metadata in a set of tables where relationships are expressed through linked IDs. As it can be unwieldy to directly turn your data into this format, we have created a set of templates and functions to streamline the data importing process. The next few paragraphs will go over how to prepare your data for import using these helpers.

If you run into issues with the provided methods (e.g. memory limitations, edge cases not sufficiently mapped in our helper functions), you can also manually generate the tables and IDs in a "legacy" import script. Sparser instructions and help for this can be found [here](#import-guide-legacy-method).

After following the steps above, check out your `import.R`. The template can be divided into 5 steps. 

### 1. Initial Setup

The first part of the script loads the required libraries/helper functions and downloads the raw dataset from osf (if not already downloaded). If you want to redownload the raw_data, delete the `raw_data/` folder and rerun the `init()` function. Naturally, put the libraries you use at the top.

### 2. Creating a wide.table

First, read in all relevant data from `raw_data` (the folder path is specified in `data_path`). 
The main portion of the import script will now be transforming this raw data into a big table that we will later feed into the digestion function.


This table should have one row per recorded gazepoint, with all of the associated metadata present (subject, trial information, stimuli, general metadata) in each row. This format has a high level of redundancy but is easy to reason about and can be easily split by our helper function.


Refer to the generated template for an overview of the columns that need to be present in this table. This [specification of the wide.table fields](https://docs.google.com/spreadsheets/d/1Z24n9vfrAmEk6_HpoTSh58LnkmSGmNJagguRpI1jwdo/edit?gid=516411759#gid=516411759) explains the contents of each column. Some columns are optional, so depending on your dataset, you might not have to specify all of them.

This is the part where you have to wrangle the original data, which might not always be obvious. 
Create a `README.md` in the datasets folder and fill it with any oddiies/ambiguities that you encounter during import and document the decisions you make.


### 3. Digest the wide.table

Pass the `wide.table` to the digestion function along with the `dataset_name`.
Also specify:
*  `lab_dataset_id`: internal name of the dataset by the original authors, can be `NA`
*  `cite`: Full length citation to the original publication containing the dataset (APA-Format)
*  `shortcite`: author (year) - APA-style in-text citation
*  `rezero`: `TRUE`, except if the t=0 already marks the start of trials or t is centered around the target onset
*  `normalize`: `TRUE`, except if the t=0 is already centered on the target word onset
*  `resample`: `TRUE` (until we find an edgec ase where this breaks things)

Explanation for rezero/normalize/resample:
Our final data format expects every trial to have t values centered around the point of disambiguation(target word onset) and timesteps to be resampled to 40Hz. To achieve this, the digest function runs the looking data through three manipulations:
* *Rezeroing*: Resets t to zero at the beginning of each trial
* *Normalization*: Subtracts the point of disambiguation from each timepoint
* *Resampling*: Resamples the timepoints to 40Hz

Normally you should leave all of these flags as `TRUE`, but they give you control over the time manipulation process in edge cases (e.g. your data is centered around the target onset already).

The function returns a list of 7 or 9 datasets containing the tables in `peekds` format. The specific tables can be accessed using
`dataset_list[["TABLE_NAME"]]`.

### 4. Aux Data

To attach arbitrarily shaped data to the peekds data tables, tables have special "*_aux_data" columns that house json strings. The output of the digestion function has `NA` values for all *_aux_data columns of the tables. If you have any aux data to add (CDI Data, Language Exposures, Other Language Measures) - this is the place to do it. 

As CDI is the most commonly imported aux data, there is a specific helper to ingest this type of data, which expects a long format table with one cdi rawscore per row. The template contains code that specifies the usage of the helper function and the exact shape of the input table. If you have more data or data other than cdi, refer to the next section.

#### Passing in other aux data

A more generalized helper function to turn nested data into the correct json output is on the roadmap, but you will have to manually populate the json strings for the aux data columns for now.

The required shape for the aux data json objects can be found [here](https://docs.google.com/spreadsheets/d/1Z24n9vfrAmEk6_HpoTSh58LnkmSGmNJagguRpI1jwdo/edit?gid=556307379#gid=556307379). For reference, you can check other `import.R` scripts that imported aux data or the `idless_draft.R` cdi helper function at the bottom.

When joining data together, be mindful that the meanings of some fields have changed in the output, e.g. what you entered as `subject_id` is now called `lab_subject_id`, while the new `subject_id` refers to the internal IDs of the `peekds` format (the renamings were done for clarity on the digestion side).

### 5. Write and Validate the Data (+ Upload)

In `write_and_validate_list()`, set `cdi_expected`to indicate whether you are importing any cdi data.

After you have finished all of the steps, run the entire import script. Some automated validation steps will be run, so check if the validator complains about any aspect of your data - adjust your import script accordingly. If some error messages are confusing, make sure to open a github issue so we can improve the feedback provided by the validator.

After the validation, a summary along with multiple plots about the dataset will be printed. Use these to sanity-check the data and cross-reference it with the original paper.

Once you are happy with the state of the data, upload the data by adding a `upload=TRUE` flag to the arguments of `write_and_validate_list()` and run it again.
Finally, manually upload your `README.md` to the OSF folder of the dataset.

## Import Guide (Legacy Method) 

Here's [an old in-progress google doc guide](https://docs.google.com/document/d/1hQrbV33Zdl3SmbJAdTuzyfwCNF9nYpouQ7lQ8U0dQSw/edit) for importing.

Further information about the database schema is also linked in [Peekbank - START HERE](https://docs.google.com/document/d/1PrIrLg_A9VTITIp--ucf_wMN-C0VPODtirz8jowUL1Y/edit).
