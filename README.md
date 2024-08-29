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

You can (re-)run singular import scripts inside their respective folders. It will automatically download the specific dataset from osf, run the import, run validation and (if the flag at the bottom is set) upload the processed data to OSF.

To run and validate the entirety of the import scripts (& download/upload all data), you can run the `run_all()` function in `pipeline.R`. It has parameters to control various aspects of process (e.g. should all data be redownloaded, should previous processed data be wiped, should the data be uploaded).

## Creating a new import script

The following steps will get you up and running with creating an import script for a new dataset.

### Gathering and uploading the raw data

We use an [OSF repository](https://osf.io/pr6wu/) for storing both the raw input data as well as the processed output data. 

When starting to import a new dataset, create a folder at the top level of the OSF repo named after the dataset identifier (e.g. `weaver_zettersten_2024`) - if no identifier exists yet, you can refer to the other datasets to get an idea about the format.

Next, create a `raw_data` folder in the 

Now put the dat. Usually, the paper has a github or osf repository with pointers to where the data is stored.

### Generating the import template

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

3. If your data is already in an appropiate osf, run the `import.R` script up to the `init()` function call to automatically download it. Otherwise, create a `raw_data` folder within your dataset specific folder and move your raw data there.

## Import Guide

The underlying `peekds` data format stores the eyetracking gazepoints and metadata in a set of tables where relationship are expressed through linked ids. As it can be a bit unwiedly to directly turn your data into this format, we have created a set of templates and functions to streamline the data importing process. The next few paragraphs will go over how to prepare your data for import using these helpers.

If you run into issues with the provided methods (e.g. memory limitations, edge cases not sufficiently mapped in our helper functions), you can also manually generate the tables and IDs in a "legacy" import script. Sparser instructions and help for this can be found [here](#import-guide-legacy-method).


TODO



## Import Guide (Legacy Method) 

Here's [an in-progress google doc guide](https://docs.google.com/document/d/1hQrbV33Zdl3SmbJAdTuzyfwCNF9nYpouQ7lQ8U0dQSw/edit) for importing.

Further information about the database schema is also linked in [Peekbank - START HERE](https://docs.google.com/document/d/1PrIrLg_A9VTITIp--ucf_wMN-C0VPODtirz8jowUL1Y/edit).
