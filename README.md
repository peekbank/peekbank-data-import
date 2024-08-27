# peekbank-data-import

This repo contains `import.R` scripts for various datasets, allowing them to be preprocessed into the `peekds` format. 

## Prerequisites

If you want to upload data directly to osf using this pipeline, you need to authenticate with osf.

1. go to the [OSF token settings page](https://accounts.osf.io/login?service=https%3A%2F%2Fosf.io%2Fsettings%2Ftokens%2F)
2. Generate a token, name it anything you like, and make sure to give it `osf.full_write` and `osf.full_read`
3. create a file called `osf_token.txt` in the root of the repository and paste you token into it

## import process documentation

The `data/generic_import_template` folder contains some starting points.

Here's [an in-progress google doc guide](https://docs.google.com/document/d/1hQrbV33Zdl3SmbJAdTuzyfwCNF9nYpouQ7lQ8U0dQSw/edit) for importing.

Further information about the database schema is also linked in [Peekbank - START HERE](https://docs.google.com/document/d/1PrIrLg_A9VTITIp--ucf_wMN-C0VPODtirz8jowUL1Y/edit).

## quick reference for other repositories

[peekds](https://github.com/langcog/peekds) - readers for processing raw data

[peekbank](https://github.com/langcog/peekbank) - Django backend for database

[peekbankr](https://github.com/langcog/peekbankr) - R package for accessing database

[peekbank-shiny](https://github.com/langcog/peekbank-shiny) - shiny apps

[peekbank-website](https://github.com/langcog/peekbank-website) - website

