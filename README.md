# peekbank-data-import

This repo contains `import.R` scripts for various datasets, allowing them to be preprocessed into the `peekds` format. 

## Prerequisites

We use `renv` to manage the package versions, so if you haven't already, install renv using:

```
install.packages("renv")
```

Install the used R packages into the environment using
```
renv::restore()
```

Whenever you install new packages (e.g. using `renv::install()`), be sure to freeze the package to the renv using

```
renv::snapshot()
```

If you want to upload data directly to osf using this pipeline, you need to authenticate with osf.

1. go to the [OSF token settings page](https://accounts.osf.io/login?service=https%3A%2F%2Fosf.io%2Fsettings%2Ftokens%2F)
2. Generate a token, name it anything you like, and make sure to give it `osf.full_write` and `osf.full_read`
3. create a file called `osf_token.txt` in the root of the repository and paste you token into it

## import process documentation

Here's [an in-progress google doc guide](https://docs.google.com/document/d/1hQrbV33Zdl3SmbJAdTuzyfwCNF9nYpouQ7lQ8U0dQSw/edit) for importing.

Further information about the database schema is also linked in [Peekbank - START HERE](https://docs.google.com/document/d/1PrIrLg_A9VTITIp--ucf_wMN-C0VPODtirz8jowUL1Y/edit).

## TODO: Usage: pipeline, new section on creating import scripts

## Troubleshooting

### gfortran on MacOS
If the package installation fails due to a missing `gfortran` library, make sure to update you R to >4.4.0 and visit [this page](https://cran.r-project.org/bin/macosx/tools/) to download and install `gfortran-XX.X-universal.pkg`