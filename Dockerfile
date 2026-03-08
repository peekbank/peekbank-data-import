FROM rocker/r-ver:4.3.2

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libmariadb-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libtiff-dev \
    libjpeg-dev \
    cmake \
    git \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /project

COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

ENV RENV_CONFIG_PPM_ENABLED=TRUE
# Use prebuilt libarrow binaries on x86_64 instead of compiling from source
ENV LIBARROW_BINARY=true
ENV NOT_CRAN=true

# Cache mount persists compiled packages across builds so only new/changed
# packages need to recompile when renv.lock changes.
# CACHE_SYMLINKS=FALSE copies packages into the image instead of symlinking,
# since the cache mount is only available at build time.
RUN --mount=type=cache,target=/renv-cache \
    RENV_PATHS_CACHE=/renv-cache RENV_CONFIG_CACHE_SYMLINKS=FALSE \
    R -e "renv::restore(prompt = FALSE)"

COPY . .

CMD ["Rscript", "-e", "global_block_peekbank_summary <- TRUE; source('helper_functions/pipeline.R')"]
