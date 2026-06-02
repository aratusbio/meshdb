
<!-- README.md is generated from README.Rmd. Please edit that file -->

# meshdb

<!-- badges: start -->

<!-- badges: end -->

The meshdb package provides a tidy interface to the MeSH ontology, which
is stored in local `*.parquet` files.

## Installation

You can install the development version of meshdb like so:

``` r
remotes::install_github("aratusbio/meshdb")
```

## Configuration

Set the `MESHDB_PARQUET_PATH` environment variable to make your life
easy.

## Example

``` r
devtools::load_all(".")
#> ℹ Loading meshdb
mdb.dir <- checkmate::assert_directory_exists(Sys.getenv("MESHDB_PARQUET_PATH"))

mdb <- meshdb()
```

WIP: enter more here.
