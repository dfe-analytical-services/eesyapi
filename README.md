# <a href="https://dfe-analytical-services.github.io/eesyapi/">eesyapi</a>

<!-- badges: start -->
[![R-CMD-check](https://github.com/dfe-analytical-services/eesyapi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dfe-analytical-services/eesyapi/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/dfe-analytical-services/eesyapi/branch/main/graph/badge.svg)](https://app.codecov.io/gh/dfe-analytical-services/eesyapi?branch=main)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

An R package to support analysts in connecting to and processing data from 
the DfE's Explore Education Statistics API endpoints.

## Installation

eesyapi is not currently available on CRAN. For the time being you can
install the development version from GitHub.

If you are using
[renv](https://rstudio.github.io/renv/articles/renv.html) in your
project (recommended):

``` r
renv::install("dfe-analytical-services/eesyapi")
```

Otherwise:

``` r
# install.packages("devtools")
devtools::install_github("dfe-analytical-services/eesyapi")
```

## Potential errors when installing

If you get `ERROR [curl: (22) The requested URL returned error: 401]`, and don't 
know why, try running `Sys.unsetenv("GITHUB_PAT")` to temporarily clear your 
GitHub PAT variable.

Then try to install again. 

If this works, then you will need to look for where that "GITHUB_PAT" variable 
is being set from and remove it to permanently fix the issue, contact us for 
support if you need help with this or have any other issues installing.

## Contributing

Ideas for eesyapi should first be raised as a [GitHub
issue](https://github.com/dfe-analytical-services/eesyapi) after which
anyone is free to write the code and create a pull request for review.

For more details on contributing to eesyapi, see our [contributing
guidelines](https://dfe-analytical-services.github.io/eesyapi/CONTRIBUTING.html).
