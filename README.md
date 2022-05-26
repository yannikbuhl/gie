# giedata

<!-- badges: start -->
[![R-CMD-check](https://github.com/yannikbuhl/giedata/workflows/R-CMD-check/badge.svg)](https://github.com/yannikbuhl/giedata/actions)
[![Lifecycle: experimental](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

A wrapper for Gas Infrastructure Europe's (GIE) AGSI+ (and soon also ALSI+) transparency platform API

## AGSI+ API update
Gas Infrastructure Europe (GIE) recently provided an update of its API to v5, introducing, amongst changes to parameters and endpoints, the use of pagination. This package provides easy-to-use functions to download small amounts of data or use pagination to download large amounts of (historical) data.

This package is currently under development and shall eventually be send to CRAN.

However, you can already use the first main function `giedata_getgasdata()` to download data from AGSI+ using pagination (if your request exceeds one page).

For example:

```r

gasdata <- giedata::get_giedata(country = "de",
                                from = "2022-01-01",
                                to = "2022-03-31",
                                size = 50,
                                verbose = TRUE,
                                apikey = apikey)

```

Also, the function `get_gielisting()` already works and returns a nested list with all gas facilities available, including the EIC codes for companies and facilities (to do so, do not set the 'region' and 'country' parameter and 'facilities' to `FALSE`. Also, it is possible to get parsed results for the region and country level. Setting the 'facilities' parameter to `TRUE`, the function returns all the available facilities within a country, including company information (including company and facility EICs). 

For example:

```r

german_facilities <- giedata::get_gielisting(region = "Europe",
                                             country = "Germany",
                                             facilities = TRUE,
                                             apikey = apikey)

```

Now, also a basic function is available that lets you download data for multiple countries, companies or facilities at once. Note, however, that as of yet, it is only possible to download a set of various companies for within one country; also, you can only download multiple facilities if the country and the company are fixed (this is mainly due to the way the API is built).

```r 

companydata <- giedata::get_giedatabulk()

```

