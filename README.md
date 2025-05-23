
# gie <img src="man/figures/hexsticker_gie.png" width="160px" align="right" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/yannikbuhl/gie/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yannikbuhl/gie/actions/workflows/R-CMD-check.yaml)
[![CRAN_Download_Badge](https://cranlogs.r-pkg.org/badges/gie)](https://cran.r-project.org/package=gie)
<!-- badges: end -->

A R wrapper for Gas Infrastructure Europe’s (GIE) AGSI+ and ALSI+
transparency platform API (also supports getting Urgent Market Messages
from Inside Information Platform, IIP)

## Collecting data on natural gas storages

[Gas Infrastructure Europe (GIE)](https://www.gie.eu/) is an
organisation that represents the vast majority of operators of natural
gas storage units in Europe and some non-EU countries (such as the
United Kingdom and Ukraine). They provide a REST API for their AGSI+
transparency platform to retrieve data on country level, operator level
and facility level. Data include information on, e.g., the filling
level, overall capacity, inflow or outflow. In order to use `{gie}`, you
have to register for an API key on the [AGSI+
website](https://agsi.gie.eu/).

In spring of 2022, Gas Infrastructure Europe (GIE) provided a complete
makeover of its API, introducing, among changes to parameters and
endpoints, the use of pagination. This package provides easy-to-use
functions to download small amounts of data or use pagination to
download large amounts of (historical) data. Also, it includes a
function to get metadata on operators and facilities across Europe and
within each country.

Get the package from CRAN:

``` r
install.packages("gie")
```

Install the development version via GitHub:

``` r
remotes::install_github("yannikbuhl/gie")
```

## Usage

The function `gie_listing()` returns a nested list with all gas
facilities available, including the EIC codes for companies and
facilities (to do so, do not set the ‘region’ and ‘country’ parameter
and ‘facilities’ to `FALSE`). Also, it is possible to get parsed results
(as `data.frame`) for the region and country level. Setting the
‘facilities’ parameter to `TRUE`, the function returns all the available
facilities within a country (including company and facility EIC codes
for further use in the download functions `gie_load()` and
`gie_batchload()`).

``` r
german_facilities <- gie::gie_listing(region = "Europe",
                                      country = "Germany",
                                      facilities = TRUE,
                                      apikey = apikey)
```

One main function for data download is called `gie_load()` and can be
used to download data from AGSI+, either for one country, for one
operator in a certain country or for a certain facility (see
documentation for info on all parameters).

``` r
gasdata <- gie::gie_load(country = "de",
                         from = "2022-01-01",
                         to = "2022-03-31",
                         size = 50,
                         verbose = TRUE,
                         apikey = apikey)
```

The second download function `gie_batchload()` lets you download data
for multiple countries, companies or facilities at once. Note, however,
that as of yet, it is only possible to download a set of various
companies for within one country; also, you can only download multiple
facilities if the country and the company are fixed (due to the way the
API is built).

``` r
companydata <- gie::gie_batchload(countries = c("DE", "IE", "NL"), 
                                  from = "2022-01-01",
                                  apikey = apikey)
```

## Development

If you encounter problems or if you have suggestions for improvement,
let me know or, ideally, open an issue. Also, I am looking forward to
pull requests.

## Disclaimer

This package has been developed independently of and is not in any way
associated to GIE or the AGSI/ALSI+ transparency platform. Some parts of
or calls to the API may exhibit unforeseen behaviour due to the API
still being subject to changes. The hex sticker to this package has been
generated with the use of OpenAI’s image generation.
