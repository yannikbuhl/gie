# giedata
A wrapper for Gas Infrastructure Europe's (GIE) AGSI+ API

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
                                verbose = TRUE)

```