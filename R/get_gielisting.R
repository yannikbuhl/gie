#' get_gielisting
#'
#' @description Function to download raw or parsed results for the countries, \cr
#' companies and facilities available from the AGSI/ALSI+ API of GIE. The EIC codes \cr
#' of the results can be used to in turn download the actual data \cr
#' using \code{get_giedata()}.
#'
#' @param region Character. The broader region you want results for (can be 'Europe' or 'Non-EU').
#' @param country Character. The country you want the results for (must be the \cr
#' written-out name (e.g., "Germany"), NOT the two-digit country code). \cr
#'  If you use this parameter, you have to specify the 'region' parameter accordingly.
#' @param facilities Logical. If TRUE, facility data will be added to the country or company results. \cr
#' If you use this parameter, 'region' and 'country' have to be set. Defaults to FALSE.
#' @param database Character. The type of API you want to address ('agsi' or 'alsi'). \cr
#' As of yet, only 'agsi' works, 'alsi' support is expected at a later time.
#' @param apikey Character. Your personal API key.
#'
#' @return Data.frame with results
#' @export
#'
#' @examples
#' \dontrun{
#' get_gielisting(region = "Europe", country = "Germany", facilities = TRUE)
#' }
#'
get_gielisting <- function(region = NULL,
                           country = NULL,
                           facilities = FALSE,
                           database = "agsi",
                           apikey = Sys.getenv("GIE_APIKEY")) {

  # Error handling
  check_gielistinginput(region = region,
                        country = country,
                        facilities = facilities,
                        database = database,
                        apikey = apikey)

  endpoint <- paste0("https://", database, ".gie.eu/api/about")

  query <- list()

  url <- construct_url(url = endpoint, query = query)

  raw_request <- send_getrequest(url = url, apikey = apikey)

  raw_results <- raw_request %>% httr::content(as = "parsed")

  if (is.null(region) & is.null(country) & isFALSE(facilities)) {

    return(raw_results)

  } else {

    # Parse results according to region, country, company parameters
    results <- get_listinghierarchy(raw_results = raw_results,
                                    region = region,
                                    country = country,
                                    facilities = facilities)

    return(results)

  }
}
