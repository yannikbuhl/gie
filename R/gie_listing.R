#' gie_listing -- Load info on companies and facilities
#'
#' @description Function to download raw or parsed results for the countries, \cr
#' companies and facilities available from the AGSI+/ALSI+ API of GIE. The EIC codes \cr
#' of the results can be used in turn to download the actual data \cr
#' using \code{gie_load()}.
#'
#' @param region Character. The broader region you want results for (can be 'Europe' or 'Non-EU').
#' @param country Character. The country you want the results for (must be the \cr
#' written-out name (e.g., "Germany"), NOT the two-digit country code). \cr
#'  If you use this parameter, you have to specify the 'region' parameter accordingly.
#' @param facilities Logical. If TRUE, facility data will be added to the country or company results. \cr
#' If you use this parameter, 'region' and 'country' have to be set. Defaults to FALSE.
#' @param database Character. The type of API you want to address ('agsi' or 'alsi').
#' @param apikey Character. Your personal API key.
#'
#' @return Data.frame with results
#' @export
#'
#' @examples
#' \dontrun{
#' gie_listing(region = "Europe", country = "Germany", facilities = TRUE)
#' }
#'
gie_listing <- function(region = NULL,
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

  # Create endpoint URL
  endpoint <- paste0("https://", database, ".gie.eu/api/about")

  # Create empty query list
  query <- list()

  # Construct URL
  url <- construct_url(url = endpoint, query = query)

  # Send HTTP request
  raw_request <- send_getrequest(url = url, apikey = apikey)

  # Extract raw results from HTTP response
  raw_results <- raw_request %>% httr::content(as = "parsed")

  # Parse results depending on database queried

  if (is.null(region) & is.null(country) & isFALSE(facilities)) {

    return(raw_results)

  } else if (database == "agsi") {

    # ALSI: Parse results according to region, country, company parameters
    results <- gie_listinghierarchy(raw_results = raw_results,
                                    region = region,
                                    country = country,
                                    facilities = facilities,
                                    database = database)

    return(results)

  } else if (database == "alsi") {

    # ALSI: Parse results according to region, country, company parameters
    results <- gie_listinghierarchy(raw_results = raw_results,
                                    region = region,
                                    country = country,
                                    facilities = facilities,
                                    database = database)

    return(results)

  } else {

    stop("Something went wrong parsing your results.", call. = FALSE)

  }
}
