
#' get_gielisting
#'
#' @description Function to download raw or parsed results for the countries, \cr
#' companies and facilities available from the AGSI+ API of GIE. The EIC codes \cr
#' of the results can be used to in turn download the actual data \cr
#' using \code{get_giedata()}.
#'
#' @param show
#' @param region
#' @param country
#' @param facilities
#' @param type
#' @param apikey
#'
#' @return
#' @export
#'
#' @examples
#'
get_gielisting <- function(show = "listing",
                           region = NULL,
                           country = NULL,
                           facilities = FALSE,
                           type = "agsi",
                           apikey) {

  endpoint <- paste0("https://", type, ".gie.eu/api/about")

  query <- list(show = show)

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
