#' get_gielisting
#'
#' @description Get data on all available gas facilities from GIE
#'
#' @param show
#' @param apikey
#' @param type
#' @param raw Set to TRUE if no parsed but raw results desired. Default: FALSE.
#'
#' @return
#' @export
#'
#' @examples
#'
get_gielisting <- function(show = "listing",
                           region = NULL,
                           country = NULL,
                           company = NULL,
                           type = "agsi",
                           # raw = FALSE,
                           apikey) {

  endpoint <- paste0("https://", type, ".gie.eu/api/about")

  query <- list(show = show)

  url <- construct_url(url = endpoint, query = query)

  raw_request <- send_getrequest(url = url, apikey = apikey)

  raw_results <- raw_request %>% httr::content(as = "parsed")

  if (is.null(region) & is.null(country) & is.null(company)) {

    return(raw_results)

  } else {

    # Parse results according to region, country, company parameters
    results <- get_listinghierarchy(raw_results = raw_results,
                                    region = region,
                                    country = country,
                                    company = company)

    return(results)

  }
}
