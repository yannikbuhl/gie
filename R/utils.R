################################################################################
# Helper functions for {giedata}
################################################################################

#' Function to execute GET request
#'
#' @param country
#' @param from
#' @param to
#' @param page
#' @param date
#' @param size
#' @param type
#' @param apikey
#'
#' @return
#' @import httr
#'
#' @examples
giedata_getrequest <- function(country,
                               from,
                               to,
                               page,
                               date,
                               size,
                               type,
                               apikey) {

  # Construct base URL
  endpoint <- paste0("https://", type, ".gie.eu/api")

  message("~~~ Processing page number ", page)

  # Parse URL
  url <- httr::parse_url(endpoint)

  # Create list with HTTP parameters
  query <- list(country = country,
                from = from,
                to = to,
                page = page,
                date = date,
                size = size)

  # Update URL
  url$query <- query

  # Execute GET request
  raw_request <- httr::GET(url, httr::add_headers(`x-key` = apikey))

  status <- httr::status_code(raw_request)

  if (status != 200) stop("HTTP error with code: ", status, ".")

  raw_results <- raw_request %>% httr::content(as = "parsed")

  return(raw_results)

}

## ---------------------------------------------------------------------------##

#' Process results from GET request
#'
#' @param raw_results
#' @param country
#'
#' @return
#' @import dplyr purrr lubridate magrittr
#'
#' @examples
giedata_parseresult <- function(raw_results, country) {

  results <- raw_results %>%
    magrittr::extract2("data") %>%
    purrr::map(., ~ giedata_setnull(.x, "info")) %>%
    purrr::map_dfr(.f = bind_rows) %>%
    dplyr::arrange(gasDayStart) %>%
    dplyr::mutate(gasDayStart = lubridate::as_date(gasDayStart),
           across(!c(status, gasDayStart, name, code, url), as.numeric)) %>%
    suppressWarnings()

  return(results)

}

## ---------------------------------------------------------------------------##

#' Delete elements from list
#'
#' @param data
#' @param x
#'
#' @return
#'
#' @examples
giedata_setnull <- function(data, x) {
  data[x] <- NULL
  return(data)
}
