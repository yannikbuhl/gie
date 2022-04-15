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
getrequest <- function(country,
                       company,
                       facility,
                       from,
                       to,
                       page,
                       date,
                       size,
                       type,
                       pages = NULL,
                       verbose,
                       apikey) {

  # Construct base URL
  endpoint <- paste0("https://", type, ".gie.eu/api")

  if (isTRUE(verbose)) {

    if (is.null(pages)) {

      message("~~~ Processing page number ", page, ".")

    } else if (!is.null(pages) & is.integer(pages)) {

      message("~~~ Processing page number ", page, " of ", pages, ".")

    }

  }

  # Parse URL
  url <- httr::parse_url(endpoint)

  # Create list with HTTP parameters
  query <- list(country = country,
                company = company,
                facility = facility,
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
  error_message <- httr::http_status(raw_request)

  if (isTRUE(verbose) & status != 200) print(error_message)

  if (status != 200) stop("HTTP error with code: ", status, ".", call. = FALSE)

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
parseresult <- function(raw_results) {

  results <- raw_results %>%
    magrittr::extract2("data") %>%
    purrr::map(., ~ setnull(.x, "info")) %>%
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
setnull <- function(data, x) {
  data[x] <- NULL
  return(data)
}

## ---------------------------------------------------------------------------##

check_giedatainput <- function(country,
                               company,
                               facility,
                               from,
                               to,
                               page,
                               date,
                               size,
                               type,
                               verbose,
                               apikey) {

  if (!is.null(company) & is.null(country)) stop()

  if (!is.null(facility & is.null(country)) | is.null(company)) stop()

  if (!is.logical(verbose)) stop()

  if (!is.numeric(page)) stop()

  if (!is.numeric(size)) stop()

  if (!is.character(type)) stop()

}
