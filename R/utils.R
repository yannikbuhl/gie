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
#' @param database
#' @param apikey
#'
#' @return
#' @import httr
#'
getrequest <- function(country,
                       company,
                       facility,
                       from,
                       to,
                       page,
                       pagelength = NULL,
                       date,
                       size,
                       type,
                       timeout,
                       database,
                       pages = NULL,
                       verbose,
                       apikey) {

  # Construct base URL
  endpoint <- paste0("https://", database, ".gie.eu/api")

  if (isTRUE(verbose)) {

    if (is.null(pages)) {

      message("~~~ Processing page number ", page, ".")

    } else if (!is.null(pages) & is.integer(pages)) {

      message("~~~ Processing page number ", page, " of ", pages, ".")

    }

  }

  # Create list with HTTP parameters
  query <- list(country = country,
                company = company,
                facility = facility,
                from = from,
                to = to,
                page = page,
                date = date,
                size = size,
                type = type)

  # Parse URL
  url <- construct_url(url = endpoint, query = query)

  # Execute GET request
  raw_request <- send_getrequest(url = url, apikey = apikey)

  if (!is.null(pagelength) && pagelength > 60) Sys.sleep(timeout)

  raw_results <- raw_request %>% httr::content(as = "parsed")

  return(raw_results)

}

## ---------------------------------------------------------------------------##

#' send_getrequest
#'
#' @param url
#' @param apikey
#'
#' @return
#'
send_getrequest <- function(url, apikey) {

  raw_request <- httr::GET(url, httr::add_headers(`x-key` = apikey))

  status <- httr::status_code(raw_request)
  error_message <- httr::http_status(raw_request)

  if (isTRUE(verbose) & status != 200) print(error_message)

  if (status != 200) stop("HTTP error with code: ", status, ".", call. = FALSE)

  return(raw_request)

}

## ---------------------------------------------------------------------------##

#' construct_url
#'
#' @param url
#' @param query
#'
#' @return
#'
construct_url <- function(url, query) {

  url <- httr::parse_url(url)

  url$query <- query

  return(url)

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
parseresult <- function(raw_results) {

  results <- raw_results %>%
    magrittr::extract2("data") %>%
    purrr::map(., ~ setnull(.x, c("info", "children"))) %>%
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
setnull <- function(data, x) {

    data[x] <- NULL
    return(data)

}

## ---------------------------------------------------------------------------##

#' Title
#'
#' @param country
#' @param company
#' @param facility
#' @param from
#' @param to
#' @param page
#' @param date
#' @param size
#' @param database
#' @param verbose
#' @param apikey
#'
#' @return
#'
check_giedatainput <- function(country,
                               company,
                               facility,
                               from,
                               to,
                               page,
                               date,
                               size,
                               type,
                               timeout,
                               database,
                               verbose,
                               apikey) {

  if (!is.null(company) & is.null(country)) {
    stop("If 'company' is specified, 'country' must be specified, too.")
  }

  if (!is.null(facility) & is.null(country) & is.null(company)) {
    stop("If 'facility' is specified, 'country' and 'company' must be specified, too.")
  }

  if (!is.logical(verbose) | length(verbose) != 1) {
    stop("Parameter 'verbose' needs to be type logical and length 1.")
  }

  if (!is.numeric(page) | length(page) != 1) {
    stop("Parameter 'page' needs to be type numeric and length 1.")
  }

  if (!is.numeric(size) | length(size) != 1 | size > 300) {
    stop("Parameter 'size' needs to be type numeric and length 1 and max. 300.")
  }

  if (!is.character(database) | length(database) != 1) {
    stop("Parameter 'type' needs to be type character and length 1.")
  }

  if (!is.numeric(timeout) | length(timeout) != 1) {
    stop("Parameter 'timeout' needs to be type character and length 1.")
  }
}

#------------------------------------------------------------------------------#

#' get_listinghierarchy
#'
#' @param raw_results
#' @param region
#' @param country
#' @param facilities
#'
#' @return
#'
get_listinghierarchy <- function(raw_results,
                                 region,
                                 country,
                                 facilities) {

  # Get all data from a region
  if (!is.null(region) & is.null(country) & isFALSE(facilities)) {

    results <- raw_results %>%
      pluck("SSO") %>%
      pluck(region) %>%
      map(.f = ~ map(.x, .f = ~ setnull(., c("facilities", "data", "image")))) %>%
      map_dfr(.f = bind_rows, .id = "country")

    return(results)

  } else if (!is.null(region) & !is.null(country) & isFALSE(facilities)) {

  # Get all data for a given country in a given region

    results <- raw_results %>%
      pluck("SSO") %>%
      pluck(region) %>%
      map(.f = ~ map(.x, .f = ~ setnull(., c("facilities", "data", "image")))) %>%
      map_dfr(.f = bind_rows, .id = "country") %>%
      filter(country == {{ country }})

    return(results)

  } else if (!is.null(region) & !is.null(country) & isTRUE(facilities)) {

    results <- raw_results %>%
      pluck("SSO") %>%
      pluck(region) %>%
      pluck(country) %>%
      map_dfr(.x, .f = ~ extract_listelements(.))

    return(results)

  } else {

    stop("Misspecified parameters. Please check parameter combination.",
         call. = FALSE)

  }

}

#------------------------------------------------------------------------------#

#' extract_listelements
#'
#' @param listelement
#'
#' @return
#'
extract_listelements <- function(listelement) {

  company <- tibble(country = listelement$data$country$name,
                    country_code = listelement$data$country$code,
                    company_shortname = listelement$short_name,
                    company_name = listelement$name,
                    company_url = listelement$url,
                    company_eic = listelement$eic)

  facilities <- listelement %>%
    setnull(., "data") %>%
    pluck(., "facilities") %>%
    map(.x, .f = ~ setnull(., "country")) %>%
    map_dfr(bind_rows) %>%
    mutate(company_eic = listelement$eic) %>%
    rename(facility_name = name,
           facility_eic = eic,
           facility_type = type)

  data <- facilities %>% left_join(., company, by = "company_eic")

  return(data)

}
