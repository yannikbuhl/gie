################################################################################
# Helper functions for {giedata}
################################################################################

#' getrequest
#'
#' @param country Country to get data for
#' @param from Date for extraction start
#' @param to Date for extraction end
#' @param page Page number if multiple pages available
#' @param date Date for extraction
#' @param size Page size
#' @param database Database name
#' @param apikey API key
#' @param company Company EIC code
#' @param facility Facility EIC code
#' @param timeout Seconds to timeout if query is large
#' @param pages Total number of pages
#' @param verbose Verbose mode
#'
getrequest <- function(country,
                       company,
                       facility,
                       from,
                       to,
                       page,
                       date,
                       size,
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
                size = size)

  # Parse URL
  url <- construct_url(url = endpoint, query = query)

  # Execute GET request
  raw_request <- send_getrequest(url = url, apikey = apikey)

  if (!is.null(pages) && pages > 60) Sys.sleep(timeout)

  raw_results <- raw_request %>% httr::content(as = "parsed")

  return(raw_results)

}

## ---------------------------------------------------------------------------##

#' send_getrequest
#'
#' @param url URL for GET request
#' @param apikey API key
#'
send_getrequest <- function(url, apikey) {

  if (!curl::has_internet()) stop("There seems to be no internet connection.")

  raw_request <- httr::GET(url, httr::add_headers(`x-key` = apikey))

  status <- httr::status_code(raw_request)
  error_message <- httr::http_status(raw_request)

  if (status != 200) {

    print(paste0("There has been the following error message: ",
                 error_message))

    stop("The request was unsuccessful, the API returned HTTP error code ",
         status,
         ". More information can be found in the API error message printed above.",
         call. = FALSE)

  }

  # if (isTRUE(verbose) & status != 200) {}
  # if (status != 200) stop("HTTP error with code: ", status, ".", call. = FALSE)

  return(raw_request)

}

## ---------------------------------------------------------------------------##

#' construct_url
#'
#' @param url Base URL
#' @param query Query list
#'
construct_url <- function(url, query) {

  url <- httr::parse_url(url)

  url$query <- query

  return(url)

}

## ---------------------------------------------------------------------------##

#' parseresult
#'
#' @param raw_results Raw results from GET request
#'
parseresult <- function(raw_results) {

  results <- raw_results %>%
    magrittr::extract2("data") %>%
    purrr::map(., ~ setnull(.x, c("info", "children"))) %>%
    purrr::map_dfr(.f = bind_rows) %>%
    dplyr::arrange(gasDayStart) %>%
    dplyr::mutate(gasDayStart = lubridate::as_date(gasDayStart),
           dplyr::across(!c(status,
                            gasDayStart,
                            name,
                            code,
                            url), as.numeric)) %>%
    suppressWarnings()

  return(results)

}

## ---------------------------------------------------------------------------##

#' setnull
#'
#' @param data Data.frame or list element
#' @param x Name of element to delete
#'
setnull <- function(data, x) {

    data[x] <- NULL
    return(data)

}

## ---------------------------------------------------------------------------##

#' check_giedatainput
#'
#' @param country Passed from data function for check
#' @param company Passed from data function for check
#' @param facility Passed from data function for check
#' @param from Passed from data function for check
#' @param to Passed from data function for check
#' @param date Passed from data function for check
#' @param size Passed from data function for check
#' @param database Passed from data function for check
#' @param verbose Passed from data function for check
#' @param apikey Passed from data function for check
#' @param timeout Passed from data function for check
#'
check_giedatainput <- function(country,
                               company,
                               facility,
                               from,
                               to,
                               date,
                               size,
                               timeout,
                               database,
                               verbose,
                               apikey) {

  if ((!is.null(from) | !is.null(to)) & !is.null(date)) {
    warning("If 'from' and/or 'to' parameters are set and 'date', too, 'date' will override 'from' and/or 'to'.")
  }

  if (!is.character(country) | length(country) != 1) {
    stop("Parameter 'country' needs to be type character and length 1.",
         call. = FALSE)
  }

  if (!is.null(facility) & is.null(company) & !is.null(country)) {
    stop("If 'facility' and 'country' are specified, 'company' must be specified, too.",
         call. = FALSE)
  }

  if (!is.null(facility) & is.null(company)) {
    stop("If 'facility' is specified, 'company' must be specified, too.",
         call. = FALSE)
  }

  if (!is.logical(verbose) | length(verbose) != 1) {
    stop("Parameter 'verbose' needs to be type logical and length 1.",
         call. = FALSE)
  }

  if (!is.numeric(size) | length(size) != 1 | size > 300) {
    stop("Parameter 'size' needs to be type numeric and length 1 and max. 300.",
         call. = FALSE)
  }

  if (!is.character(database) | length(database) != 1) {
    stop("Parameter 'type' needs to be type character and length 1.",
         call. = FALSE)
  }

  if (database != "agsi") {
    stop("Currently, only 'agsi' is supported as database. 'alsi' support will be added later.",
         call. = FALSE)
  }

  if (!is.numeric(timeout) | length(timeout) != 1) {
    stop("Parameter 'timeout' needs to be type character and length 1.",
         call. = FALSE)
  }

  if (!is.character(apikey) | length(apikey) != 1) {
    stop("Parameter 'apikey' needs to be type character and length 1.",
         call. = FALSE)
  }

}

#------------------------------------------------------------------------------#

#' check_gielistinginput
#'
#' @param region Passed from listing function for check
#' @param country Passed from listing function for check
#' @param facilities Passed from listing function for check
#' @param database Passed from listing function for check
#' @param apikey Passed from listing function for check
#'
check_gielistinginput <- function(region,
                                  country,
                                  facilities,
                                  database,
                                  apikey) {

  if (!is.null(region)) {
    if (!is.character(region) | length(region) != 1) {
      stop("Parameter 'region' needs to be type character and length 1.", call. = FALSE)
    }
  }

  if (!is.null(region)) {
    if (!(region %in% c("Europe", "Non-EU"))) {
      stop("Parameter 'region' must be either 'Europe' or 'Non-EU'.", call. = FALSE)
    }
  }

  if (!is.logical(facilities) | length(facilities) != 1) {
    stop("Parameter 'facilities' needs to be type logical and length 1.", call. = FALSE)
  }

  if (!is.null(country)) {
    if (!is.character(country) | length(country) != 1) {
      stop("Parameter 'country' needs to be type character and length 1.", call. = FALSE)
    }
  }

  if (!is.character(database) | length(database) != 1) {
    stop("Parameter 'database' needs to be type character and length 1.", call. = FALSE)
  }

  if (database != "agsi") {
    stop("Currently, only 'agsi' is supported as database. 'alsi' support will be added later.", call. = FALSE)
  }

  if (length(apikey) != 1) {
    stop("Parameter 'apikey' must be of length 1.", call. = FALSE)
  }

}

#------------------------------------------------------------------------------#

#' get_listinghierarchy
#'
#' @param raw_results Raw results from API call
#' @param region Region to filter for
#' @param country Country to filter for
#' @param facilities Should facilties be exported as well
#'
get_listinghierarchy <- function(raw_results,
                                 region,
                                 country,
                                 facilities) {

  # Get all data from a region
  if (!is.null(region) & is.null(country) & isFALSE(facilities)) {

    results <- raw_results %>%
      purrr::pluck("SSO") %>%
      purrr::pluck(region) %>%
      purrr::map(.f = ~ purrr::map(.x, .f = ~ setnull(., c("facilities", "data", "image")))) %>%
      purrr::map_dfr(.f = bind_rows, .id = "country")

    return(results)

  } else if (!is.null(region) & !is.null(country) & isFALSE(facilities)) {

  # Get all data for a given country in a given region

    results <- raw_results %>%
      purrr::pluck("SSO") %>%
      purrr::pluck(region) %>%
      purrr::map(.f = ~ purrr::map(.x, .f = ~ setnull(., c("facilities", "data", "image")))) %>%
      purrr::map_dfr(.f = bind_rows, .id = "country") %>%
      dplyr::filter(country == {{ country }})

    return(results)

  } else if (!is.null(region) & !is.null(country) & isTRUE(facilities)) {

    results <- raw_results %>%
      purrr::pluck("SSO") %>%
      purrr::pluck(region) %>%
      purrr::pluck(country) %>%
      purrr::map_dfr(.x, .f = ~ extract_listelements(.))

    return(results)

  } else {

    stop("Misspecified parameters. Please check parameter combination.",
         call. = FALSE)

  }

}

#------------------------------------------------------------------------------#

#' extract_listelements
#'
#' @param listelement List element to extract from
#'
extract_listelements <- function(listelement) {

  company <- dplyr::tibble(country = listelement$data$country$name,
                    country_code = listelement$data$country$code,
                    company_shortname = listelement$short_name,
                    company_name = listelement$name,
                    company_url = listelement$url,
                    company_eic = listelement$eic)

  facilities <- listelement %>%
    setnull(., "data") %>%
    purrr::pluck(., "facilities") %>%
    purrr::map(.x, .f = ~ setnull(., "country")) %>%
    purrr::map_dfr(bind_rows) %>%
    dplyr::mutate(company_eic = listelement$eic) %>%
    dplyr::rename(facility_name = name,
           facility_eic = eic,
           facility_type = type)

  data <- facilities %>% dplyr::left_join(., company, by = "company_eic")

  return(data)

}

#------------------------------------------------------------------------------#

#' check_giedata2input
#'
#' @param countries Passed from data function for check
#' @param companies Passed from data function for check
#' @param facilities Passed from data function for check
#' @param from Passed from data function for check
#' @param to Passed from data function for check
#' @param date Passed from data function for check
#' @param size Passed from data function for check
#' @param database Passed from data function for check
#' @param verbose Passed from data function for check
#' @param apikey Passed from data function for check
#' @param timeout Passed from data function for check
#'
check_giedata2input <- function(countries,
                                companies,
                                facilities,
                                from,
                                to,
                                date,
                                size,
                                timeout,
                                database,
                                verbose,
                                apikey) {

  if ((!is.null(from) | !is.null(to)) & !is.null(date)) {
    warning("If 'from' and/or 'to' parameters are set and 'date', too, 'date' will override 'from' and/or 'to'.",
            call. = FALSE)
  }

  if (!is.null(facilities) & is.null(companies)) {
    stop("If 'facilities' is specified, 'companies' must be specified, too.",
         call. = FALSE)
  }

  if (!is.null(facilities) & length(companies) != 1) {
    stop("If 'facilities' is specified, 'companies' must only contain one company EIC.",
         call. = FALSE)
  }

  if (!is.logical(verbose) | length(verbose) != 1) {
    stop("Parameter 'verbose' needs to be type logical and length 1.",
         call. = FALSE)
  }

  if (!is.numeric(size) | length(size) != 1 | size > 300) {
    stop("Parameter 'size' needs to be type numeric and length 1 and max. 300.",
         call. = FALSE)
  }

  if (!is.character(database) | length(database) != 1) {
    stop("Parameter 'type' needs to be type character and length 1.",
         call. = FALSE)
  }

  if (database != "agsi") {
    stop("Currently, only 'agsi' is supported as database. 'alsi' support will be added later.",
         call. = FALSE)
  }

  if (!is.numeric(timeout) | length(timeout) != 1) {
    stop("Parameter 'timeout' needs to be type character and length 1.",
         call. = FALSE)
  }

  if (!is.character(apikey) | length(apikey) != 1) {
    stop("Parameter 'apikey' needs to be type character and length 1.",
         call. = FALSE)
  }

}
