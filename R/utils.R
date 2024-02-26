################################################################################
# Helper functions for {gie}
################################################################################

#-------------------------------------------------------------------------------
#' try_GET
#'
#' @param url URL for API call
#' @param apikey API key
#' @param ... Further parameter for httr::GET
#'
try_GET <- function(url, apikey, ...) {

  tryCatch(

    httr::GET(url,
              httr::add_headers(`x-key` = apikey),
              # httr::timeout(20), # Checking for timeouts here proved unstable
              ...),

    error = function(e) conditionMessage(e),

    warning = function(w) conditionMessage(w)


  )}

##----------------------------------------------------------------------------##

#' is_response
#'
#' @param x Object to check
#'
is_response <- function(x) {

  class(x) == "response"

}

## ---------------------------------------------------------------------------##

#' send_getrequest
#'
#' @param url URL for GET request
#' @param apikey API key
#' @param ... Further API parameters
#'
send_getrequest <- function(url, apikey, ...) {{

  # First check internet connection
  if (!curl::has_internet()) {

    message("There seems to be no internet connection.")

    return(invisible(NULL))

  }

  # Then try for timeout problems
  resp <- try_GET(url, apikey, ...)

  if (!is_response(resp)) {

    message(resp)

    return(invisible(NULL))

  }

  # Then stop if status > 400
  if (httr::http_error(resp)) {

    httr::message_for_status(resp)

    return(invisible(NULL))

  }

  # If everything is OK, return raw results
  raw_request <- httr::GET(url, httr::add_headers(`x-key` = apikey))

  return(raw_request)

}

  #-----------------------------------------------------------------------------
  # OLD CODE FOR send_getrequest

  # Check if there is an internet connection
  # if (!curl::has_internet()) {
  #
  #   message("There seems to be no internet connection.")
  #   return(invisible(NULL))
  #
  # }
  #
  # Check if the resource/API is available and the request successful
  # if (isTRUE(httr::http_error(httr::GET(url, httr::add_headers(`x-key` = apikey))))) {
  #
  #   raw_request <- httr::GET(url, httr::add_headers(`x-key` = apikey))
  #
  #   status <- httr::status_code(raw_request)
  #   error_description <- httr::http_status(raw_request)
  #
  #   error_message <- paste0("The API returned the HTTP error code ",
  #                           status,
  #                           ". The error message was: '",
  #                           error_description,
  #                           "'. In case of failure, function invisibly returns 'NULL'.")
  #
  #   message(error_message)
  #
  #   return(invisible(NULL))
  #
  # } else {
  #
  # raw_request <- httr::GET(url, httr::add_headers(`x-key` = apikey))
  #
  # return(raw_request)
  # }
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
#' @param database The database to be used
#'
parseresult <- function(raw_results,
                        database) {

  if (database == "agsi") {

  results <- raw_results %>%
    magrittr::extract2("data") %>%
    purrr::map(., ~ setnull(.x, c("info", "children"))) %>%
    purrr::map_dfr(.f = bind_rows) %>%
    dplyr::arrange(gasDayStart) %>%
    dplyr::mutate(gasDayStart = lubridate::as_date(gasDayStart),
           dplyr::across(!c(status,
                            gasDayStart,
                            updatedAt,
                            name,
                            code,
                            url),
                         as.numeric)) %>%
    suppressWarnings()

  } else if (database == "alsi") {

    results <- raw_results %>%
      magrittr::extract2("data") %>%
      purrr::map(., ~ setnull(.x, c("info", "children"))) %>%
      purrr::map(.f = unlist) %>%
      purrr::map_dfr(.f = bind_rows) %>%
      dplyr::arrange(gasDayStart) %>%
      dplyr::mutate(gasDayStart = lubridate::as_date(gasDayStart),
                    dplyr::across(!c(status,
                                     gasDayStart,
                                     updatedAt,
                                     name,
                                     code,
                                     url),
                                  as.numeric)) %>%
      suppressWarnings()

    names(results) <- purrr::map_chr(.x = names(results),
                                     .f = ~ stringr::str_replace(.x,
                                                                 "\\.(.)",
                                                                 function(match) {

                                                                   toupper(match[[1]])

                                                                 })) %>%
                      purrr::map_chr(.,
                                     .f = ~ stringr::str_replace(.x,
                                                                 "[.]",
                                                                 ""))

  } else {

    stop("Something went wrong parsing your raw results (Error 1).", call. = FALSE)

  }

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

  if (length(country) == 1 && nchar(country) != 2) {
    warning("The parameter value for 'country' does not have 2 characters. This might lead to you receiving no results, because gie_load() requires a two-character country code (e.g., 'DE' for Germany). Check the gie::countryinfo dataset for information on the country codes required.",
            call. = FALSE)
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

  if (!(database %in% c("agsi", "alsi"))) {
    stop("Incorrectly specified database name. Choose between 'agsi' or 'alsi'.",
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

  if (!is.null(country) && length(country) == 1 && nchar(country) == 2) {
    warning("The parameter value for 'country' does have 2 characters. This might lead to you receiving no results, because gie_listing() requires the whole country name (e.g., 'Germany'). Check the gie::countryinfo dataset for information on the country names required.",
            call. = FALSE)
  }

  if (!is.character(database) | length(database) != 1) {
    stop("Parameter 'database' needs to be type character and length 1.", call. = FALSE)
  }

  if (!(database %in% c("agsi", "alsi"))) {
    stop("Incorrectly specified database name. Choose between 'agsi' or 'alsi'.",
         call. = FALSE)
  }

  if (length(apikey) != 1) {
    stop("Parameter 'apikey' must be of length 1.", call. = FALSE)
  }

}

#------------------------------------------------------------------------------#

#' gie_listinghierarchy
#'
#' @param raw_results Raw results from API call
#' @param region Region to filter for
#' @param country Country to filter for
#' @param facilities Should facilties be exported as well
#' @param database Database to get info from
#'
gie_listinghierarchy <- function(raw_results,
                                 region,
                                 country,
                                 facilities,
                                 database) {

  # AGSI ---------------------------------------------------------------------

  if (database == "agsi") {

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

  # ALSI -----------------------------------------------------------------------

  } else if (database == "alsi") {

    # Get all data from a region
    if (!is.null(region) & is.null(country) & isFALSE(facilities)) {

      results <- raw_results %>%
        purrr::pluck("LSO") %>%
        purrr::pluck(region) %>%
        purrr::map(.f = ~ purrr::map(.x, .f = ~ setnull(., c("facilities", "data", "image")))) %>%
        purrr::map_dfr(.f = bind_rows, .id = "country")

      return(results)

    } else if (!is.null(region) & !is.null(country) & isFALSE(facilities)) {

      # Get all data for a given country in a given region

      results <- raw_results %>%
        purrr::pluck("LSO") %>%
        purrr::pluck(region) %>%
        purrr::map(.f = ~ purrr::map(.x, .f = ~ setnull(., c("facilities", "data", "image")))) %>%
        purrr::map_dfr(.f = bind_rows, .id = "country") %>%
        dplyr::filter(country == {{ country }})

      return(results)

    } else if (!is.null(region) & !is.null(country) & isTRUE(facilities)) {

      results <- raw_results %>%
        purrr::pluck("LSO") %>%
        purrr::pluck(region) %>%
        purrr::pluck(country) %>%
        purrr::map_dfr(.x, .f = ~ extract_listelements(.))

      return(results)

    } else {

      stop("Misspecified parameters. Please check parameter combination.",
           call. = FALSE)

    }

  } else {

    stop("Something went wrong specifying the 'database' parameter ('agsi' or 'alsi') or parsing your results.",
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

  if (length(purrr::pluck(listelement, "facilities")) != 0) {

    facilities <- listelement %>%
      setnull(., "data") %>%
      purrr::pluck(., "facilities") %>%
      purrr::map(.x, .f = ~ setnull(., "country")) %>%
      purrr::map_dfr(bind_rows) %>%
      dplyr::mutate(company_eic = listelement$eic) %>%
      dplyr::rename(facility_name = name,
                    facility_eic = eic,
                    facility_type = type)

  } else if (length(purrr::pluck(listelement, "facilities")) == 0) {

    # If there has been a empty 'facilities' field, create a data.frame with NAs
    facilities <- data.frame(facility_eic = NA,
                             facility_name = NA,
                             facility_type = NA,
                             operational_start_date = NA,
                             operational_end_date = NA,
                             company_eic = listelement$eic)

  } else {

    stop("There has been an error parsing your 'facilities = TRUE' call.",
         call. = FALSE)

  }

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

  if (!(database %in% c("agsi", "alsi"))) {
    stop("Incorrectly specified database name. Choose between 'agsi' or 'alsi'.",
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

#-------------------------------------------------------------------------------

#' check_gieunavinput
#'
#' @param country Passed from data function for check
#' @param start Passed from data function for check
#' @param end Passed from data function for check
#' @param type Passed from data function for check
#' @param end_flag Passed from data function for check
#' @param timeout Passed from data function for check
#' @param size Passed from data function for check
#' @param database Passed from data function for check
#' @param apikey Passed from data function for check
#'
check_gieunavinput <- function(country,
                               start,
                               end,
                               type,
                               end_flag,
                               timeout,
                               size,
                               database,
                               apikey) {

  if (!is.null(country) & (!is.character(country) | length(country) != 1)) {
    stop("Parameter 'country' needs to be type character and length 1.",
         call. = FALSE)
  }

  if (!is.null(start) & (!is.character(start) | length(start) != 1)) {
    stop("Parameter 'start' needs to be type character and length 1 (and format 'YYYY-MM-DD').",
         call. = FALSE)
  }

  if (!is.null(end) & (!is.character(end) | length(end) != 1)) {
    stop("Parameter 'end' needs to be type character and length 1 (and format 'YYYY-MM-DD').",
         call. = FALSE)
  }

  if (!is.null(type) && (!is.character(type) | length(type) != 1 | !(type %in% c("planned", "unplanned")))) {
    stop("Parameter 'type' needs to be type character, length 1 and either 'planned' or 'unplanned'.",
         call. = FALSE)
  }

  if (!is.null(end_flag) && (!is.character(end_flag) | length(end_flag) != 1 | !(end_flag %in% c("confirmed", "estimate")))) {
    stop("Parameter 'end_flag' needs to be type character, length 1 and either 'confirmed' or 'estimate'.",
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

  if (!(database %in% c("agsi", "alsi"))) {
    stop("Incorrectly specified database name. Choose between 'agsi' or 'alsi'.",
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

#-------------------------------------------------------------------------------

#' strip_html
#'
#' @description A function to HTML decode a character vector of length > 1
#'
#' @param string String to strip HTML tags from
#'
#' @return A vector with HTML decoded text
#'
strip_html <- function(string) {

  parsed <- map_chr(.x = string, .f = ~ html_text2(read_html(charToRaw(.))))

  return(parsed)

}

#-------------------------------------------------------------------------------

#' parse_unav
#'
#' @description A function to parse unavailability results
#'
#' @param raw_results Raw results
#'
#' @return A data.frame
#'
parse_unav <- function(raw_results) {

  results <- raw_results %>%
    purrr::pluck(., "data") %>%
    purrr::map(.x = .,
               .f = unlist) %>%
    purrr::map_dfr(.x = .,
                   .f = dplyr::bind_rows)

  names(results) <- purrr::map_chr(.x = names(results),
                                   .f = ~ stringr::str_replace(.x,
                                                               "\\.(.)",
                                                               function(match) {

                                                                 toupper(match[[1]])

                                                               })) %>%
                    purrr::map_chr(.x = .,
                                   .f = ~ stringr::str_replace(.x,
                                                               "[.]",
                                                               ""))

  return(results)

}

#-------------------------------------------------------------------------------

#' getrequest_general
#'
#' @param database Database name
#' @param target The API endpoint to target
#' @param page The page to retrieve
#' @param size The page size of the request
#' @param timeout Seconds to delay the batch request
#' @param pages Number of total pages of existing request
#' @param apikey API key
#' @param verbose Enable verbose mode
#' @param ... Further valid API parameters
#'
#' @return Raw results
#'
getrequest_general <- function(database,
                               target,
                               page,
                               size,
                               timeout,
                               pages = NULL,
                               apikey,
                               verbose = FALSE,
                               ...) {

  # Construct base URL
  endpoint <- paste0("https://", database, ".gie.eu/api/", target)

  #-----------------------------------------------------------------------------

  if (isTRUE(verbose)) {

    if (is.null(pages)) {

      message("~~~ Processing page number ", page, ".")

    } else if (!is.null(pages) & is.integer(pages)) {

      message("~~~ Processing page number ", page, " of ", pages, ".")

    }

  }

  #-----------------------------------------------------------------------------

  # Create list with HTTP parameters
  query <- list(page = page,
                size = size,
                ...)

  # Parse URL
  url <- construct_url(url = endpoint,
                       query = query)

  # Execute GET request
  raw_request <- send_getrequest(url = url,
                                 apikey = apikey)

  if (!is.null(pages) && pages > 60) Sys.sleep(timeout)

  raw_results <- raw_request %>% httr::content(as = "parsed")

  return(raw_results)

}

##----------------------------------------------------------------------------##
