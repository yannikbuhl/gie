#' gie_load -- Main download function
#'
#' @description Function to download data from GIE's AGSI+ and ALSI+ API
#'
#' @param country Character. Specify the country of interest as two-digit country code (e.g., 'DE', 'IE').
#' @param company Character. EIC code for the requested company.
#' @param facility Character. EIC code for the requested facility.
#' @param from Character. Specify the start of the time span you are \cr
#' interested in downloading (format: YYYY-MM-DD).
#' @param to Character. Specify the end of the time span you are \cr
#' interested in downloading (format: YYYY-MM-DD).
#' @param date Character. If you want to have data only for one date. \cr
#' If you set 'date', you cannot set the 'from' and/or 'to' parameters \cr
#' (format: YYYY-MM-DD).
#' @param size Integer. The number of results per page.
#' @param timeout Numeric. If the amount of pages of your request exceeds 60, a timeout \cr
#' will be enforced to prevent the API from timing out. Defaults to 3 seconds, any \cr
#' values must be set in seconds, too.
#' @param database Character. The type of API you want to address ('agsi' or 'alsi').
#' @param verbose Logical. Prints information on function progress to the console (default: FALSE).
#' @param apikey Character. Your personal API key.
#'
#' @import dplyr httr lubridate purrr
#' @importFrom magrittr extract2 %>%
#'
#' @return A data.frame or list with the results.
#' @export
#'
#' @examples
#' \dontrun{
#' gie_load(country = "DE", date = "2022-01-03", database = "alsi")
#' }
#'
gie_load <- function(country,
                     company = NULL,
                     facility = NULL,
                     from = NULL,
                     to = NULL,
                     date = NULL,
                     size = 30,
                     timeout = 3,
                     database = "agsi",
                     verbose = FALSE,
                     apikey = Sys.getenv("GIE_APIKEY")) {


  # First step of error handling -----------------------------------------------

  if (missing(country)) stop("You have to at least specify the 'country' parameter (Error 1).",
                             call. = FALSE)

  check_giedatainput(country = country,
                     company = company,
                     facility = facility,
                     from = from,
                     to = to,
                     date = date,
                     size = size,
                     timeout = timeout,
                     database = database,
                     verbose = verbose,
                     apikey = apikey)

  # Execute first GET request --------------------------------------------------
  raw_results <- getrequest_general(database = database,
                                    target = "",
                                    page = 1,
                                    size = size,
                                    timeout = timeout,
                                    # pages = pages, # implicitly = NULL
                                    verbose = verbose,
                                    apikey = apikey,
                                    # Beginning of optional parameters
                                    country = country,
                                    company = company,
                                    facility = facility,
                                    from = from,
                                    to = to,
                                    date = date)

  # Get number of pages to the request
  pages <- raw_results[["last_page"]]

  #-----------------------------------------------------------------------------
  # Check if there was an empty response with 0 pages --------------------------

  if (pages == 0 | length(raw_results[["data"]]) == 0) {

    warning("No results found for your query (possibly). Invisibly returning raw return object (Error 2).",
            call. = FALSE)

    invisible(return(raw_results))

    stop(call. = FALSE)

  }

  #-----------------------------------------------------------------------------
  # If there is only one page, proceed -----------------------------------------
  if (pages == 1L) {

    if (isTRUE(verbose)) {

      message("~~~ There is only one page for your request, proceeding normally.")

    }

    # tryCatch if parsing goes wrong -------------------------------------------

    tryCatch(

      error = function(cnd) {

        # In case of error, warn and return raw results
        warning("!~~~ Parsing failed. Returning raw response (Error 3).", call. = FALSE)

        return(raw_results)

      },
      {

        # Parse results from GET request
        results <- parseresult(raw_results, database)

        # Return parsed results
        return(results)

      }

    )

    # End of tryCatch ----------------------------------------------------------

  } else if (pages > 1L) {

    if (isTRUE(verbose)) {

    message(paste0("~~~ Found ", pages, " pages. Downloading data."))

    }

    first_page <- parseresult(raw_results, database)

    if (pages > 60L & isTRUE(verbose)) {

      message(paste0("!~~~ Large request (total of ", pages, " pages), slowing down querying process by ", timeout, " seconds per API call. \n You can adjust this using the 'timeout' parameter."))

    }

    raw_results <- purrr::map(.x = c(2:pages),
                              .f = ~ getrequest_general(database = database,
                                                        target = "",
                                                        page = .x,
                                                        size = size,
                                                        timeout = timeout,
                                                        pages = pages,
                                                        verbose = verbose,
                                                        apikey = apikey,
                                                        # Beginnig of optional parameters
                                                        country = country,
                                                        company = company,
                                                        facility = facility,
                                                        from = from,
                                                        to = to,
                                                        date = date))

    if (isTRUE(verbose)) {

    message("~~~ Parsing raw request results.")

    }

    # tryCatch if parsing goes wrong -------------------------------------------

    tryCatch(

      error = function(cnd) {

        warning("!~~~ Parsing failed. Returning raw response (Error 4).", call. = FALSE)

        # Return raw remainder of request as well as the already parsed first page as list
        return(list(raw_results, parsed_first_page = first_page))

      },{

        results <- raw_results %>% purrr::map_dfr(.,
                                                  .f = ~ parseresult(.x,
                                                                     database))

        results <- dplyr::bind_rows(first_page, results)

        return(results)

      }

    )

    # End of tryCatch ----------------------------------------------------------

  }

}
