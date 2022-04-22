#' Function to download data from GIE's AGSI+ API
#'
#' @param country Character. Specify the country of interest.
#' @param company Character. EIC code for the requested company.
#' @param facility Character. EIC code for the requested facility.
#' @param from Character. Specify the start of the time span you are \cr
#' interested in downloading (format: YYYY-MM-DD).
#' @param to Character. Specify the end of the time span you are \cr
#' interested in downloading (format: YYYY-MM-DD).
#' @param page Integer. The page of a multi-page query you want to get.
#' @param date Character. If you want to have data only for one date. \cr
#' If you set 'date', you cannot set the 'from' and/or 'to' parameters \cr
#' (format: YYYY-MM-DD).
#' @param size Integer. The number of results per page.
#' @param database Character. The type of API you want to address ('agsi' or 'alsi').
#' @param apikey Character. Your personal API key.
#'
#' @return A data.frame or list with the results.
#' @import magrittr httr
#' @export
#'
#' @examples
get_giedata <- function(country = NULL,
                        company = NULL,
                        facility = NULL,
                        from = NULL,
                        to = NULL,
                        page = 1,
                        date = NULL,
                        size = 30,
                        type = NULL,
                        database = "agsi",
                        verbose = FALSE,
                        apikey) {

  if (missing(apikey)) stop("You need to specify an 'apikey' parameter.")

  # First step of error handling -----------------------------------------------
  check_giedatainput(country = country,
                     company = company,
                     facility = facility,
                     from = from,
                     to = to,
                     page = page,
                     date = date,
                     size = size,
                     type = type,
                     database = database,
                     verbose = verbose,
                     apikey = apikey)

  # Execute first GET request --------------------------------------------------
  raw_results <- getrequest(country = country,
                            company = company,
                            facility = facility,
                            from = from,
                            to = to,
                            page = page,
                            date = date,
                            size = size,
                            type = type,
                            database = database,
                            verbose = verbose,
                            apikey = apikey)

  # Get number of pages to the request
  pages <- raw_results[["last_page"]]

  # Check if there was an empty response with 0 pages --------------------------
  if (pages == 0) {

    warning("No results found for your query. Invisibly returning raw results.",
            call. = FALSE)

    invisible(return(raw_results))

    stop(call. = FALSE)

  }

  # If there is only one page, proceed -----------------------------------------
  if (pages == 1L) {

    if (isTRUE(verbose)) {

      message("~~~ There is only one page for your request, proceeding normally.")

    }

    # tryCatch if parsing goes wrong -------------------------------------------

    tryCatch(

      error = function(cnd) {

        # In case of error, warn and return raw results
        warning("!~~~ Parsing failed. Returning raw response.", call. = FALSE)

        return(raw_results)

      },
      {

        # Parse results from GET request
        results <- parseresult(raw_results)

        # Return parsed results
        return(results)

      }

    )

    # End of tryCatch ----------------------------------------------------------

  } else if (pages > 1L) {

    if (isTRUE(verbose)) {

    message(paste0("~~~ Found ", pages, " pages. Downloading data."))

    }

    first_page <- parseresult(raw_results)

    raw_results <- purrr::map(c(2:pages),
                              .f = ~ getrequest(country = country,
                                                company = company,
                                                facility = facility,
                                                from = from,
                                                to = to,
                                                page = .x,
                                                date = date,
                                                size = size,
                                                type = type,
                                                database = database,
                                                pages = pages,
                                                verbose = verbose,
                                                apikey = apikey))

    if (isTRUE(verbose)) {

    message("~~~ Parsing raw request results.")

    }

    # tryCatch if parsing goes wrong -------------------------------------------

    tryCatch(

      error = function(cnd) {

        warning("!~~~ Parsing failed. Returning raw response.", call. = FALSE)

        return(raw_results)

      },{

        results <- raw_results %>% purrr::map_dfr(., .f = ~ parseresult(.x))

        results <- dplyr::bind_rows(first_page, results)

        return(results)

      }

    )

    # End of tryCatch ----------------------------------------------------------

  }

}
