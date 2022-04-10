#' Function to download data from GIE's AGSI+ API
#'
#' @param country Character string. Specify the country of interest.
#' @param from Character string. Specify the start of the time span you are \cr
#' interested in downloading (format: YYYY-MM-DD).
#' @param to
#' @param page
#' @param date
#' @param size
#' @param type
#' @param apikey
#'
#' @return A data frame.
#' @import magrittr httr
#' @export
#'
#' @examples
get_giedata <- function(country,
                        from = Sys.Date() - 5,
                        to = Sys.Date() - 1,
                        page = 1,
                        date = NULL,
                        size = 20,
                        type = "agsi",
                        verbose = FALSE,
                        apikey) {

  # Execute first GET request
  raw_results <- getrequest(country = country,
                            from = from,
                            to = to,
                            page = page,
                            date = date,
                            size = size,
                            type = type,
                            verbose = verbose,
                            apikey = apikey)

  # Get number of pages to the request
  pages <- raw_results[["last_page"]]

  # If there is only one page, proceed
  if (pages == 1L) {

    if (isTRUE(verbose)) {

      message("~~~ There is only one page for your request, proceeding normally.")

    }

    # Parse results from GET request
    results <- parseresult(raw_results)

    return(results)

  } else if (pages > 1L) {

    if (isTRUE(verbose)) {

    message(paste0("~~~ Found ", pages, " pages. Downloading data."))

    }

    first_page <- parseresult(raw_results)

    raw_results <- purrr::map(c(2:pages),
                              .f = ~ getrequest(country = country,
                                                from = from,
                                                to = to,
                                                page = .x,
                                                date = date,
                                                size = size,
                                                type = type,
                                                pages = pages,
                                                verbose = verbose,
                                                apikey = apikey))

    if (isTRUE(verbose)) {

    message("~~~ Parsing raw request results.")

    }

    results <- raw_results %>% purrr::map_dfr(., .f = ~ parseresult(.x))

    results <- dplyr::bind_rows(first_page, results)

    return(results)

  }

}
