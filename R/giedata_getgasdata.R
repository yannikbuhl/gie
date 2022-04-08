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
#' @export
#'
#' @examples
giedata_gastanks <- function(country,
                             from = Sys.Date() - 5,
                             to = Sys.Date() - 1,
                             page = 1,
                             date = NULL,
                             size = 20,
                             type = "agsi",
                             apikey) {

  # Execute first GET request
  raw_results <- giedata_getrequest(country = country,
                                    from = from,
                                    to = to,
                                    page = page,
                                    date = date,
                                    size = size,
                                    type = type,
                                    apikey = apikey)

  # Get number of pages to the request
  pages <- raw_results[["last_page"]]

  # If there is only one page, proceed
  if (pages == 1L) {

    message("~~~ There is only one page for your request, proceeding normally.")

    # Parse results from GET request
    results <- giedata_parseresult(raw_results)

    return(results)

  } else if (pages > 1L) {

    message(paste0("~~~ Found ", pages, " pages. Downloading data."))

    raw_results <- purrr::map(c(1:pages),
                              .f = ~ giedata_getrequest(country = country,
                                                        from = from,
                                                        to = to,
                                                        page = .x,
                                                        date = date,
                                                        size = size,
                                                        type = type,
                                                        apikey = apikey))

    message("~~~ Parsing raw request results.")

    results <- raw_results %>% purrr::map_dfr(., .f = ~ giedata_parseresult(.x))

    return(raw_results)

  }

}
