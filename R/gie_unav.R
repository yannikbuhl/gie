#' gie_unav -- Download info on unavailabilities
#'
#' @description Function to download data on AGSI+ and ALSI+ unavailability
#'
#' @param country Character. Specify the country of interest as two-digit country code (e.g., 'DE', 'IE').
#' @param start Character. Specify the start of the time span you are \cr
#' interested in downloading (format: YYYY-MM-DD).
#' @param end Character. Specify the end of the time span you are \cr
#' interested in downloading (format: YYYY-MM-DD).
#' @param type Character. The type of unavailability info you want (choose between 'planned' and 'unplanned').
#' @param end_flag Character. The end flag for the unavailability info (choose between 'confirmed' and 'estimate').
#' @param size Integer. The number of results per page.
#' @param timeout Numeric. If the amount of pages of your request exceeds 60, a timeout \cr
#' will be enforced to prevent the API from timing out. Defaults to 3 seconds, any \cr
#' values must be set in seconds, too.
#' @param database Character. The type of API you want to address ('agsi' or 'alsi').
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
#' gie_unav(country = "DE", date = "2022-01-03", database = "alsi")
#' }
gie_unav <- function(country = NULL,
                     start = NULL,
                     end = NULL,
                     type = NULL,
                     end_flag = NULL,
                     timeout = 3,
                     size = 30,
                     database = "agsi",
                     apikey = Sys.getenv("GIE_APIKEY")) {


  # First step of error handling -----------------------------------------------

  # Error handling
  check_gieunavinput(country = country,
                     start = start,
                     end = end,
                     type = type,
                     end_flag = end_flag,
                     timeout = timeout,
                     size = size,
                     database = database,
                     apikey = apikey)

  # Get raw results
  raw_results <- getrequest_general(database = database,
                                    target = "unavailability",
                                    size = size,
                                    timeout = timeout,
                                    page = 1,
                                    # pages = NULL, # implicitly
                                    apikey = apikey,
                                    # verbose = FALSE, # implicitly
                                    # API parameters hereafter
                                    country = country,
                                    start = start,
                                    end = end,
                                    type = type,
                                    end_flag = end_flag)

  pages <- raw_results[["last_page"]]

  #-----------------------------------------------------------------------------
  # Check if there was an empty response with 0 pages --------------------------

  if (pages == 0 | length(raw_results[["data"]]) == 0) {

    warning("No results found for your query (possibly). Invisibly returning raw return object.",
            call. = FALSE)

    invisible(return(raw_results))

    stop(call. = FALSE)

  }

  #-----------------------------------------------------------------------------

  # Parse results depending on database queried
  if (pages == 1L) {

    # tryCatch if parsing goes wrong -------------------------------------------

    tryCatch(

      error = function(cnd) {

        # In case of error, warn and return raw results
        warning("!~~~ Parsing in gie_unav() failed, Error 1. Returning raw response.", call. = FALSE)

        return(raw_results)

      },
      {

        # Parse results from GET request
        results <- parse_unav(raw_results)

        # Return parsed results
        return(results)

      })

    # End of tryCatch ----------------------------------------------------------

  } else if (pages > 1L) {

      first_page <- parse_unav(raw_results)

      if (pages > 60L) {

        message(paste0("!~~~ Large request (total of ", pages, " pages), slowing down querying process by ", timeout, " seconds per API call. \n You can adjust this using the 'timeout' parameter."))

      }

      raw_results <- purrr::map(.x = c(2:pages),
                                .f = ~ getrequest_general(database = database,
                                                          target = "unavailability",
                                                          size = size,
                                                          timeout = timeout,
                                                          page = .x,
                                                          pages = pages, # now necessary
                                                          apikey = apikey,
                                                          # API parameters hereafter
                                                          country = country,
                                                          start = start,
                                                          end = end,
                                                          type = type,
                                                          end_flag = end_flag))

      # tryCatch if parsing goes wrong -----------------------------------------

      tryCatch(

        error = function(cnd) {

          warning("!~~~ Parsing failed in gie_unav(), Error 2. Returning raw response.", call. = FALSE)

          return(raw_results)

        },{

          results <- raw_results %>% purrr::map_dfr(., .f = ~ parse_unav(.x))

          results <- dplyr::bind_rows(first_page, results)

          return(results)

        }

      )

      # End of tryCatch --------------------------------------------------------

  }


}
