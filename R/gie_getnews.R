#' gie_getnews -- Get AGSI+/ALSI+ news
#'
#' @description A function that conveniently outputs all 'News' items published by GIE with regards to AGSI+ or ALSI+ platforms
#'
#' @param database Character. The data base for which the 'News' items should be returned ('agsi' or 'alsi').
#' @param html_parsed Logical. Some of the columns in the resulting data.frame might contain HTML \cr
#' tags and other encodings. If set to 'TRUE', this parameter will result in the respective column \cr
#' values being decoded so there are no HTML residuals left. This requires the 'rvest' package to be installed and loaded.\cr
#' Defaults to 'FALSE'.
#' @param apikey Character. Your personal API key.
#'
#' @return A data.frame with all the news for the respective data base.
#' @export
#'
#' @examples
#' \dontrun{
#' news <- gie_getnews(database = "alsi", html_parsed = TRUE)
#' }
#'
gie_getnews <- function(database,
                        html_parsed = FALSE,
                        apikey = Sys.getenv("GIE_APIKEY")) {

  # Input check
  if (base::missing(database) | !(database %in% c("agsi", "alsi"))) {

    stop("You have to provide a parameter 'database' (either 'alsi' or 'agsi').",
         call. = FALSE)

  }

  if (!is.logical(html_parsed) | length(html_parsed) != 1) {

    stop("Misspecified parameter 'html_parsed'.",
         call. = FALSE)

  }

  # Create endpoint URL
  endpoint <- paste0("https://", database, ".gie.eu/api/news")

  # Construct URL
  url <- construct_url(url = endpoint,
                       query = list())

  # Get raw response
  raw_news <- send_getrequest(url = url,
                              apikey = apikey)

  # Parse content of response
  parsed_news <- raw_news %>% httr::content(as = "parsed")

  # Parse response content based on whether HTML tags should be removed or not
  if (isTRUE(html_parsed)) {

    if (!requireNamespace("rvest", quietly = TRUE)) {

      stop("If you set the 'html_parsed' parameter to TRUE, the package {rvest} needs to be installed.",
           call. = FALSE)

    }

    # Remove HTML tags and encodings
    news <- purrr::map_dfr(parsed_news, bind_rows) %>%
              mutate(summary = ifelse(summary == '', NA, summary),
                     details = ifelse(details == '', NA, details),
                     summary = strip_html(summary),
                     details = strip_html(details))

  } else {

    # If html_parsed = FALSE, return content with not HTML tags removed
    news <- purrr::map_dfr(parsed_news, bind_rows)

  }

  return(news)

}


