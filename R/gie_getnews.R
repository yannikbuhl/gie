#' gie_getnews
#'
#' @description A function that conveniently outputs all 'News' items published by GIE with regards to AGSI or ALSI platforms
#'
#' @param database Character. The data base for which the 'News' items should be returned ('agsi' or 'alsi')
#' @param apikey Character. Your personal API key.
#' @param html_parsed Logical. Some of the columns in the resulting data.frame might contain HTML \cr
#' tags and other encodings. If set to 'TRUE', this parameter will result in the respective column \cr
#' values being decoded so there are no HTML residuals left. This requires the 'rvest' package to be installed and loaded.\cr
#' Defaults to 'FALSE'.
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
                        apikey = Sys.getenv("GIE_APIKEY"),
                        html_parsed = FALSE) {

  endpoint <- paste0("https://", database, ".gie.eu/api/news")

  url <- construct_url(url = endpoint,
                       query = list())

  raw_news <- send_getrequest(url = url,
                              apikey = apikey)

  parsed_news <- raw_news %>% httr::content(as = "parsed")

  if (isTRUE(html_parsed)) {

    if (isFALSE(require("rvest"))) {

      stop("If you set the 'html_parsed' parameter to TRUE, the package {rvest} needs to be installed.",
           call. = FALSE)

    }

    news <- purrr::map_dfr(parsed_news, bind_rows) %>%
              mutate(summary = ifelse(summary == '', NA, summary),
                     details = ifelse(details == '', NA, details),
                     summary = strip_html(summary),
                     details = strip_html(details))

  } else {

    news <- purrr::map_dfr(parsed_news, bind_rows)

  }

  return(news)

}


