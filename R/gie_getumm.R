#' gie_getumm -- Download info on IIP urgent market messages
#'
#' @description Function to download urgent market messages (UMM) from the Inside Information Platform (IIP)
#'
#' @param from Character. Date the UMM is being in place from (format: YYYY-MM-DD).
#' @param to Character. Date the UMM is being in place to (format: YYYY-MM-DD).
#' @param published_date Character. Filter by publication date of UMM. Can be partial (e.g.: "2025-02-25", "2025-02", "2025").
#' @param eic_entity Character. EIC code of the UMM issuing entity.
#' @param eic_participant Character. EIC code of the UMM issuing market participant.
#' @param eic_asset Character. EIC code of the asset the UMM is issued for.
#' @param status Character. Can be 'active', 'inactive' or 'dismissed'. Default is all.
#' @param direction Character. Direction of gas flows. Can be "entry" or "exit". Default is all.
#' @param timeout Numeric. If the amount of pages of your request exceeds 60, a timeout \cr
#' will be enforced to prevent the API from timing out. Defaults to 3 seconds, any \cr
#' values must be set in seconds, too.
#' @param size Integer. The number of results per page.
#' @param apikey Character. Your personal API key.
#'
#' @returns A nested list with UMMs for your specific API query to IIP. Note: The results are not parsed for there are too many data variants a user could want.
#' @export
#'
#' @examples
#' \dontrun{
#' umm <- gie_getumm(from = "2024-01-01", to = "2024-12-31")
#' }
#'
gie_getumm <- function(from = NULL,
                       to = NULL,
                       published_date = NULL,
                       eic_entity = NULL,
                       eic_participant = NULL,
                       eic_asset = NULL,
                       status = NULL,
                       direction = NULL,
                       timeout = 3,
                       size = 300,
                       apikey = Sys.getenv("GIE_APIKEY")) {

  check_gieumminput(from = from,
                    to = to,
                    published_date = published_date,
                    eic_entity = eic_entity,
                    eic_participant = eic_participant,
                    eic_asset = eic_asset,
                    status = status,
                    direction = direction,
                    timeout = timeout,
                    size = size,
                    apikey = apikey)

  raw_results <- getrequest_general(database = "iip",
                                    target = "",
                                    timeout = timeout,
                                    size = size,
                                    page = 1,
                                    # pages = NULL, # implicitly
                                    apikey = apikey,
                                    # verbose = FALSE, # implicitly
                                    # API parameters hereafter
                                    from = from,
                                    to = to,
                                    published = published_date,
                                    reportingEntity = eic_entity,
                                    marketParticipant = eic_participant,
                                    asset = eic_asset,
                                    status = status,
                                    direction = direction)

  pages <- raw_results[["last_page"]]

  #-----------------------------------------------------------------------------
  # Check if there was an empty response with 0 pages --------------------------

  if (pages == 0 | length(raw_results[["data"]]) == 0) {

    warning("No results found for your query. Invisibly returning raw return object.",
            call. = FALSE)

    invisible(return(raw_results))

    stop(call. = FALSE)

  }

  #-----------------------------------------------------------------------------

  if (pages == 1L) {

    return(raw_results[["data"]])

  }

  #-------------------------------------------------------------------------------

  if (pages > 1L) {

    first_page <- raw_results

    if (pages > 60L) {

      message(paste0("!~~~ Large request (total of ",
                     pages,
                     " pages), slowing down querying process by ",
                     timeout,
                     " seconds per API call. \n You can adjust this using the 'timeout' parameter."))

    }

    raw_results <- purrr::map(.x = c(2:pages),
                              .f = ~ getrequest_general(database = "iip",
                                                        target = "",
                                                        size = size,
                                                        timeout = timeout,
                                                        page = .x,
                                                        pages = pages, # now necessary
                                                        apikey = apikey,
                                                        # API parameters hereafter
                                                        from = from,
                                                        to = to,
                                                        published = published_date,
                                                        reportingEntity = eic_entity,
                                                        marketParticipant = eic_participant,
                                                        asset = eic_asset,
                                                        status = status,
                                                        direction = direction)) %>%
                    purrr::map("data")

    combined_results_raw <- c(list(first_page[["data"]]), raw_results)

    combined_results <- do.call(c, combined_results_raw)

    return(combined_results)

  }

}
