#' gie_batchload -- Load data in batch
#'
#' @description Function to download data from GIE's AGSI+/ALSI+ API in bulk
#'
#' @param countries Character. Specify the countries of interest as two-digit \cr
#' country codes (e.g., 'DE', 'IE'). \cr
#' Must be of length one (i.e., one country) if you want to specify multiple \cr
#' companies and/or multiple facilities.
#' @param companies A character vector of company EIC codes to get data from. \cr
#' Must be of length one (i.e., one company) if you want to specify multiple \cr
#' facilities.
#' @param facilities A character vector of facility EIC codes to get data from.
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
#' @return A data.frame with results
#' @export
#'
#' @examples
#' \dontrun{
#' gie_batchload(countries = c("DE", "AT", "FR"), date = "2022-04-01")
#' }
#'
gie_batchload <- function(countries,
                          companies = NULL,
                          facilities = NULL,
                          from = NULL,
                          to = NULL,
                          date = NULL,
                          size = 30,
                          timeout = 3,
                          database = "agsi",
                          verbose = FALSE,
                          apikey = Sys.getenv("GIE_APIKEY")) {

  if (missing(countries)) stop("You have to at least specify the 'country' parameter.",
                             call. = FALSE)

  check_giedata2input(countries = countries,
                      companies = companies,
                      facilities = facilities,
                      from = from,
                      to = to,
                      date = date,
                      size = size,
                      timeout = timeout,
                      database = database,
                      verbose = verbose,
                      apikey = apikey)

  # Download bulk data only for countries
  if (!is.null(countries) & is.null(companies) & is.null(facilities)) {

  # Loop over the countries vector
  results <- purrr::map_dfr(.x = countries,
                            .f = ~ gie_load(country = .,
                                            from = from,
                                            to = to,
                                            date = date,
                                            size = size,
                                            timeout = timeout,
                                            database = database,
                                            verbose = verbose,
                                            apikey = apikey))

  return(results)

  }

  #----------------------------------------------------------------------------#

  # Download data for countries and companies
  if (!is.null(countries) & !is.null(companies) & is.null(facilities)) {

    if (length(countries) != 1) {
      stop("If you specify various companies, you can only choose one country.")
    }

    results <- purrr::map2_dfr(.x = countries,
                               .y = companies,
                               .f = ~ gie_load(country = .x,
                                               company = .y,
                                               from = from,
                                               to = to,
                                               date = date,
                                               size = size,
                                               timeout = timeout,
                                               database = database,
                                               verbose = verbose,
                                               apikey = apikey))

   return(results)

  }

  #----------------------------------------------------------------------------#

  # Download data for countries, companies and facilities
  if (!is.null(countries) & !is.null(companies) & !is.null(facilities)) {

    if (length(countries) != 1 | length(companies) != 1) {
      stop("If you specify various facilities, you can only choose one country and one company.")
    }

    pmap_arguments <- list(country = countries,
                           company = companies,
                           facility = facilities)

    results <- purrr::pmap_dfr(pmap_arguments,
                               .f = ~ gie_load(country = ..1,
                                               company = ..2,
                                               facility = ..3,
                                               from = from,
                                               to = to,
                                               date = date,
                                               size = size,
                                               timeout = timeout,
                                               database = database,
                                               verbose = verbose,
                                               apikey = apikey))

    return(results)

  }

}
