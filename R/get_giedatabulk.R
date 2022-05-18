#' get_giedatabulk
#'
#' @param countries A character vector of countries in short format (e.g., "DE")
#' to get data from. Must be of length one (i.e., one country) if you want to
#' specify multiple companies and/or multiple facilities.
#' @param companies A character vector of company EIC codes to get data from. \cr
#' Must be of length one (i.e., one company) if you want to specify multiple \cr
#' facilities.
#' @param facilities A character vector of facility EIC codes to get data from.
#' @param from Character. Specify the start of the time span you are \cr
#' interested in downloading (format: YYYY-MM-DD).
#' @param to Character. Specify the end of the time span you are \cr
#' interested in downloading (format: YYYY-MM-DD).
#' @param page Integer. The page of a multi-page query you want to get.
#' @param date Character. If you want to have data only for one date. \cr
#' If you set 'date', you cannot set the 'from' and/or 'to' parameters \cr
#' (format: YYYY-MM-DD).
#' @param size Integer. The number of results per page.
#' @param type
#' @param database Character. The type of API you want to address ('agsi' or 'alsi').
#' @param verbose
#' @param apikey Character. Your personal API key.

#'
#' @return
#' @export
#'
#' @examples
get_giedatabulk <- function(countries = NULL,
                            companies = NULL,
                            facilities = NULL,
                            from = NULL,
                            to = NULL,
                            page = 1,
                            date = NULL,
                            size = 30,
                            type = NULL,
                            timeout = 3,
                            database = "agsi",
                            verbose = FALSE,
                            apikey) {

  # Download bulk data only for countries
  if (!is.null(countries) & is.null(companies) & is.null(facilities)) {

  # Loop over the countries vector
  results <- purrr::map_dfr(.x = countries,
                            .f = ~ get_giedata(country = .,
                                               from = from,
                                               to = to,
                                               page = page,
                                               date = date,
                                               size = size,
                                               type = type,
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
                               .f = ~ get_giedata(country = .x,
                                                  company = .y,
                                                  from = from,
                                                  to = to,
                                                  page = page,
                                                  date = date,
                                                  size = size,
                                                  type = type,
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
                               .f = ~ get_giedata(country = ..1,
                                                  company = ..2,
                                                  facility = ..3,
                                                  from = from,
                                                  to = to,
                                                  page = page,
                                                  date = date,
                                                  size = size,
                                                  type = type,
                                                  timeout = timeout,
                                                  database = database,
                                                  verbose = verbose,
                                                  apikey = apikey))

    return(results)

  }

}
