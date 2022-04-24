get_giedatabulk <- function(countries = NULL,
                            companies = NULL,
                            facilities = NULL,
                            from = NULL,
                            to = NULL,
                            page = 1,
                            date = NULL,
                            size = 30,
                            type = NULL,
                            database = "agsi",
                            verbose = FALSE,
                            apikey) {

  # Download bulk data only for countries
  if (!is.null(countries) & is.null(companies) & is.null(facilities)) {

  results <- purrr:map_dfr(.x = countries,
                           .f = ~ get_giedata(country = .,
                                              from = from,
                                              to = to,
                                              page = page,
                                              date = date,
                                              size = size,
                                              type = type,
                                              database = database,
                                              verbose = verbose,
                                              apikey = apikey))

  return(results)

  }

  #----------------------------------------------------------------------------#

  # Download data for countries and companies
  if (!is.null(countries) & !is.null(companies) & is.null(facilities)) {


  }

  #----------------------------------------------------------------------------#

  # Download data for countries, companies and facilities
  if (!is.null(countries) & !is.null(companies) & !is.null(facilities)) {


  }

}
