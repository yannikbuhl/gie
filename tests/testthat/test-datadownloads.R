#------------------------------------------------------------------------------#
# Test file: Functions to fetch data
#------------------------------------------------------------------------------#

test_that("download single country data", {

  vcr::use_cassette("test1.1",
                    record = "new_episodes",{

    result <- gie_load(country = "DE",
                       date = "2022-05-01")

  })

  expect_s3_class(result,
                  class = "data.frame")

  skip_on_cran()

})

#------------------------------------------------------------------------------#

test_that("download single country data with from/to", {

  vcr::use_cassette("test1.2",
                    record = "new_episodes", {

    result <- gie_load(country = "AT",
                       from = "2022-02-01",
                       to = "2022-02-15")

  })

  expect_s3_class(result,
                  class = "data.frame")

  skip_on_cran()

  })

#------------------------------------------------------------------------------#

test_that("check that gie_batchload works with multiple countries", {

  vcr::use_cassette("test1.3",
                    record = "new_episodes", {

    result <- gie_batchload(countries = c("DE", "AT"),
                            date = "2022-05-01")

  })

  expect_s3_class(object = result,
                  class = "data.frame")

  skip_on_cran()

})

#------------------------------------------------------------------------------#

test_that("check that gie_load works with company & facilities", {

  vcr::use_cassette("test1.4",
                    record = "new_episodes", {

    result <- gie_load(country = c("DE"),
                       date = "2022-05-01",
                       company = "21X000000001160J",
                       facility = "21Z000000000271O")

  })

  expect_s3_class(object = result,
                  class = "data.frame")

  skip_on_cran()

})

#------------------------------------------------------------------------------#

test_that("check that gie_load works with company", {

  vcr::use_cassette("test1.5",
                    record = "new_episodes", {

    result <- gie_load(country = c("DE"),
                       date = "2022-05-01",
                       company = "21X000000001160J")

  })

  expect_s3_class(object = result,
                  class = "data.frame")

  skip_on_cran()

})

#------------------------------------------------------------------------------#

test_that("check that gie_batchload works with companies", {

  vcr::use_cassette("test1.6",
                    record = "new_episodes", {

    result <- gie_batchload(countries = "DE",
                            date = "2022-05-01",
                            companies = c("21X000000001160J",
                                          "21X0000000011756"))

  })

  expect_s3_class(object = result,
                  class = "data.frame")

  skip_on_cran()

})

#------------------------------------------------------------------------------#

test_that("check that gie_batchload works with company and facilities", {

  vcr::use_cassette("test1.7",
                    record = "new_episodes", {

    result <- gie_batchload(countries = "DE",
                            date = "2022-05-01",
                            companies = "21X000000001160J",
                            facilities = c("21Z000000000271O",
                                           "21W0000000001148",
                                           "21W0000000001261"))

  })

  expect_s3_class(object = result,
                  class = "data.frame")

  skip_on_cran()

})

#------------------------------------------------------------------------------#

test_that("check that gie_listing outputs a list without specifying any parameters", {

  vcr::use_cassette("test1.8", {

    result <- gie_listing()

  })

  expect_type(object = result,
              type = "list")

  skip_on_cran()

})

#------------------------------------------------------------------------------#

test_that("check that gie_listing outputs a data.frame specifying parameters", {

  vcr::use_cassette("test1.9", {

    result <- gie_listing(region = "Europe")

  })

  expect_s3_class(object = result,
                  class = "data.frame")

  skip_on_cran()

})

#------------------------------------------------------------------------------#

test_that("check that gie_listing outputs facilities if TRUE", {

  vcr::use_cassette("test1.10", {

    result <- gie_listing(region = "Europe",
                          country = "Germany",
                          facilities = TRUE)

  })

  expect_match(object = names(result),
               regexp = "facility_eic",
               all = FALSE)

  skip_on_cran()

})

#-------------------------------------------------------------------------------

test_that("check that gie_unav outputs are correct", {

  vcr::use_cassette("test1.11", {

    result <- gie_unav(country = "Germany",
                       start = "2024-01-01",
                       end = "2024-01-01")

  })

  expect_s3_class(object = result,
                  class = "data.frame")

  skip_on_cran()

})

#-------------------------------------------------------------------------------

test_that("check that gie_getnews outputs are correct", {

  vcr::use_cassette("test1.12", {

    result <- gie_getnews(database = "alsi",
                          html_parsed = FALSE)

  })

  expect_s3_class(object = result,
                  class = "data.frame")

  skip_on_cran()

})

#-------------------------------------------------------------------------------
