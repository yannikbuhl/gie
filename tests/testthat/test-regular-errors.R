#------------------------------------------------------------------------------#
# Test file: Test functions independent of API calls/HTTP failures
#------------------------------------------------------------------------------#

test_that("download single data only works for one country", {

  expect_error(object = gie_load(country = c("DE", "AT"),
                                 date = "2022-05-01"),
               regexp = "type character and length 1")

})

#------------------------------------------------------------------------------#

test_that("company must be specified if facility is specified", {

  expect_error(object = gie_load(country = "DE",
                                 date = "2022-05-01",
                                 facility = "mock_facility"),
                 regexp = "specified, too")

})

#------------------------------------------------------------------------------#

test_that("warning pops up if date, from and to are set (error 1)", {

  expect_warning(object = gie_load(country = "DE",
                                   date = "2022-05-01",
                                   from = "2022-05-01",
                                   to = "2022-05-15"),
                 regexp = "override")

  skip_on_cran()

})

test_that("warning pops up if date, from and to are set (error 2)", {

  expect_warning(object = gie_batchload(countries = c("DE", "AT"),
                                        date = "2022-05-01",
                                        from = "2022-05-01",
                                        to = "2022-05-15"),
                 regexp = "override")

  skip_on_cran()

})

#------------------------------------------------------------------------------#

test_that("companies must be specified if facility is specified", {

  expect_error(object = gie_batchload(countries = "DE",
                                      date = "2022-05-01",
                                      facilities = c("mock_facilities",
                                                     "mock2_facilities")),
               regexp = "specified, too")

})

#------------------------------------------------------------------------------#

test_that("companies must be of length one if facility is specified", {

  expect_error(object = gie_batchload(countries = "DE",
                                      date = "2022-05-01",
                                      companies = c("mock_facility",
                                                    "mock2_facility"),
                                      facilities = c("mock_facilities",
                                                     "mock2_facilities")),
               regexp = "must only contain one company EIC")

})

#------------------------------------------------------------------------------#

test_that("gie_getnews returns errors upon misspecified parameters",{

 expect_error(object = gie_getnews(database = "alsi",
                                   html_parsed = 1),
              regexp = "Misspecified")

 expect_error(object = gie_getnews(database = "foobar"),
              regexp = "parameter 'database'")

})

#------------------------------------------------------------------------------#

test_that("gie_unav returns errors upon misspecified parameters", {

  expect_error(object = gie_unav(country = c("DE", "AT")),
               regexp = "and length 1.")

  expect_error(object = gie_unav(database = "foobar"),
               regexp = "between 'agsi' or 'alsi'")

  expect_error(object = gie_unav(start = 2024),
               regexp = "be type character")

  expect_error(object = gie_unav(end = 2024),
               regexp = "be type character")

  expect_error(object = gie_unav(type = "alsi"),
               regexp = "either 'planned' or 'unplanned'")

  expect_error(object = gie_unav(end_flag = "alsi"),
               regexp = "either 'confirmed' or 'estimate'")

  expect_error(object = gie_unav(size = 301),
               regexp = "max. 300.")

})

#------------------------------------------------------------------------------#
