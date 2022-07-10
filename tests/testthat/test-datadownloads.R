#------------------------------------------------------------------------------#
# Test file: Functions to fetch data
#------------------------------------------------------------------------------#

test_that("download single country data", {

  vcr::use_cassette("test1.1", {

    result <- get_giedata(country = "DE",
                          date = "2022-05-01")

  })

  expect_s3_class(result,
                  class = "data.frame")

})

#------------------------------------------------------------------------------#

test_that("download single country data with from/to", {

  vcr::use_cassette("test1.2", {

    result <- get_giedata(country = "AT",
                          from = "2022-02-01",
                          to = "2022-02-15")

  })

  expect_s3_class(result,
                  class = "data.frame")

})



#------------------------------------------------------------------------------#

test_that("check that get_giedata2 works with multiple countries", {

  vcr::use_cassette("test1.3", {

    result <- get_giedata2(countries = c("DE", "AT"),
                           date = "2022-05-01")

  })

  expect_s3_class(object = result,
                  class = "data.frame")

})

#------------------------------------------------------------------------------#

test_that("check that get_giedata works with company & facilities", {

  vcr::use_cassette("test1.4", {

    result <- get_giedata(country = c("DE"),
                          date = "2022-05-01",
                          company = "21X000000001160J",
                          facility = "21Z000000000271O")

  })

  expect_s3_class(object = result,
                  class = "data.frame")

})

#------------------------------------------------------------------------------#

test_that("check that get_giedata works with company", {

  vcr::use_cassette("test1.5", {

    result <- get_giedata(country = c("DE"),
                          date = "2022-05-01",
                          company = "21X000000001160J")

  })

  expect_s3_class(object = result,
                  class = "data.frame")

})

#------------------------------------------------------------------------------#

test_that("check that get_giedata2 works with companies", {

  vcr::use_cassette("test1.6", {

    result <- get_giedata2(countries = "DE",
                           date = "2022-05-01",
                           companies = c("21X000000001160J",
                                         "21X0000000011756"))

  })

  expect_s3_class(object = result,
                  class = "data.frame")

})

#------------------------------------------------------------------------------#

test_that("check that get_giedata2 works with company and facilities", {

  vcr::use_cassette("test1.7", {

    result <- get_giedata2(countries = "DE",
                           date = "2022-05-01",
                           companies = "21X000000001160J",
                           facilities = c("21Z000000000271O",
                                          "21W0000000001148",
                                          "21W0000000001261"))

  })

  expect_s3_class(object = result,
                  class = "data.frame")

})

#------------------------------------------------------------------------------#
