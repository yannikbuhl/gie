#------------------------------------------------------------------------------#
# Test file: Functions to fetch data
#------------------------------------------------------------------------------#

test_that("download single country data", {
  vcr::use_cassette("test1.1", {

    result <- get_giedata(country = "DE",
                          date = "2022-05-01")

  })
  expect_s3_class(result, class = "data.frame")
})

#------------------------------------------------------------------------------#

test_that("check that get_giedata2 works with multiple countries", {
  vcr::use_cassette("test1.2", {

  result <- get_giedata2(countries = c("DE", "AT"),
                 date = "2022-05-01")

  })

  expect_s3_class(object = result,
                  class = "data.frame")

})
