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

test_that("download single data only works for one country", {
  vcr::use_cassette("test1.2", {

    # This remains empty

  })

  expect_error(object = get_giedata(country = c("DE", "AT"),
                                    date = "2022-05-01"),
               regexp = "type character and length 1")
})
