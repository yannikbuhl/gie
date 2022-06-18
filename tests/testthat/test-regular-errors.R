#------------------------------------------------------------------------------#
# Test file: Test functions independent of API calls/HTTP failures
#------------------------------------------------------------------------------#

test_that("download single data only works for one country", {

  expect_error(object = get_giedata(country = c("DE", "AT"),
                                    date = "2022-05-01"),
               regexp = "type character and length 1")

})
