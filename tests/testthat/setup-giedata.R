library("vcr") # *Required* as vcr is set up on loading
invisible(vcr::vcr_configure(
  filter_sensitive_data = list("<<<GIE_APIKEY>>>" = Sys.getenv("GIE_APIKEY")),
  dir = vcr::vcr_test_path("fixtures")
))
vcr::check_cassette_names()
