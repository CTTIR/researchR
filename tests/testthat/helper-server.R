# Integration tests run against a real researchD instance when one is
# configured, and are skipped otherwise so `R CMD check` stays offline-safe.
#
#   RESEARCHR_TEST_URL   base URL of a disposable researchD
#   RESEARCHR_TEST_TOKEN its API_TOKEN
#
# Point both at a throwaway, seeded researchD instance to run the suite.

skip_without_server <- function() {
  testthat::skip_if_not(
    nzchar(Sys.getenv("RESEARCHR_TEST_URL")) &&
      nzchar(Sys.getenv("RESEARCHR_TEST_TOKEN")),
    "no researchD test server configured")
}

test_conn <- function(check = TRUE) {
  rr_connect(Sys.getenv("RESEARCHR_TEST_URL"),
             Sys.getenv("RESEARCHR_TEST_TOKEN"),
             check = check)
}
