conn <- structure(list(base_url = "https://feeds.example.org"),
                  class = "researchr_conn")

test_that("feed URLs are built for every stream", {
  expect_identical(rr_feed_url(conn, "glp1_mash"),
                   "https://feeds.example.org/feed/glp1_mash.xml")
  expect_identical(rr_feed_url(conn, "glp1_mash", study_type = "rct"),
                   "https://feeds.example.org/feed/glp1_mash/rct.xml")
  expect_identical(rr_feed_url(conn, "glp1_mash", all = TRUE),
                   "https://feeds.example.org/feed/glp1_mash/all.xml")
  expect_identical(rr_feed_url(conn, "glp1_mash", preprints = TRUE),
                   "https://feeds.example.org/feed/glp1_mash/preprints.xml")
  expect_identical(rr_feed_url(conn, "glp1_mash", signals = TRUE),
                   "https://feeds.example.org/signals/glp1_mash.xml")
})

test_that("mutually exclusive feed variants are refused", {
  expect_error(rr_feed_url(conn, "q", study_type = "rct", all = TRUE),
               "at most one")
})

test_that("feed URL variants are valid and encode path values", {
  expect_error(rr_feed_url(conn, "q", signals = TRUE, preprints = TRUE), "only available")
  expect_error(rr_feed_url(conn, "q", all = NA), "all")
  expect_error(rr_feed_url(conn, "../q"), "slash")
  expect_identical(rr_feed_url(conn, "old question?"),
                   "https://feeds.example.org/feed/old%20question%3F.xml")
  expect_identical(rr_feed_url(conn, "literal%2Fname"),
                   "https://feeds.example.org/feed/literal%252Fname.xml")
  expect_identical(rr_feed_url(conn, "q", signals = TRUE, study_type = "trial_result"),
                   "https://feeds.example.org/signals/q/trial_result.xml")
})
