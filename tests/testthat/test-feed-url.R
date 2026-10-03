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
