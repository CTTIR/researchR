test_that("rr_connect refuses to build a useless connection", {
  expect_error(rr_connect("", "tok"), "No researchd URL")
  expect_error(rr_connect("https://x.invalid", ""), "No API token")
})

test_that("rr_connect normalises the base URL and never prints the token", {
  conn <- rr_connect("https://feeds.example.org///", "s3cret-token",
                     check = FALSE)
  expect_s3_class(conn, "researchr_conn")
  expect_identical(conn$base_url, "https://feeds.example.org")
  # cli writes straight to its own connection, so capture.output() misses it
  out <- paste(cli::cli_fmt(print(conn)), collapse = " ")
  expect_false(grepl("s3cret-token", out, fixed = TRUE))
  expect_match(out, "not shown")
})

test_that("functions reject anything that is not a connection", {
  expect_error(rr_feed_url(list(base_url = "x"), "q"), "must be a connection")
})
