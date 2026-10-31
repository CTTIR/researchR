offline_conn <- function() rr_connect("https://feeds.example.org", "synthetic-token", check = FALSE)

test_that("empty endpoints preserve the columns and types needed by user code", {
  local_mocked_bindings(rr_get = function(conn, path, ...) {
    switch(path, questions = list(), source_health = list(sources = list()),
           heartbeat = list(services = list()),
           records = list(total = 0L, records = list()),
           signals = list(total = 0L, signals = list()))
  }, .package = "researchR")
  conn <- offline_conn()
  q <- rr_questions(conn)
  expect_named(q, c("id", "label", "enabled", "from_days_back", "digest_mode",
                   "min_score", "email_enabled", "email_cron", "email_last_sent",
                   "email_to", "study_filter", "delivery", "sources", "api_key_present", "concepts"))
  expect_type(q$enabled, "logical")
  expect_type(q$from_days_back, "integer")
  expect_type(q$min_score, "double")
  expect_s3_class(q$email_last_sent, "POSIXct")
  expect_type(q$concepts, "list")
  health <- rr_source_health(conn)
  expect_type(health$stale, "logical")
  expect_type(health$error_runs, "integer")
  expect_s3_class(health$last_ok, "POSIXct")
  expect_type(rr_heartbeat(conn)$stale, "logical")
  expect_s3_class(rr_records(conn, "q")$pub_date, "Date")
  expect_type(rr_records(conn, "q")$provenance, "list")
  expect_type(rr_signals(conn, "q")$extra, "list")
})

test_that("record provenance remains compatible with servers that omit it", {
  rows <- list(list(dedup_key = "a", provenance = '[{"source":"pubmed","ext_id":"123"}]'),
               list(dedup_key = "b"))
  local_mocked_bindings(rr_get = function(...) list(total = 2L, records = rows),
                       .package = "researchR")
  recs <- rr_records(offline_conn(), "q")
  expect_identical(recs$provenance[[1]][[1]]$source, "pubmed")
  expect_identical(recs$provenance[[2]], list())
})

test_that("malformed structured metadata is not silently discarded", {
  rows <- list(list(dedup_key = "a", provenance = '{"broken"'))
  local_mocked_bindings(rr_get = function(...) list(total = 1L, records = rows),
                       .package = "researchR")
  expect_error(rr_records(offline_conn(), "q"), "provenance.*row 1",
               class = "researchR_protocol_error")
  rows[[1]]$provenance <- '"not structured"'
  expect_error(rr_records(offline_conn(), "q"), class = "researchR_protocol_error")
  rows[[1]]$provenance <- "null"
  expect_identical(rr_records(offline_conn(), "q")$provenance[[1]], list())
})

test_that("missing health rows do not masquerade as a successful empty response", {
  local_mocked_bindings(rr_get = function(...) list(status = "ok"), .package = "researchR")
  expect_error(rr_source_health(offline_conn()), class = "researchR_protocol_error")
  expect_error(rr_heartbeat(offline_conn()), class = "researchR_protocol_error")
  expect_error(rr_questions(offline_conn()), class = "researchR_protocol_error")
})

test_that("server timestamps preserve clock times, fractions and UTC offsets", {
  local_mocked_bindings(rr_get = function(...) list(list(
    id = "q", label = "Q", concepts = list(), enabled = TRUE,
    email_last_sent = "2026-10-30T13:14:15.125+01:00")), .package = "researchR")
  actual <- rr_questions(offline_conn())$email_last_sent
  expected <- as.POSIXct("2026-10-30 12:14:15", tz = "UTC") + 0.125
  expect_equal(as.numeric(actual), as.numeric(expected))
  expect_identical(attr(actual, "tzone"), "UTC")
  parsed <- researchR:::rr_parse_time(c("2026-10-30T13:14:15Z",
    "2026-10-30T07:44:15-0530", "2026-10-30 13:14:15", NA, "invalid", "2026-10-30"))
  expect_equal(as.numeric(parsed[1:3]), rep(as.numeric(expected) + 3600 - 0.125, 3))
  expect_true(all(is.na(parsed[4:6])))
})

test_that("pagination rejects invalid limits before contacting the server", {
  local_mocked_bindings(rr_get = function(...) stop("unexpected request"), .package = "researchR")
  conn <- offline_conn()
  for (value in list(-1, NA_real_, NaN, 1.5, c(1, 2), "10")) {
    expect_error(rr_records(conn, "q", n_max = value), "n_max")
  }
  for (value in list(0, -1, NA_real_, Inf, 1.5)) {
    expect_error(rr_signals(conn, "q", page_size = value), "page_size")
  }
  expect_equal(nrow(rr_records(conn, "q", n_max = 0)), 0L)
  conn$meta <- list(max_page = 0)
  expect_error(rr_records(conn, "q"), class = "researchR_protocol_error")
})

test_that("pagination respects finite limits and detects inconsistent responses", {
  pages <- list(
    list(total = 3L, records = list(list(dedup_key = "a"), list(dedup_key = "b"))),
    list(total = 3L, records = list(list(dedup_key = "c"))))
  i <- 0L; limits <- numeric()
  local_mocked_bindings(rr_get = function(conn, path, query) {
    i <<- i + 1L; limits <<- c(limits, query$limit); pages[[i]]
  }, .package = "researchR")
  expect_identical(rr_records(offline_conn(), "q", page_size = 2, n_max = 3)$dedup_key,
                   c("a", "b", "c"))
  expect_equal(limits, c(2, 1))
  pages[[2]] <- list(total = 4L, records = list(list(dedup_key = "c")))
  i <- 0L
  expect_error(rr_records(offline_conn(), "q", page_size = 2),
               class = "researchR_pagination_changed")
  pages[[2]] <- list(total = 3L, records = list(list(dedup_key = "a")))
  i <- 0L
  expect_error(rr_records(offline_conn(), "q", page_size = 2),
               class = "researchR_pagination_changed")
  pages[[2]] <- list(total = 3L, records = list())
  i <- 0L
  expect_error(rr_records(offline_conn(), "q", page_size = 2),
               class = "researchR_protocol_error")
})

test_that("query filters reject ambiguous values locally", {
  local_mocked_bindings(rr_get = function(...) stop("unexpected request"), .package = "researchR")
  conn <- offline_conn()
  expect_error(rr_records(conn, "q", is_preprint = NA), "is_preprint")
  expect_error(rr_records(conn, "q", since = "2026-02-30"), "valid date")
  expect_error(rr_records(conn, "q", since = "2026-10-30junk"), "valid date")
  expect_error(rr_records(conn, "q", since = as.Date(Inf, origin = "1970-01-01")), "finite")
  expect_error(rr_signals(conn, "q", since = as.POSIXct(-Inf, origin = "1970-01-01")), "finite")
  expect_error(rr_records(conn, "q", since = "2026-10-30", until = "2026-10-29"), "later")
  expect_error(rr_signals(conn, "q", min_confidence = "certain"), "min_confidence")
  expect_error(rr_set_signal_config(conn, "q", instant_alerts = "yes"), "instant_alerts")
  expect_error(rr_upsert_question(conn, "bad/id", "Bad", list()), "slug")
  expect_error(rr_upsert_question(conn, "q", "Q", rr_concept("empty")), "at least one")
})

test_that("path-based APIs encode legacy IDs without introducing route segments", {
  received <- character()
  local_mocked_bindings(rr_get = function(conn, path, ...) {
    received <<- path; list()
  }, rr_delete_path = function(conn, path) { received <<- path; list() },
  .package = "researchR")
  rr_signal_config(offline_conn(), "old question?")
  expect_identical(received, "signal_config/old%20question%3F")
  suppressMessages(rr_delete_question(offline_conn(), "old question?"))
  expect_identical(received, "questions/old%20question%3F")
  expect_error(rr_signal_config(offline_conn(), "../q"), "slash")
})
