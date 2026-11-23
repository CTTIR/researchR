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
  local_mocked_bindings(
    rr_get = function(...) cli::cli_abort("unexpected request"),
    .package = "researchR"
  )
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
  local_mocked_bindings(
    rr_get = function(...) cli::cli_abort("unexpected request"),
    .package = "researchR"
  )
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

test_that("question and signal definitions are checked before any request", {
  local_mocked_bindings(
    rr_get = function(...) cli::cli_abort("unexpected request"),
    rr_post = function(...) cli::cli_abort("unexpected request"),
    .package = "researchR"
  )
  conn <- offline_conn()
  save <- function(...) {
    rr_upsert_question(conn, "q", "Q", rr_concept("drug", "x"), ...)
  }
  expect_error(save(email_to = NA_character_), "email_to")
  expect_error(save(study_filter = c("rct", "")), "study_filter")
  expect_error(save(delivery = 1), "delivery")
  expect_error(save(email_enabled = TRUE), "needs a recipient")
  expect_error(save(sources = list(TRUE)), "unique, non-empty names")
  expect_error(save(sources = list(pubmed = TRUE, pubmed = FALSE)), "unique")
  expect_error(save(sources = list(pubmed = "yes")), "TRUE or FALSE")
  err <- tryCatch(save(api_keys = list(pubmed = 12345)), error = identity)
  expect_match(conditionMessage(err), "pubmed")
  expect_false(grepl("12345", conditionMessage(err), fixed = TRUE))
  expect_error(
    rr_set_signal_config(conn, "q", sources = list(sec_edgar = NA)),
    "TRUE or FALSE"
  )
  expect_error(
    rr_set_signal_config(conn, "q", feed_categories = c(TRUE, FALSE)),
    "feed_categories"
  )
  single_target <- list(type = "rss", url = "https://example.org/feed")
  expect_error(
    rr_set_signal_config(conn, "q", scrape_targets = single_target),
    "list of target definitions"
  )
})

test_that("valid optional definition forms are still accepted", {
  sent <- NULL
  local_mocked_bindings(
    rr_post = function(conn, path, body) {
      sent <<- body
      list(ok = TRUE)
    },
    rr_question = function(conn, question_id) tibble::tibble(id = question_id),
    .package = "researchR"
  )
  suppressMessages(rr_upsert_question(
    offline_conn(), "q", "Q", rr_concept("drug", "x"),
    sources = c(pubmed = TRUE, biorxiv = FALSE), study_filter = NULL,
    email_enabled = TRUE, email_to = c(first = "a@example.org")
  ))
  expect_identical(sent$sources, list(pubmed = TRUE, biorxiv = FALSE))
  expect_identical(sent$study_filter, list())
  expect_identical(sent$email_to, list("a@example.org"))
})

test_that("functions report a non-connection before inspecting it", {
  calls <- list(
    function() rr_records("not a connection", "q", study_type = "rct"),
    function() rr_signals(list(), "q"),
    function() rr_meta(NULL),
    function() rr_upsert_question("x", "q", "Q", rr_concept("a", "b")),
    function() rr_set_signal_config("x", "q"),
    function() rr_delete_question("x", "q")
  )
  for (call in calls) {
    expect_error(call(), "must be a connection")
  }
})

empty_object <- structure(list(), names = character(0))

test_that("saving a question sends the server's documented body shape", {
  sent <- NULL
  stored <- list(list(
    id = "glp1", label = "GLP-1", enabled = TRUE,
    concepts = list(list(name = "drug", terms = list("semaglutide")))
  ))
  local_mocked_bindings(
    rr_post = function(conn, path, body) {
      sent <<- list(path = path, body = body)
      list(ok = TRUE)
    },
    rr_get = function(conn, path, ...) stored,
    .package = "researchR"
  )
  concept <- rr_concept("drug", c(a = "semaglutide"), mesh = "Liraglutide")
  out <- suppressMessages(rr_upsert_question(
    offline_conn(), "glp1", "GLP-1", list(drug = concept),
    digest_mode = "threshold", min_score = 2L, from_days_back = 14
  ))
  expect_identical(sent$path, "questions")
  body <- sent$body
  expect_identical(body$concepts, list(list(
    name = "drug", terms = list("semaglutide"), mesh = list("Liraglutide")
  )))
  expect_identical(body$sources, empty_object)
  expect_identical(body$api_keys, empty_object)
  expect_identical(body$delivery, list("file"))
  expect_identical(body$from_days_back, 14L)
  expect_identical(body$min_score, 2)
  expect_identical(body$digest_mode, "threshold")
  expect_identical(as.character(jsonlite::toJSON(body$sources)), "{}")
  expect_identical(out$id, "glp1")
})

test_that("a missing question is a not-found condition listing known ids", {
  stored <- list(list(id = "a", label = "A"), list(id = "b", label = "B"))
  local_mocked_bindings(rr_get = function(...) stored, .package = "researchR")
  expect_identical(rr_question(offline_conn(), "b")$label, "B")
  expect_error(rr_question(offline_conn(), "c"), "Known ids",
               class = "researchR_not_found")
})

test_that("signal configuration updates keep every field that is not given", {
  feed <- list(name = "", type = "rss", url = "https://example.org/feed.xml")
  current <- list(
    sources = list(sec_edgar = TRUE), min_confidence = "authoritative",
    instant_alerts = TRUE, feed_categories = list(news = FALSE),
    scrape_targets = list(feed), api_key_present = list(epo_ops = TRUE)
  )
  sent <- NULL
  local_mocked_bindings(
    rr_get = function(conn, path, ...) current,
    rr_post = function(conn, path, body) {
      sent <<- list(path = path, body = body)
      list(ok = TRUE)
    },
    .package = "researchR"
  )
  update <- function(...) {
    suppressMessages(rr_set_signal_config(offline_conn(), ...))
  }
  update("q one", min_confidence = "reported")
  expect_identical(sent$path, "signal_config/q%20one")
  expect_identical(sent$body$min_confidence, "reported")
  expect_identical(sent$body$sources, current$sources)
  expect_true(sent$body$instant_alerts)
  expect_identical(sent$body$feed_categories, current$feed_categories)
  expect_identical(sent$body$scrape_targets, current$scrape_targets)
  expect_identical(sent$body$api_keys, empty_object)
  expect_null(sent$body$api_key_present)

  update("q", instant_alerts = FALSE,
         sources = c(sec_edgar = FALSE, openfda = TRUE))
  expect_false(sent$body$instant_alerts)
  expect_identical(sent$body$min_confidence, "authoritative")
  expect_identical(sent$body$sources, list(sec_edgar = FALSE, openfda = TRUE))

  current <- list(sources = empty_object)
  update("q")
  expect_identical(sent$body$min_confidence, "unverified")
  expect_false(sent$body$instant_alerts)
  expect_identical(sent$body$scrape_targets, list())
})

test_that("study types are checked against the server vocabulary", {
  queried <- NULL
  meta <- list(api_version = 1L, study_types = list("rct", "other"),
               signal_types = list("grant"))
  local_mocked_bindings(rr_get = function(conn, path, query = list()) {
    if (path == "meta") return(meta)
    queried <<- query
    list(total = 0L, records = list())
  }, .package = "researchR")
  conn <- offline_conn()
  expect_error(rr_records(conn, "q", study_type = "cohort"),
               "Unknown study type")
  out <- rr_records(conn, "q", study_type = "rct",
                    since = as.Date("2026-01-01"))
  expect_identical(nrow(out), 0L)
  expect_identical(queried$study_type, "rct")
  expect_identical(queried$since, "2026-01-01")
  conn$meta <- list(study_types = "other")
  expect_error(rr_records(conn, "q", study_type = "rct"), "Unknown study type")
})
