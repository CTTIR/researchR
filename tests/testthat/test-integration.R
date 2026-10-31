# End-to-end against a live researchD. Skipped unless a test server is
# configured; see helper-server.R.

test_that("we can connect and read the server's vocabulary", {
  skip_without_server()
  conn <- test_conn()
  expect_s3_class(conn, "researchr_conn")
  expect_true(rr_health(conn))
  meta <- rr_meta(conn)
  expect_identical(meta$study_types[[1]], "systematic_review")
  expect_identical(meta$study_types[[length(meta$study_types)]], "unclassified")
  expect_true("sec_filing" %in% meta$signal_types)
})

test_that("a bad token is reported as unauthorised, not as a parse failure", {
  skip_without_server()
  expect_error(
    rr_connect(Sys.getenv("RESEARCHR_TEST_URL"), "definitely-not-the-token"),
    class = "researchR_unauthorized")
})

test_that("an unreachable server is reported clearly", {
  expect_error(
    rr_connect("http://127.0.0.1:1", "tok", timeout = 2),
    class = "researchR_unreachable")
})

test_that("questions round-trip through the client", {
  skip_without_server()
  conn <- test_conn()
  id <- "client_it"
  on.exit(try(rr_delete_question(conn, id), silent = TRUE), add = TRUE)

  rr_upsert_question(
    conn, id, "Client integration question",
    concepts = list(drug = rr_concept("drug", c("semaglutide", "GLP-1"),
                               mesh = "Glucagon-Like Peptide 1"),
                    disease = rr_concept("disease", c("MASH", "steatohepatitis"))),
    sources = list(pubmed = TRUE, biorxiv = FALSE),
    api_keys = list(pubmed = "PLAINTEXT-SHOULD-NEVER-COME-BACK"),
    study_filter = c(design = "randomized controlled trial"),
    from_days_back = 14, digest_mode = "threshold", min_score = 1.25,
    delivery = c(local = "file", reference = "zotero"), email_enabled = TRUE,
    email_cron = "0 7 * * 1", email_to = c(first = "a@example.org", second = "b@example.org"))

  q <- rr_question(conn, id)
  expect_identical(q$label, "Client integration question")
  expect_identical(q$from_days_back, 14L)
  expect_identical(q$digest_mode, "threshold")
  expect_identical(q$min_score, 1.25)
  expect_true(q$email_enabled)
  expect_identical(q$email_to[[1]], c("a@example.org", "b@example.org"))
  expect_identical(q$study_filter[[1]], "randomized controlled trial")
  expect_setequal(q$delivery[[1]], c("file", "zotero"))
  expect_length(q$concepts[[1]], 2)
  expect_identical(q$concepts[[1]][[1]]$terms, c("semaglutide", "GLP-1"))
  expect_identical(q$concepts[[1]][[1]]$mesh, "Glucagon-Like Peptide 1")
})

test_that("the plaintext API key never comes back over the wire", {
  skip_without_server()
  conn <- test_conn()
  id <- "client_key_it"
  on.exit(try(rr_delete_question(conn, id), silent = TRUE), add = TRUE)
  rr_upsert_question(conn, id, "Key test",
                     concepts = rr_concept("a", "alpha"),
                     api_keys = list(pubmed = "PLAINTEXT-SHOULD-NEVER-COME-BACK"))
  q <- rr_question(conn, id)
  expect_false(any(grepl("PLAINTEXT-SHOULD-NEVER-COME-BACK",
                         unlist(lapply(q, as.character)), fixed = TRUE)))
  expect_true(isTRUE(q$api_key_present[[1]]$pubmed))
})

test_that("a rejected question never exposes its supplied API key", {
  skip_without_server()
  key <- "SYNTHETIC-VALIDATION-KEY-NEVER-DISPLAY"
  err <- tryCatch(rr_upsert_question(test_conn(), "client_invalid", "Invalid",
    rr_concept("drug", "test"), email_cron = "bad cron",
    api_keys = list(pubmed = key)), error = identity)
  expect_s3_class(err, "researchR_http_error")
  expect_false(grepl(key, conditionMessage(err), fixed = TRUE))
})

test_that("records come back typed, filtered and paginated", {
  skip_without_server()
  conn <- test_conn()
  qid <- Sys.getenv("RESEARCHR_TEST_QUESTION", "seeded")

  all <- rr_records(conn, qid)
  expect_s3_class(all, "tbl_df")
  expect_true(nrow(all) > 0)
  expect_s3_class(all$pub_date, "Date")
  expect_s3_class(all$first_seen, "POSIXct")
  expect_type(all$score, "double")
  expect_type(all$is_preprint, "integer")

  # pagination must return the same set as one big page, without duplicates
  paged <- rr_records(conn, qid, page_size = 1)
  expect_identical(sort(paged$dedup_key), sort(all$dedup_key))
  expect_false(any(duplicated(paged$dedup_key)))

  rct <- rr_records(conn, qid, study_type = "rct")
  expect_true(all(rct$study_type == "rct"))
  expect_true(nrow(rct) < nrow(all))

  pre <- rr_records(conn, qid, is_preprint = TRUE)
  expect_true(all(pre$is_preprint == 1L))

  expect_true(nrow(rr_records(conn, qid, n_max = 2)) <= 2)

  # the recall safety valve: thresholding is opt-in and never loses rows
  expect_true(nrow(rr_records(conn, qid, apply_threshold = TRUE)) <= nrow(all))
})

test_that("an unknown study type is caught against the server's taxonomy", {
  skip_without_server()
  conn <- test_conn()
  expect_error(rr_records(conn, "seeded", study_type = "not_a_bucket"),
               "Unknown study type")
})

test_that("an unknown question is a not-found condition", {
  skip_without_server()
  conn <- test_conn()
  expect_error(rr_records(conn, "no_such_question"),
               class = "researchR_not_found")
})

test_that("empty result sets still return a typed zero-row tibble", {
  skip_without_server()
  conn <- test_conn()
  qid <- Sys.getenv("RESEARCHR_TEST_QUESTION", "seeded")
  out <- rr_records(conn, qid, since = as.Date("2099-01-01"))
  expect_s3_class(out, "tbl_df")
  expect_identical(nrow(out), 0L)
  expect_true(all(c("doi", "title", "study_type") %in% names(out)))
})

test_that("signals honour min_confidence as a floor over the tiers", {
  skip_without_server()
  conn <- test_conn()
  qid <- Sys.getenv("RESEARCHR_TEST_QUESTION", "seeded")
  all <- rr_signals(conn, qid)
  expect_true(nrow(all) > 0)
  expect_type(all$extra, "list")
  rep <- rr_signals(conn, qid, min_confidence = "reported")
  expect_true(all(rep$confidence %in% c("reported", "authoritative")))
  auth <- rr_signals(conn, qid, min_confidence = "authoritative")
  expect_true(nrow(auth) <= nrow(rep))
  expect_true(nrow(rep) <= nrow(all))
})

test_that("source health reports the heartbeat", {
  skip_without_server()
  out <- rr_source_health(test_conn())
  expect_s3_class(out, "tbl_df")
  expect_true(all(c("source", "track", "stale") %in% names(out)))
})

test_that("signal config round-trips", {
  skip_without_server()
  conn <- test_conn()
  qid <- Sys.getenv("RESEARCHR_TEST_QUESTION", "seeded")
  rr_set_signal_config(conn, qid, min_confidence = "reported",
                       instant_alerts = TRUE,
                       scrape_targets = list(journal = list(name = "Synthetic feed",
                         type = "rss", url = "https://example.org/feed.xml")))
  cfg <- rr_signal_config(conn, qid)
  expect_identical(cfg$min_confidence, "reported")
  expect_true(isTRUE(cfg$instant_alerts))
  expect_identical(cfg$scrape_targets[[1]]$url, "https://example.org/feed.xml")
})

test_that("the service heartbeat is readable", {
  skip_without_server()
  out <- rr_heartbeat(test_conn())
  expect_s3_class(out, "tbl_df")
  expect_true(all(c("service", "last_run", "last_ok", "stale") %in% names(out)))
})
