# ---------------------------------------------------------------------------
# Records: the literature track.
# ---------------------------------------------------------------------------

RECORD_COLS <- c("dedup_key", "question_id", "source", "ext_id", "doi", "title",
                 "abstract", "authors", "journal", "pub_date", "url", "oa_url",
                 "type", "study_type", "is_preprint", "score", "first_seen",
                 "emailed")

#' Fetch stored literature records
#'
#' Pulls what researchd has already found and stored — this never triggers a
#' fetch from the upstream databases, which is the pipeline's own job on its
#' cron schedule.
#'
#' Results are paginated by the server; this function follows the pages for
#' you up to `n_max`.
#'
#' # Recall versus precision
#'
#' The server stores *everything* it finds and never drops a record. Scoring
#' orders results; it does not filter them. `apply_threshold = TRUE`
#' reproduces what the curated feed and the digest email show for questions
#' in `digest_mode = "threshold"`; the default `FALSE` gives you the complete
#' stored set, the equivalent of the `all.xml` safety valve.
#'
#' @param conn A [rr_connect()] connection.
#' @param question_id Question id.
#' @param study_type Optional single study type. Must be one of
#'   `rr_meta(conn)$study_types`.
#' @param is_preprint Optional logical: preprints only, or non-preprints only.
#' @param since,until Optional publication-date bounds. `Date` or
#'   `"YYYY-MM-DD"`.
#' @param min_score Optional score floor.
#' @param emailed Optional logical: filter on whether the record has already
#'   gone out in a digest.
#' @param apply_threshold Apply the question's own digest threshold.
#' @param n_max Maximum records to return.
#' @param page_size Records per request.
#' @return A tibble of records, one row each, with `pub_date` as `Date` and
#'   `first_seen` as `POSIXct`.
#' @examplesIf nzchar(Sys.getenv("RESEARCHR_URL"))
#' conn <- rr_connect()
#' rr_records(conn, "glp1_mash", study_type = "rct", since = Sys.Date() - 90)
#' @export
rr_records <- function(conn, question_id,
                       study_type = NULL, is_preprint = NULL,
                       since = NULL, until = NULL, min_score = NULL,
                       emailed = NULL, apply_threshold = FALSE,
                       n_max = Inf, page_size = 200) {
  if (!is.null(study_type)) rr_validate_study_type(conn, study_type)
  rows <- rr_paginate(conn, "records", "records",
                      query = list(question_id = question_id,
                                   study_type = study_type,
                                   is_preprint = is_preprint,
                                   since = since, until = until,
                                   min_score = min_score,
                                   emailed = emailed,
                                   apply_threshold = apply_threshold),
                      n_max = n_max, page_size = page_size)
  out <- rr_type_columns(rr_rows_to_tbl(rows, RECORD_COLS))
  out
}

SIGNAL_COLS <- c("dedup_key", "question_id", "signal_type", "source", "ext_id",
                 "title", "summary", "org", "signal_date", "url", "confidence",
                 "extra", "score", "first_seen", "emailed")

#' Fetch stored signals
#'
#' The signals track carries non-publication intelligence — grant awards,
#' trial registrations and results postings, regulatory decisions, SEC
#' filings, patents, press and investor-relations items. It runs off the same
#' questions as the literature track but is a deliberately separate stream,
#' because these objects are earlier and less reliable than papers.
#'
#' # Confidence tiers
#'
#' Every signal carries a confidence: `"authoritative"` (registries,
#' regulators, SEC), `"reported"` (official but preliminary, e.g. a topline
#' readout), or `"unverified"` (scraped press or IR — a lead, not a fact).
#' `min_confidence` is a **floor** over that ordering, not an equality test:
#' `"reported"` returns reported *and* authoritative.
#'
#' @param conn A [rr_connect()] connection.
#' @param question_id Question id.
#' @param signal_type Optional single signal type; see
#'   `rr_meta(conn)$signal_types`.
#' @param min_confidence Optional minimum confidence tier.
#' @param since Optional lower bound on `signal_date`.
#' @param n_max Maximum signals to return.
#' @param page_size Signals per request.
#' @return A tibble of signals. `extra` is parsed from JSON into a list
#'   column of type-specific fields.
#' @examplesIf nzchar(Sys.getenv("RESEARCHR_URL"))
#' rr_signals(rr_connect(), "glp1_mash", min_confidence = "reported")
#' @export
rr_signals <- function(conn, question_id, signal_type = NULL,
                       min_confidence = NULL, since = NULL,
                       n_max = Inf, page_size = 200) {
  rows <- rr_paginate(conn, "signals", "signals",
                      query = list(question_id = question_id,
                                   signal_type = signal_type,
                                   min_confidence = min_confidence,
                                   since = since),
                      n_max = n_max, page_size = page_size)
  out <- rr_type_columns(rr_rows_to_tbl(rows, SIGNAL_COLS))
  if ("extra" %in% names(out)) out$extra <- rr_json_column(out$extra)
  out
}

#' Walk the server's pages until n_max or exhaustion
#' @noRd
rr_paginate <- function(conn, path, key, query, n_max, page_size) {
  page_size <- max(1L, as.integer(page_size))
  out <- list(); offset <- 0L
  repeat {
    want <- if (is.finite(n_max)) min(page_size, n_max - length(out)) else page_size
    if (want <= 0) break
    res <- rr_get(conn, path, c(query, list(limit = want, offset = offset)))
    batch <- res[[key]] %||% list()
    out <- c(out, batch)
    offset <- offset + length(batch)
    if (!length(batch) || offset >= (res$total %||% 0)) break
  }
  out
}

#' @noRd
rr_validate_study_type <- function(conn, study_type) {
  known <- conn$meta$study_types %||% rr_meta(conn)$study_types
  if (!study_type %in% known) {
    cli::cli_abort(c("Unknown study type {.val {study_type}}.",
                     i = "The server's taxonomy is: {.val {known}}."))
  }
  invisible(TRUE)
}
