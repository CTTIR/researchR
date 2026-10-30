# ---------------------------------------------------------------------------
# Records: the literature track.
# ---------------------------------------------------------------------------

RECORD_COLS <- c("dedup_key", "question_id", "source", "ext_id", "doi", "title",
                 "abstract", "authors", "journal", "pub_date", "url", "oa_url",
                 "type", "study_type", "is_preprint", "score", "first_seen",
                 "emailed", "provenance")

#' Fetch stored literature records
#'
#' Pulls what researchD has already found and stored — this never triggers a
#' fetch from the upstream databases, which is the pipeline's own job on its
#' cron schedule.
#'
#' Results are paginated by the server; this function follows the pages for
#' you up to `n_max`. Offset pagination is not a database snapshot. If the
#' result count changes or a record repeats during paging, the request stops
#' with `researchR_pagination_changed`; retry after collection has finished.
#'
#' # Recall versus precision
#'
#' Scoring orders results without deleting below-threshold records.
#' `apply_threshold = TRUE`
#' reproduces what the curated feed and the digest email show for questions
#' in `digest_mode = "threshold"`; the default `FALSE` gives you the complete
#' stored set. RSS feeds, including `all.xml`, have a server-side item limit.
#' Configured server retention can remove old records.
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
#'   `first_seen` as UTC `POSIXct`. `provenance` is a list column of source
#'   contributors; it is empty for servers that do not expose provenance.
#' @examplesIf nzchar(Sys.getenv("RESEARCHR_URL"))
#' conn <- rr_connect()
#' rr_records(conn, "glp1_mash", study_type = "rct", since = Sys.Date() - 90)
#' @export
rr_records <- function(conn, question_id,
                       study_type = NULL, is_preprint = NULL,
                       since = NULL, until = NULL, min_score = NULL,
                       emailed = NULL, apply_threshold = FALSE,
                       n_max = Inf, page_size = 200) {
  rr_string(question_id, "question_id")
  rr_flag(is_preprint, "is_preprint", optional = TRUE)
  rr_flag(emailed, "emailed", optional = TRUE)
  rr_flag(apply_threshold, "apply_threshold")
  if (!is.null(min_score)) rr_number(min_score, "min_score")
  since <- rr_date_bound(since, "since")
  until <- rr_date_bound(until, "until")
  if (!is.null(since) && !is.null(until) && since > until) {
    cli::cli_abort("{.arg since} must not be later than {.arg until}.")
  }
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
  out$provenance <- rr_json_column(out$provenance)
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
  rr_string(question_id, "question_id")
  if (!is.null(signal_type)) rr_string(signal_type, "signal_type")
  if (!is.null(min_confidence)) {
    rr_string(min_confidence, "min_confidence")
    if (!min_confidence %in% c("unverified", "reported", "authoritative")) {
      cli::cli_abort("{.arg min_confidence} must be unverified, reported or authoritative.")
    }
  }
  since <- rr_date_bound(since, "since")
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
  rr_check_conn(conn)
  rr_number(n_max, "n_max", lower = 0, integer = TRUE, infinite = TRUE)
  rr_number(page_size, "page_size", lower = 1, integer = TRUE)
  max_page <- conn$meta$max_page %||% 1000L
  if (!is.numeric(max_page) || length(max_page) != 1L || is.na(max_page) ||
      !is.finite(max_page) || max_page < 1 || max_page != floor(max_page)) {
    cli::cli_abort("The server advertised an invalid maximum page size.",
                   class = "researchR_protocol_error")
  }
  page_size <- min(page_size, max_page)
  out <- list(); offset <- 0; expected_total <- NULL; seen <- character()
  repeat {
    want <- if (is.finite(n_max)) min(page_size, n_max - length(out)) else page_size
    if (want <= 0) break
    res <- rr_get(conn, path, c(query, list(limit = want, offset = offset)))
    batch <- res[[key]]
    total <- res$total
    if (!is.list(batch) || !all(vapply(batch, is.list, logical(1))) ||
        !is.numeric(total) || length(total) != 1L ||
        is.na(total) || !is.finite(total) || total < 0 || total != floor(total) ||
        length(batch) > want || offset + length(batch) > total) {
      cli::cli_abort("The server returned an invalid pagination response.",
                     class = "researchR_protocol_error")
    }
    ids <- vapply(batch, function(row) rr_scalar(row$dedup_key), character(1))
    if ((!is.null(expected_total) && total != expected_total) ||
        anyDuplicated(ids[!is.na(ids)]) || any(ids[!is.na(ids)] %in% seen)) {
      cli::cli_abort("The result set changed while paging. Retry after collection has finished.",
                     class = "researchR_pagination_changed")
    }
    if (length(batch) == 0L && offset < total) {
      cli::cli_abort("The server ended pagination before returning its reported total.",
                     class = "researchR_protocol_error")
    }
    expected_total <- total
    seen <- c(seen, ids[!is.na(ids)])
    out <- c(out, batch)
    offset <- offset + length(batch)
    if (!length(batch) || offset >= total) break
  }
  out
}

#' @noRd
rr_validate_study_type <- function(conn, study_type) {
  rr_string(study_type, "study_type")
  known <- conn$meta$study_types %||% rr_meta(conn)$study_types
  if (!study_type %in% known) {
    cli::cli_abort(c("Unknown study type {.val {study_type}}.",
                     i = "The server's taxonomy is: {.val {known}}."))
  }
  invisible(TRUE)
}
