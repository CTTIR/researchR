# ---------------------------------------------------------------------------
# Operational views: source health, signal configuration, feed URLs.
# ---------------------------------------------------------------------------

#' Per-source fetch health
#'
#' A surveillance system's worst failure is a source that quietly stopped
#' returning anything, because the symptom — fewer results — looks exactly
#' like a quiet week in the literature. Every pipeline run writes a heartbeat
#' row per source; this reports the last successful fetch, the last one that
#' actually returned rows, and the error count.
#'
#' Watch `last_nonempty` as much as `stale`: a source can be answering
#' happily while its query has silently stopped matching anything.
#'
#' @param conn A [rr_connect()] connection.
#' @param stale_days A source with no successful fetch in this many days is
#'   flagged `stale`.
#' @return A tibble, one row per source and track.
#' @examplesIf nzchar(Sys.getenv("RESEARCHR_URL"))
#' rr_source_health(rr_connect())
#' @export
rr_source_health <- function(conn, stale_days = 3) {
  rr_check_conn(conn)
  rr_number(stale_days, "stale_days", lower = 1, integer = TRUE)
  res <- rr_get(conn, "source_health", list(stale_days = stale_days))
  rows <- res$sources
  out <- rr_rows_to_tbl(rows, c("source", "track", "last_ok", "last_nonempty",
                                "error_runs", "total_runs", "stale"))
  rr_type_columns(out)
}

#' Is each service still alive?
#'
#' `rr_source_health()` answers "is this *source* still returning anything".
#' This answers the prior question: "did the engine and the scheduler run at
#' all". The distinction matters because a crashed engine writes no source
#' heartbeats either, so without this the two failures look identical — and
#' both look like a quiet week in the literature.
#'
#' @param conn A [rr_connect()] connection.
#' @return A tibble with one row per service: when it last ran, when it last
#'   ran *successfully*, a short detail string, the most recent error, and a
#'   `stale` flag.
#' @examplesIf nzchar(Sys.getenv("RESEARCHR_URL"))
#' rr_heartbeat(rr_connect())
#' @export
rr_heartbeat <- function(conn) {
  rr_check_conn(conn)
  res <- rr_get(conn, "heartbeat")
  out <- rr_rows_to_tbl(res$services,
                        c("service", "last_run", "last_ok", "last_detail",
                          "last_error", "stale"))
  rr_type_columns(out)
}

#' Read a question's signals configuration
#'
#' @param conn A [rr_connect()] connection.
#' @param question_id Question id.
#' @return A named list: enabled signal sources, minimum confidence, whether
#'   instant alerts are on, enabled feed categories, scrape targets, and
#'   which signal API keys are stored (never the keys themselves).
#' @examplesIf nzchar(Sys.getenv("RESEARCHR_URL"))
#' rr_signal_config(rr_connect(), "glp1_mash")
#' @export
rr_signal_config <- function(conn, question_id) {
  rr_check_conn(conn)
  rr_get(conn, paste0("signal_config/", rr_path_id(question_id)))
}

#' Update a question's signals configuration
#'
#' @param conn A [rr_connect()] connection.
#' @param question_id Question id.
#' @param sources Named list of signal-source toggles.
#' @param min_confidence Minimum confidence for the signals digest and the
#'   default signals feed. One of `"unverified"`, `"reported"`,
#'   `"authoritative"`.
#' @param instant_alerts If `TRUE`, high-priority signals are emailed on the
#'   next scheduler tick rather than waiting for the digest cadence.
#' @param feed_categories Named list of curated-feed category toggles.
#' @param scrape_targets List of scrape target definitions. Optional list names
#'   are ignored.
#' @param api_keys Named list of plaintext signal API keys. Write-only, as
#'   with [rr_upsert_question()].
#' @return Invisibly, the configuration as the server now holds it.
#' @examples
#' \dontrun{
#' # Not run: needs a live researchD server with an API token, and it
#' # changes the server's stored configuration.
#' rr_set_signal_config(rr_connect(), "glp1_mash",
#'                      min_confidence = "reported", instant_alerts = TRUE)
#' }
#' @export
rr_set_signal_config <- function(conn, question_id,
                                 sources = NULL,
                                 min_confidence = NULL,
                                 instant_alerts = NULL,
                                 feed_categories = NULL,
                                 scrape_targets = NULL,
                                 api_keys = NULL) {
  rr_check_conn(conn)
  if (!is.null(min_confidence)) {
    rr_string(min_confidence, "min_confidence")
    if (!min_confidence %in% c("unverified", "reported", "authoritative")) {
      cli::cli_abort("{.arg min_confidence} must be unverified, reported or authoritative.")
    }
  }
  rr_flag(instant_alerts, "instant_alerts", optional = TRUE)
  rr_named_map(sources, "sources", "flag")
  rr_named_map(feed_categories, "feed_categories", "flag")
  rr_named_map(api_keys, "api_keys", "string")
  if (!is.null(scrape_targets) && (!is.list(scrape_targets) ||
      !all(vapply(scrape_targets, function(x) is.list(x) && length(names(x)) > 0L, logical(1))))) {
    cli::cli_abort(c("{.arg scrape_targets} must be a list of target definitions.",
                     i = "Wrap a single target: {.code list(list(type = \"rss\", url = ...))}."))
  }
  current <- rr_signal_config(conn, question_id)
  body <- list(
    sources = rr_obj(sources %||% current$sources),
    min_confidence = min_confidence %||% (current$min_confidence %||% "unverified"),
    instant_alerts = if (is.null(instant_alerts)) isTRUE(current$instant_alerts)
                     else isTRUE(instant_alerts),
    feed_categories = rr_obj(feed_categories %||% current$feed_categories),
    scrape_targets = rr_array(scrape_targets %||% current$scrape_targets),
    api_keys = rr_obj(api_keys))
  rr_post(conn, paste0("signal_config/", rr_path_id(question_id)), body)
  cli::cli_alert_success("Signal config for {.val {question_id}} saved.")
  invisible(rr_signal_config(conn, question_id))
}

#' Feed URLs for a question
#'
#' The feeds are open — no token — so these URLs can go straight into Zotero
#' (My Library, right-click, New Feed, From URL) or any reader.
#'
#' @param conn A [rr_connect()] connection.
#' @param question_id Question id.
#' @param study_type Optional study type, or signal type when `signals = TRUE`,
#'   for a filtered feed. URL construction does not query server metadata.
#'   The feed names `"all"` and `"preprints"` are not study types; use the
#'   `all` or `preprints` argument for those feeds.
#' @param all If `TRUE`, ignore the question's digest threshold (literature)
#'   or its signal confidence floor (signals). All feeds
#'   still contain at most the server's configured `FEED_LIMIT` recent items;
#'   use [rr_records()] or [rr_signals()] for the retained collection.
#' @param preprints If `TRUE`, the preprints-only literature feed. Cannot be
#'   combined with `signals = TRUE`.
#' @param signals If `TRUE`, the signals stream rather than the literature one.
#' @return A single URL string.
#' @examples
#' # Building a feed URL is local: an offline connection is enough.
#' conn <- rr_connect("https://feeds.example.org", token = "not-a-real-token",
#'                    check = FALSE)
#' rr_feed_url(conn, "glp1_mash")
#' rr_feed_url(conn, "glp1_mash", study_type = "rct")
#' rr_feed_url(conn, "glp1_mash", signals = TRUE)
#' @export
rr_feed_url <- function(conn, question_id, study_type = NULL,
                        all = FALSE, preprints = FALSE, signals = FALSE) {
  rr_check_conn(conn)
  rr_flag(all, "all")
  rr_flag(preprints, "preprints")
  rr_flag(signals, "signals")
  question_id <- rr_path_id(question_id)
  if (!is.null(study_type)) {
    study_type <- rr_path_id(study_type, "study_type")
    # These names are routed to the unfiltered and preprint feeds, so a type
    # filter with that name would silently return a different stream.
    reserved <- if (signals) "all" else c("all", "preprints")
    if (study_type %in% reserved) {
      cli::cli_abort(c(
        "{.val {study_type}} is a feed name, not a {if (signals) 'signal' else 'study'} type.",
        i = "Use {.code {study_type} = TRUE} for that feed."))
    }
  }
  if (signals && preprints) {
    cli::cli_abort("{.arg preprints} is only available for literature feeds; it cannot be combined with {.arg signals}.")
  }
  n_set <- sum(!is.null(study_type), isTRUE(all), isTRUE(preprints))
  if (n_set > 1) {
    cli::cli_abort("Choose at most one of {.arg study_type}, {.arg all}, {.arg preprints}.")
  }
  stem <- if (isTRUE(signals)) "signals" else "feed"
  tail <- if (!is.null(study_type)) paste0(question_id, "/", study_type, ".xml")
          else if (isTRUE(all)) paste0(question_id, "/all.xml")
          else if (isTRUE(preprints)) paste0(question_id, "/preprints.xml")
          else paste0(question_id, ".xml")
  paste0(conn$base_url, "/", stem, "/", tail)
}
