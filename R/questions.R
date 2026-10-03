# ---------------------------------------------------------------------------
# Questions: the surveillance searches themselves.
# ---------------------------------------------------------------------------

QUESTION_COLS <- c("id", "label", "enabled", "from_days_back", "digest_mode",
                   "min_score", "email_enabled", "email_cron",
                   "email_last_sent")

#' List the surveillance questions
#'
#' @param conn A [rr_connect()] connection.
#' @return A tibble, one row per question. The list-valued fields
#'   (`concepts`, `sources`, `email_to`, `study_filter`, `delivery`,
#'   `api_key_present`) come back as list columns.
#'
#'   API keys are never returned by the server: `api_key_present` reports
#'   only *which* sources have a key stored, never the key itself.
#' @examplesIf nzchar(Sys.getenv("RESEARCHR_URL"))
#' rr_questions(rr_connect())
#' @export
rr_questions <- function(conn) {
  rows <- rr_get(conn, "questions")
  if (!length(rows)) return(rr_rows_to_tbl(list(), QUESTION_COLS))
  out <- tibble::tibble(
    id             = vapply(rows, function(r) rr_scalar(r$id), character(1)),
    label          = vapply(rows, function(r) rr_scalar(r$label), character(1)),
    enabled        = vapply(rows, function(r) isTRUE(r$enabled), logical(1)),
    from_days_back = vapply(rows, function(r) as.integer(r$from_days_back %||% NA), integer(1)),
    digest_mode    = vapply(rows, function(r) rr_scalar(r$digest_mode), character(1)),
    min_score      = vapply(rows, function(r) as.numeric(r$min_score %||% NA), numeric(1)),
    email_enabled  = vapply(rows, function(r) isTRUE(r$email_enabled), logical(1)),
    email_cron     = vapply(rows, function(r) rr_scalar(r$email_cron), character(1)),
    email_last_sent = vapply(rows, function(r) rr_scalar(r$email_last_sent), character(1)),
    email_to       = lapply(rows, function(r) unlist(r$email_to) %||% character(0)),
    study_filter   = lapply(rows, function(r) unlist(r$study_filter) %||% character(0)),
    delivery       = lapply(rows, function(r) unlist(r$delivery) %||% character(0)),
    sources        = lapply(rows, function(r) r$sources %||% list()),
    api_key_present = lapply(rows, function(r) r$api_key_present %||% list()),
    concepts       = lapply(rows, function(r) lapply(r$concepts, rr_as_concept)))
  out$email_last_sent <- suppressWarnings(as.POSIXct(out$email_last_sent, tz = "UTC"))
  out
}

#' Fetch one question by id
#'
#' @param conn A [rr_connect()] connection.
#' @param question_id Question id (the slug).
#' @return A one-row tibble, as in [rr_questions()].
#' @examplesIf nzchar(Sys.getenv("RESEARCHR_URL"))
#' rr_question(rr_connect(), "glp1_mash")
#' @export
rr_question <- function(conn, question_id) {
  all <- rr_questions(conn)
  hit <- all[all$id == question_id, , drop = FALSE]
  if (!nrow(hit)) {
    cli::cli_abort(c("No question with id {.val {question_id}}.",
                     i = "Known ids: {.val {all$id}}."),
                   class = "researchR_not_found")
  }
  hit
}

#' Describe one concept block of a question
#'
#' A question is a list of concepts, AND-joined at query time; the terms and
#' MeSH headings within one concept are OR-joined. So a two-concept question
#' `(semaglutide OR "GLP-1") AND (MASH OR steatohepatitis)` is two calls to
#' this function.
#'
#' @param name Human-readable concept name, e.g. `"drug"`.
#' @param terms Character vector of free-text synonyms.
#' @param mesh Character vector of MeSH headings.
#' @return A list of class `researchr_concept`.
#' @examples
#' rr_concept("drug", terms = c("semaglutide", "GLP-1"))
#' @export
rr_concept <- function(name, terms = character(0), mesh = character(0)) {
  structure(list(name = as.character(name),
                 terms = as.character(terms),
                 mesh = as.character(mesh)),
            class = c("researchr_concept", "list"))
}

#' @noRd
rr_as_concept <- function(x) {
  rr_concept(x$name %||% "", unlist(x$terms) %||% character(0),
             unlist(x$mesh) %||% character(0))
}

#' @export
print.researchr_concept <- function(x, ...) {
  cli::cli_text("{.strong {x$name}}: {.val {x$terms}}")
  if (length(x$mesh)) cli::cli_text("  MeSH: {.val {x$mesh}}")
  invisible(x)
}

#' Create or update a question
#'
#' Upserts by `question_id`: an existing question with that id is updated in
#' place, otherwise a new one is created. Concepts are replaced wholesale, so
#' pass the full set you want.
#'
#' # API keys
#'
#' `api_keys` takes *plaintext* keys and is write-only: the server encrypts
#' them at rest and never returns them. Leave a source out to keep its stored
#' key untouched; pass the literal `"__CLEAR__"` to delete one.
#'
#' @param conn A [rr_connect()] connection.
#' @param question_id Question id (a slug, e.g. `"glp1_mash"`).
#' @param label Human-readable label.
#' @param concepts List of [rr_concept()] blocks.
#' @param sources Named list of per-source toggles, e.g.
#'   `list(pubmed = TRUE, biorxiv = FALSE)`. Sources you do not name fall back
#'   to the server's global defaults.
#' @param api_keys Named list of plaintext per-source API keys. Write-only.
#' @param study_filter Character vector of publication types to restrict to.
#'   Empty means maximum recall.
#' @param from_days_back Lookback window, in days.
#' @param enabled Whether the question is fetched at all.
#' @param digest_mode `"all"` or `"threshold"`. Under `"threshold"` the
#'   curated feed and the email show only records scoring at least
#'   `min_score`; everything is still stored and still visible in
#'   `all.xml`.
#' @param min_score Score cutoff used when `digest_mode = "threshold"`.
#' @param delivery Character vector of delivery channels, e.g.
#'   `c("file", "zotero")`.
#' @param email_enabled Whether this question emails a digest.
#' @param email_cron Cron expression for this question's own email cadence.
#' @param email_to Character vector of recipient addresses.
#' @return Invisibly, the question as the server now holds it.
#' @examplesIf nzchar(Sys.getenv("RESEARCHR_URL"))
#' rr_upsert_question(
#'   rr_connect(), "glp1_mash", "GLP-1 receptor agonists in MASH",
#'   concepts = list(
#'     rr_concept("drug", c("semaglutide", "tirzepatide", "GLP-1")),
#'     rr_concept("disease", c("MASH", "NASH", "steatohepatitis"))),
#'   sources = list(pubmed = TRUE, europepmc = TRUE),
#'   email_enabled = TRUE, email_cron = "0 7 * * 1",
#'   email_to = "me@example.org")
#' @export
rr_upsert_question <- function(conn, question_id, label, concepts,
                               sources = list(),
                               api_keys = list(),
                               study_filter = character(0),
                               from_days_back = 7L,
                               enabled = TRUE,
                               digest_mode = c("all", "threshold"),
                               min_score = 0,
                               delivery = "file",
                               email_enabled = FALSE,
                               email_cron = "0 7 * * 1",
                               email_to = character(0)) {
  digest_mode <- match.arg(digest_mode)
  if (inherits(concepts, "researchr_concept")) concepts <- list(concepts)
  if (!length(concepts)) {
    cli::cli_abort("A question needs at least one concept; see {.fun rr_concept}.")
  }
  body <- list(
    id = question_id, label = label,
    concepts = lapply(concepts, function(cpt)
      list(name = cpt$name, terms = as.list(cpt$terms), mesh = as.list(cpt$mesh))),
    study_filter = as.list(study_filter),
    from_days_back = as.integer(from_days_back),
    enabled = isTRUE(enabled),
    sources = rr_obj(sources),
    api_keys = rr_obj(api_keys),
    delivery = as.list(delivery),
    email_enabled = isTRUE(email_enabled),
    email_cron = email_cron,
    email_to = as.list(email_to),
    digest_mode = digest_mode,
    min_score = as.numeric(min_score))
  rr_post(conn, "questions", body)
  cli::cli_alert_success("Question {.val {question_id}} saved.")
  invisible(rr_question(conn, question_id))
}

#' Delete a question
#'
#' Deleting a question cascades: its concepts, stored records, signals and
#' signal configuration go with it. This cannot be undone.
#'
#' @param conn A [rr_connect()] connection.
#' @param question_id Question id.
#' @return Invisibly, `TRUE`.
#' @examplesIf nzchar(Sys.getenv("RESEARCHR_URL"))
#' rr_delete_question(rr_connect(), "obsolete_question")
#' @export
rr_delete_question <- function(conn, question_id) {
  rr_delete_path(conn, paste0("questions/", question_id))
  cli::cli_alert_success("Question {.val {question_id}} deleted.")
  invisible(TRUE)
}
