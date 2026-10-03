# ---------------------------------------------------------------------------
# Small helpers shared across the client.
# ---------------------------------------------------------------------------

#' Drop NULL entries from a list
#' @noRd
rr_drop_null <- function(x) x[!vapply(x, is.null, logical(1))]

#' Render one query value the way FastAPI expects it
#'
#' Logicals must go over the wire as `true`/`false`, not R's `TRUE`/`FALSE`,
#' and dates as ISO strings.
#' @noRd
rr_query_value <- function(x) {
  if (is.logical(x)) return(if (isTRUE(x)) "true" else "false")
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (inherits(x, "POSIXt")) return(format(x, "%Y-%m-%d"))
  as.character(x)
}

#' Coerce a list of JSON objects into a tibble with a stable column set
#'
#' The server always returns the same fields, but an empty result set carries
#' no rows to infer them from -- so callers pass the expected columns and get
#' a correctly-typed zero-row tibble instead of a `list()`.
#' @noRd
rr_rows_to_tbl <- function(rows, cols) {
  if (!length(rows)) {
    empty <- lapply(cols, function(.) character(0))
    names(empty) <- cols
    return(tibble::as_tibble(empty))
  }
  cols <- union(cols, unique(unlist(lapply(rows, names))))
  out <- lapply(cols, function(nm)
    vapply(rows, function(r) rr_scalar(r[[nm]]), character(1)))
  names(out) <- cols
  tibble::as_tibble(out)
}

#' One JSON value -> one character scalar (NA for null/empty)
#' @noRd
rr_scalar <- function(x) {
  if (is.null(x) || !length(x)) return(NA_character_)
  if (length(x) > 1) return(paste(unlist(x), collapse = "; "))
  as.character(x[[1]])
}

#' Convert the columns researchd documents as typed
#' @noRd
rr_type_columns <- function(x) {
  num <- intersect(c("score", "min_score", "from_days_back", "total"), names(x))
  for (nm in num) x[[nm]] <- suppressWarnings(as.numeric(x[[nm]]))
  int <- intersect(c("is_preprint", "emailed"), names(x))
  for (nm in int) x[[nm]] <- suppressWarnings(as.integer(x[[nm]]))
  dte <- intersect(c("pub_date", "signal_date"), names(x))
  for (nm in dte) x[[nm]] <- suppressWarnings(as.Date(x[[nm]], format = "%Y-%m-%d"))
  tme <- intersect(c("first_seen", "run_at", "last_ok", "last_nonempty",
                     "last_run"), names(x))
  for (nm in tme) x[[nm]] <- suppressWarnings(as.POSIXct(x[[nm]], tz = "UTC"))
  x
}

#' Parse a JSON string column back into a list column
#' @noRd
rr_json_column <- function(x) {
  lapply(x, function(s) {
    if (is.na(s) || !nzchar(s)) return(list())
    tryCatch(jsonlite::fromJSON(s, simplifyVector = FALSE), error = function(e) list())
  })
}

#' Force a named-list field to serialise as a JSON object, never an array
#'
#' `jsonlite::toJSON(list())` is `[]`, but the server's Pydantic models type
#' these fields as objects, so an empty one is rejected with a 422. Anything
#' the API declares as `dict` must go through here.
#' @noRd
rr_obj <- function(x) {
  if (is.null(x)) x <- list()
  if (!length(x)) return(structure(list(), names = character(0)))
  as.list(x)
}
