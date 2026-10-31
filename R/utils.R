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
  if (is.numeric(x) && all(is.finite(x)) && all(x == floor(x))) {
    return(format(x, scientific = FALSE, trim = TRUE, digits = 22))
  }
  as.character(x)
}

#' Coerce a list of JSON objects into a tibble with a stable column set
#'
#' The server always returns the same fields, but an empty result set carries
#' no rows to infer them from -- so callers pass the expected columns and get
#' a correctly-typed zero-row tibble instead of a `list()`.
#' @noRd
rr_rows_to_tbl <- function(rows, cols) {
  rr_check_rows(rows)
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

#' Reject missing or malformed row collections instead of implying no results
#' @noRd
rr_check_rows <- function(rows) {
  if (!is.list(rows) || length(names(rows)) ||
      !all(vapply(rows, function(row) is.list(row) && length(names(row)) > 0L, logical(1)))) {
    cli::cli_abort("The server returned an invalid row collection.",
                   class = "researchR_protocol_error")
  }
  invisible(TRUE)
}

#' One JSON value -> one character scalar (NA for null/empty)
#' @noRd
rr_scalar <- function(x) {
  if (is.null(x) || !length(x)) return(NA_character_)
  if (length(x) > 1) return(paste(unlist(x), collapse = "; "))
  as.character(x[[1]])
}

#' Convert the columns researchD documents as typed
#' @noRd
rr_type_columns <- function(x) {
  num <- intersect(c("score", "min_score", "total"), names(x))
  for (nm in num) x[[nm]] <- suppressWarnings(as.numeric(x[[nm]]))
  int <- intersect(c("is_preprint", "emailed", "from_days_back", "error_runs",
                     "total_runs"), names(x))
  for (nm in int) x[[nm]] <- suppressWarnings(as.integer(x[[nm]]))
  for (nm in intersect(c("stale", "enabled", "email_enabled"), names(x))) {
    value <- tolower(as.character(x[[nm]]))
    x[[nm]] <- rep(NA, length(value))
    x[[nm]][value %in% c("true", "1")] <- TRUE
    x[[nm]][value %in% c("false", "0")] <- FALSE
  }
  dte <- intersect(c("pub_date", "signal_date"), names(x))
  for (nm in dte) x[[nm]] <- suppressWarnings(as.Date(x[[nm]], format = "%Y-%m-%d"))
  tme <- intersect(c("first_seen", "run_at", "last_ok", "last_nonempty",
                     "last_run", "email_last_sent"), names(x))
  for (nm in tme) x[[nm]] <- rr_parse_time(x[[nm]])
  x
}

#' Parse SQLite UTC timestamps and ISO 8601 timestamps with offsets
#' @noRd
rr_parse_time <- function(x) {
  x <- as.character(x)
  out <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = "UTC")
  # Base R's default parser can accept only the date prefix of an ISO string,
  # silently dropping both the time and UTC offset. Select an exact grammar.
  valid <- !is.na(x) & grepl(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}[T ][0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?(Z|[+-][0-9]{2}:?[0-9]{2})?$",
    x)
  norm <- sub(" ", "T", x[valid], fixed = TRUE)
  norm <- sub("Z$", "+0000", norm)
  norm <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", norm)
  has_offset <- grepl("[+-][0-9]{4}$", norm)
  norm[!has_offset] <- paste0(norm[!has_offset], "+0000")
  out[valid] <- suppressWarnings(as.POSIXct(norm, format = "%Y-%m-%dT%H:%M:%OS%z",
                                           tz = "UTC"))
  out
}

#' Validate scalar inputs before making a request
#' @noRd
rr_string <- function(x, name, empty = FALSE) {
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
      (!empty && !nzchar(trimws(x))) || grepl("[[:cntrl:]]", x)) {
    cli::cli_abort("{.arg {name}} must be one {if (empty) '' else 'non-empty '}character string without control characters.")
  }
  invisible(x)
}

#' @noRd
rr_flag <- function(x, name, optional = FALSE) {
  if (optional && is.null(x)) return(invisible(x))
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    cli::cli_abort("{.arg {name}} must be TRUE or FALSE.")
  }
  invisible(x)
}

#' @noRd
rr_number <- function(x, name, lower = -Inf, integer = FALSE, infinite = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < lower ||
      (!is.finite(x) && !(infinite && identical(x, Inf))) ||
      (integer && is.finite(x) && x != floor(x))) {
    cli::cli_abort("{.arg {name}} must be one {if (integer) 'whole number' else 'number'} >= {lower}{if (infinite) ' (or Inf)' else ''}.")
  }
  invisible(x)
}

#' Encode one route segment; slashes cannot be represented by these routes
#' @noRd
rr_path_id <- function(x, name = "question_id") {
  rr_string(x, name)
  if (grepl("[/\\\\]", x) || x %in% c(".", "..")) {
    cli::cli_abort("{.arg {name}} cannot contain a slash or be a dot path segment.")
  }
  utils::URLencode(x, reserved = TRUE, repeated = TRUE)
}

#' Validate a date bound without truncating malformed input
#' @noRd
rr_date_bound <- function(x, name) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    if (length(x) != 1L || is.na(x) || !is.finite(as.numeric(x))) {
      cli::cli_abort("{.arg {name}} must be one finite, non-missing date.")
    }
    x <- format(x, "%Y-%m-%d")
  }
  rr_string(x, name)
  parsed <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x) || is.na(parsed) ||
      format(parsed, "%Y-%m-%d") != x) {
    cli::cli_abort("{.arg {name}} must be a valid date in YYYY-MM-DD form.")
  }
  x
}

#' Parse a JSON string column back into a list column
#' @noRd
rr_json_column <- function(x, field = "metadata") {
  lapply(seq_along(x), function(i) {
    s <- x[[i]]
    if (is.na(s) || !nzchar(s)) return(list())
    out <- tryCatch(jsonlite::fromJSON(s, simplifyVector = FALSE), error = function(e) {
      cli::cli_abort("The server returned invalid {.field {field}} JSON in row {i}.",
                     class = "researchR_protocol_error")
    })
    if (is.null(out)) return(list())
    if (!is.list(out)) {
      cli::cli_abort("The server returned a non-structured {.field {field}} value in row {i}.",
                     class = "researchR_protocol_error")
    }
    out
  })
}

#' Force array fields to remain arrays even when R values have names
#' @noRd
rr_array <- function(x) unname(as.list(x))

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
