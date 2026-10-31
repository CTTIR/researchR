# ---------------------------------------------------------------------------
# Connections.
# ---------------------------------------------------------------------------

#' Connect to a researchD server
#'
#' Creates the connection object every other function in this package takes as
#' its first argument. Nothing is sent at connection time unless `check =
#' TRUE`. The default checks the server immediately; use `check = FALSE`
#' to construct an offline connection.
#'
#' # Authentication
#'
#' researchD exposes two disjoint authentication surfaces. The browser
#' dashboard is behind a password plus a TOTP second factor; the `/api/v1`
#' surface this package uses is behind a bearer token. A dashboard session is
#' deliberately *not* accepted as an API token, and vice versa.
#'
#' Set the token on the server in its `.env` as `API_TOKEN`, and locally as
#' the `RESEARCHR_TOKEN` environment variable (in `~/.Renviron`, so it never
#' reaches your scripts or your history). If the server has no `API_TOKEN`
#' set, the whole surface answers 503 rather than serving unauthenticated —
#' this package reports that as a `researchR_disabled` condition.
#'
#' A checked connection requires valid API v1 metadata. Malformed responses
#' raise `researchR_protocol_error`. Request errors redact the bearer token
#' and submitted source keys; do not include credentials in diagnostic reports.
#'
#' @param base_url Base URL of the researchD web service, e.g.
#'   `"https://feeds.example.org"`. Defaults to the `RESEARCHR_URL`
#'   environment variable.
#' @param token API bearer token. Defaults to the `RESEARCHR_TOKEN`
#'   environment variable. Never hard-code it in a script.
#' @param timeout Request timeout in seconds.
#' @param check Whether to contact the server and verify the token now.
#' @return An object of class `researchr_conn`.
#' @examplesIf nzchar(Sys.getenv("RESEARCHR_URL"))
#' conn <- rr_connect()
#' rr_questions(conn)
#' @export
rr_connect <- function(base_url = Sys.getenv("RESEARCHR_URL"),
                       token = Sys.getenv("RESEARCHR_TOKEN"),
                       timeout = 60,
                       check = TRUE) {
  if (is.null(base_url) || identical(base_url, "")) {
    cli::cli_abort(c(
      "No researchD URL given.",
      i = "Pass {.arg base_url}, or set {.envvar RESEARCHR_URL} in {.file ~/.Renviron}."))
  }
  if (is.null(token) || identical(token, "")) {
    cli::cli_abort(c(
      "No API token given.",
      i = "Pass {.arg token}, or set {.envvar RESEARCHR_TOKEN} in {.file ~/.Renviron}.",
      i = "It must match {.envvar API_TOKEN} in the server's {.file .env}."))
  }
  rr_string(base_url, "base_url")
  rr_string(token, "token")
  rr_number(timeout, "timeout", lower = .Machine$double.eps)
  rr_flag(check, "check")
  url <- tryCatch(httr2::url_parse(base_url), error = function(e) NULL)
  if (is.null(url) || is.null(url$scheme) || !url$scheme %in% c("http", "https") ||
      is.null(url$hostname) || !nzchar(url$hostname) ||
      nzchar(url$username %||% "") || nzchar(url$password %||% "") ||
      length(url$query) || nzchar(url$fragment %||% "")) {
    cli::cli_abort("{.arg base_url} must be an HTTP(S) server URL without embedded credentials, query parameters or a fragment.")
  }
  conn <- structure(
    list(base_url = sub("/+$", "", base_url),
         token = token,
         timeout = timeout,
         user_agent = paste0("researchR/",
                             utils::packageVersion("researchR"),
                             " (R client for researchD)")),
    class = "researchr_conn")
  if (isTRUE(check)) conn$meta <- rr_meta(conn)
  conn
}

#' @export
print.researchr_conn <- function(x, ...) {
  cli::cli_h1("researchD connection")
  cli::cli_li("server: {.url {x$base_url}}")
  cli::cli_li("token: {.emph set} ({nchar(x$token)} characters, not shown)")
  if (!is.null(x$meta)) {
    cli::cli_li("api version: {x$meta$api_version}")
    cli::cli_li("study types: {length(x$meta$study_types)}")
    cli::cli_li("signal types: {length(x$meta$signal_types)}")
  }
  invisible(x)
}

#' @noRd
rr_check_conn <- function(conn) {
  if (!inherits(conn, "researchr_conn")) {
    cli::cli_abort(c("{.arg conn} must be a connection from {.fun rr_connect}.",
                     x = "Got {.cls {class(conn)[1]}}."))
  }
  invisible(TRUE)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Server vocabulary and capabilities
#'
#' Returns the study-type taxonomy, signal types, confidence tiers and source
#' list the server is actually using. Worth calling after a server upgrade:
#' it is the authoritative vocabulary, so validating against it is how the
#' client avoids drifting from the server's classifier.
#'
#' @param conn A [rr_connect()] connection.
#' @return A named list.
#' @examplesIf nzchar(Sys.getenv("RESEARCHR_URL"))
#' rr_meta(rr_connect())$study_types
#' @export
rr_meta <- function(conn) {
  out <- rr_get(conn, "meta")
  if (!is.list(out) || !is.numeric(out$api_version) ||
      length(out$api_version) != 1L || is.na(out$api_version) || out$api_version != 1) {
    cli::cli_abort("The server did not return supported API v1 metadata. Check the server URL and version.",
                   class = "researchR_protocol_error")
  }
  for (field in c("study_types", "signal_types")) {
    values <- out[[field]]
    if (!length(values) || !all(vapply(values, function(x)
        is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x), logical(1)))) {
      cli::cli_abort("The server returned invalid {.field {field}} metadata.",
                     class = "researchR_protocol_error")
    }
  }
  lapply(out, function(x) if (is.list(x) && !is.null(names(x))) x else unlist(x))
}

#' Is the server up?
#'
#' Hits the unauthenticated `/healthz` endpoint, so this answers "is the web
#' service running" independently of whether the token is any good.
#'
#' @param conn A [rr_connect()] connection.
#' @return `TRUE` if the server answered `ok`, otherwise `FALSE`.
#' @examplesIf nzchar(Sys.getenv("RESEARCHR_URL"))
#' rr_health(rr_connect(check = FALSE))
#' @export
rr_health <- function(conn) {
  rr_check_conn(conn)
  req <- httr2::request(conn$base_url)
  req <- httr2::req_url_path_append(req, "healthz")
  req <- httr2::req_timeout(req, conn$timeout)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  ok <- tryCatch({
    resp <- httr2::req_perform(req)
    httr2::resp_status(resp) == 200L &&
      identical(trimws(httr2::resp_body_string(resp)), "ok")
  }, error = function(e) FALSE)
  ok
}
