# ---------------------------------------------------------------------------
# Low-level request layer.
#
# Every call goes through rr_req(), so authentication, error translation and
# the "surface disabled" case are handled in exactly one place.
# ---------------------------------------------------------------------------

#' Build the request for one API path
#'
#' @param conn A [rr_connect()] connection.
#' @param path Path below `/api/v1`, e.g. `"records"`.
#' @param query Named list of query parameters. `NULL` entries are dropped.
#' @return An [httr2::request()] object.
#' @noRd
rr_req <- function(conn, path, query = list()) {
  rr_check_conn(conn)
  req <- httr2::request(conn$base_url)
  req <- httr2::req_url_path_append(req, "api", "v1", path)
  req <- httr2::req_headers(req, Authorization = paste("Bearer", conn$token))
  req <- httr2::req_user_agent(req, conn$user_agent)
  req <- httr2::req_timeout(req, conn$timeout)
  # Let rr_perform() translate the server's own error bodies instead of
  # httr2 throwing a generic HTTP condition.
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  q <- rr_drop_null(query)
  if (length(q)) req <- httr2::req_url_query(req, !!!lapply(q, rr_query_value))
  req
}

#' Perform a request and translate failures into researchR conditions
#' @noRd
rr_perform <- function(req, conn, secrets = character()) {
  secrets <- c(conn$token, secrets)
  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      reason <- rr_redact(conditionMessage(e), secrets)
      cli::cli_abort(
        c("Could not reach the researchD server at {.url {conn$base_url}}.",
          x = "{reason}",
          i = "Is the container running, and is {.arg base_url} right?"),
        class = "researchR_unreachable", call = NULL)
    })

  status <- httr2::resp_status(resp)
  if (status < 400) return(resp)

  detail <- rr_redact(rr_error_detail(resp), secrets)
  if (status == 401) {
    cli::cli_abort(
      c("The researchD server rejected the API token (HTTP 401).",
        i = "Check {.envvar RESEARCHR_TOKEN} against {.envvar API_TOKEN} in the server's {.file .env}."),
      class = "researchR_unauthorized", call = NULL)
  }
  if (status == 503 && grepl("API disabled", detail, fixed = TRUE)) {
    cli::cli_abort(
      c("The researchD API surface is disabled (HTTP 503).",
        i = "Set {.envvar API_TOKEN} in the server's {.file .env} and restart the {.code web} service.",
        x = "{detail}"),
      class = "researchR_disabled", call = NULL)
  }
  if (status == 503) {
    cli::cli_abort(c("The researchD server is temporarily unavailable (HTTP 503).",
                    x = "{detail}"),
                   class = c("researchR_unavailable", "researchR_http_error"), call = NULL)
  }
  if (status == 404) {
    cli::cli_abort(c("Not found (HTTP 404).", x = "{detail}"),
                   class = "researchR_not_found", call = NULL)
  }
  cli::cli_abort(c("researchD returned HTTP {status}.", x = "{detail}"),
                 class = "researchR_http_error", call = NULL)
}

#' Extract FastAPI's `detail` field, falling back to the raw body
#' @noRd
rr_error_detail <- function(resp) {
  out <- tryCatch(httr2::resp_body_json(resp)$detail, error = function(e) NULL)
  if (is.null(out)) out <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
  if (!length(out)) return("(no detail returned)")
  # A 422 returns `detail` as a LIST of per-field errors, so the list branch
  # has to come before any scalar test -- nzchar() on a length-4 list throws.
  if (is.list(out)) {
    # FastAPI validation errors may echo the entire input body, including
    # write-only API keys. Keep error locations/messages, never echoed inputs.
    strip_input <- function(x) {
      if (!is.list(x)) return(x)
      if (!is.null(names(x))) {
        x <- x[!tolower(names(x)) %in% c("input", "ctx", "api_keys", "token", "authorization")]
      }
      lapply(x, strip_input)
    }
    out <- strip_input(out)
    return(paste(as.character(jsonlite::toJSON(out, auto_unbox = TRUE)), collapse = " "))
  }
  out <- as.character(out)[1]
  if (is.na(out) || !nzchar(out)) "(no detail returned)" else out
}

#' Redact credentials echoed as plain, JSON-escaped or URL-encoded text
#' @noRd
rr_redact <- function(text, secrets) {
  secrets <- unique(as.character(secrets))
  secrets <- secrets[!is.na(secrets) & nzchar(secrets)]
  variants <- unlist(lapply(secrets, function(secret) {
    encoded <- utils::URLencode(secret, reserved = TRUE, repeated = TRUE)
    lower_hex <- encoded
    matches <- gregexpr("%[0-9A-Fa-f]{2}", lower_hex)
    regmatches(lower_hex, matches) <- lapply(regmatches(lower_hex, matches), tolower)
    forms <- unique(c(secret, encoded, lower_hex,
                       gsub("%20", "+", encoded, fixed = TRUE),
                       gsub("%20", "+", lower_hex, fixed = TRUE)))
    escaped <- vapply(forms, function(value) {
      json <- as.character(jsonlite::toJSON(value, auto_unbox = TRUE))
      substr(json, 2L, nchar(json) - 1L)
    }, character(1))
    c(forms, escaped)
  }), use.names = FALSE)
  variants <- unique(variants)
  for (secret in variants[order(nchar(variants), decreasing = TRUE)]) {
    text <- gsub(secret, "[REDACTED]", text, fixed = TRUE)
  }
  text
}

#' Read a successful JSON response without reporting its potentially private body
#' @noRd
rr_response_json <- function(resp) {
  # Force the request outside the parser handler: transport/HTTP errors must
  # retain their own condition classes rather than becoming JSON failures.
  force(resp)
  out <- tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE), error = function(e) {
    cli::cli_abort("The server returned invalid JSON. Check the server and reverse-proxy configuration.",
                   class = "researchR_protocol_error", call = NULL)
  })
  if (!is.list(out)) {
    cli::cli_abort("The server returned an unexpected JSON value instead of an API object or array.",
                   class = "researchR_protocol_error", call = NULL)
  }
  out
}

#' GET one path and return the parsed JSON body
#' @noRd
rr_get <- function(conn, path, query = list()) {
  rr_response_json(rr_perform(rr_req(conn, path, query), conn))
}

#' POST a JSON body
#' @noRd
rr_post <- function(conn, path, body) {
  req <- httr2::req_body_json(rr_req(conn, path), body, auto_unbox = TRUE)
  rr_response_json(rr_perform(req, conn, secrets = unlist(body$api_keys, use.names = FALSE)))
}

#' DELETE one path
#' @noRd
rr_delete_path <- function(conn, path) {
  req <- httr2::req_method(rr_req(conn, path), "DELETE")
  rr_response_json(rr_perform(req, conn))
}
