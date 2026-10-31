json_response <- function(status, detail) httr2::response(
  status_code = status, headers = list(`content-type` = "application/json"),
  body = charToRaw(as.character(jsonlite::toJSON(list(detail = detail), auto_unbox = TRUE))))

test_that("a transient 503 is not mistaken for an unset API token", {
  response <- json_response(503L, "Database temporarily unavailable")
  local_mocked_bindings(req_perform = function(...) response, .package = "httr2")
  conn <- rr_connect("https://feeds.example.org", "synthetic", check = FALSE)
  expect_error(rr_meta(conn), class = "researchR_unavailable")
  response <- json_response(503L, "API disabled (no API_TOKEN set)")
  expect_error(rr_meta(conn), class = "researchR_disabled")
  response <- json_response(422L, list(list(loc = list("body", "id"), msg = "invalid slug")))
  expect_error(rr_meta(conn), "invalid slug", class = "researchR_http_error")
  response <- json_response(400L, "Unexpected {braces} in upstream input")
  expect_error(rr_meta(conn), "Unexpected \\{braces\\}", class = "researchR_http_error")
})

test_that("liveness requires an HTTP success as well as the expected body", {
  response <- httr2::response(status_code = 500L, body = charToRaw("ok"))
  local_mocked_bindings(req_perform = function(...) response, .package = "httr2")
  conn <- rr_connect("https://feeds.example.org", "synthetic", check = FALSE)
  expect_false(rr_health(conn))
  response <- httr2::response(status_code = 200L, body = charToRaw("ok\n"))
  expect_true(rr_health(conn))
})

test_that("validation failures never display echoed write-only credentials", {
  token <- "synthetic-bearer-secret"
  key <- "synthetic-source-secret"
  response <- json_response(422L, list(list(
    type = "value_error", loc = list("body"), msg = "invalid cron",
    input = list(api_keys = list(pubmed = key)),
    ctx = list(error = paste("request carried", key)))))
  local_mocked_bindings(req_perform = function(...) response, .package = "httr2")
  conn <- rr_connect("https://feeds.example.org", token, check = FALSE)
  save_question <- function() rr_upsert_question(conn, "q", "Q",
    rr_concept("drug", "test"), api_keys = list(pubmed = key))
  err <- tryCatch(save_question(), error = identity)
  expect_s3_class(err, "researchR_http_error")
  expect_match(conditionMessage(err), "invalid cron")
  expect_false(grepl(key, conditionMessage(err), fixed = TRUE))

  # Older servers/proxies may echo values in plain messages rather than
  # validation input fields. Both submitted keys and the bearer are redacted.
  response <- json_response(400L, paste("Rejected", key, "and", token))
  err <- tryCatch(save_question(), error = identity)
  expect_match(conditionMessage(err), "REDACTED")
  expect_false(grepl(key, conditionMessage(err), fixed = TRUE))
  expect_false(grepl(token, conditionMessage(err), fixed = TRUE))
})

test_that("a checked connection rejects malformed JSON and incompatible metadata", {
  response <- httr2::response(200L, headers = list(`content-type` = "application/json"),
                              body = charToRaw("<html>Proxy login</html>"))
  local_mocked_bindings(req_perform = function(...) response, .package = "httr2")
  connect <- function() rr_connect("https://feeds.example.org", "synthetic")
  expect_error(connect(), "invalid JSON", class = "researchR_protocol_error")
  for (body in c('null', 'true', '{"status":"ok"}', '{"api_version":2}',
                  '{"api_version":1,"study_types":[],"signal_types":["grant"]}')) {
    response <- httr2::response(200L, headers = list(`content-type` = "application/json"),
                                body = charToRaw(body))
    expect_error(connect(), class = "researchR_protocol_error")
  }
  response <- httr2::response(200L, headers = list(`content-type` = "application/json"),
    body = charToRaw('{"api_version":1,"study_types":["other"],"signal_types":["grant"]}'))
  expect_identical(connect()$meta$study_types, "other")
})

test_that("encoded credentials are redacted without losing the error class", {
  key <- 'synthetic-"quoted"-\\path /+?&=value'
  token <- "synthetic-bearer/?secret"
  response <- json_response(422L, list(list(
    loc = list("body"), msg = paste("Rejected source key", key))))
  failure <- NULL
  local_mocked_bindings(req_perform = function(...) {
    if (!is.null(failure)) stop(failure)
    response
  }, .package = "httr2")
  conn <- rr_connect("https://feeds.example.org", token, check = FALSE)
  save_question <- function() rr_upsert_question(conn, "q", "Q",
    rr_concept("drug", "test"), api_keys = list(pubmed = key))
  json_key <- as.character(jsonlite::toJSON(key, auto_unbox = TRUE))
  json_key <- substr(json_key, 2L, nchar(json_key) - 1L)
  err <- tryCatch(save_question(), error = identity)
  expect_s3_class(err, "researchR_http_error")
  expect_match(conditionMessage(err), "Rejected source key")
  expect_false(grepl(json_key, conditionMessage(err), fixed = TRUE))
  expect_match(conditionMessage(err), "REDACTED")

  encoded <- utils::URLencode(key, reserved = TRUE, repeated = TRUE)
  lower_hex <- encoded
  matches <- gregexpr("%[0-9A-Fa-f]{2}", lower_hex)
  regmatches(lower_hex, matches) <- lapply(regmatches(lower_hex, matches), tolower)
  form_encoded <- gsub("%20", "+", lower_hex, fixed = TRUE)
  encoded_token <- utils::URLencode(token, reserved = TRUE, repeated = TRUE)
  failure <- paste("Transport rejected", encoded, form_encoded, encoded_token)
  err <- tryCatch(save_question(), error = identity)
  expect_s3_class(err, "researchR_unreachable")
  expect_match(conditionMessage(err), "Transport rejected")
  expect_false(any(vapply(c(encoded, form_encoded, encoded_token), function(secret)
    grepl(secret, conditionMessage(err), fixed = TRUE), logical(1))))
})
