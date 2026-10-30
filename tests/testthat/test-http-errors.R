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
