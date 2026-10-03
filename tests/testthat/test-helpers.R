test_that("query values are rendered the way FastAPI expects", {
  f <- researchR:::rr_query_value
  expect_identical(f(TRUE), "true")
  expect_identical(f(FALSE), "false")
  expect_identical(f(as.Date("2026-08-26")), "2026-08-26")
  expect_identical(f(3L), "3")
  expect_identical(f(1.25), "1.25")
})

test_that("NULL query parameters are dropped, FALSE ones are not", {
  f <- researchR:::rr_drop_null
  expect_named(f(list(a = 1, b = NULL, c = FALSE)), c("a", "c"))
})

test_that("an empty result set still yields a typed zero-row tibble", {
  out <- researchR:::rr_rows_to_tbl(list(), c("doi", "title"))
  expect_s3_class(out, "tbl_df")
  expect_identical(nrow(out), 0L)
  expect_named(out, c("doi", "title"))
})

test_that("rows with missing fields do not shift columns", {
  rows <- list(list(doi = "10.1/a", title = "A"), list(doi = "10.1/b"))
  out <- researchR:::rr_rows_to_tbl(rows, c("doi", "title"))
  expect_identical(out$doi, c("10.1/a", "10.1/b"))
  expect_identical(out$title, c("A", NA_character_))
})

test_that("documented columns get their documented types", {
  out <- researchR:::rr_type_columns(tibble::tibble(
    score = "1.5", is_preprint = "1", pub_date = "2026-08-01",
    first_seen = "2026-08-02 10:00:00"))
  expect_type(out$score, "double")
  expect_type(out$is_preprint, "integer")
  expect_s3_class(out$pub_date, "Date")
  expect_s3_class(out$first_seen, "POSIXct")
})

test_that("JSON columns parse, and malformed JSON degrades to an empty list", {
  out <- researchR:::rr_json_column(c('{"phase":"3"}', "not json", NA))
  expect_identical(out[[1]]$phase, "3")
  expect_identical(out[[2]], list())
  expect_identical(out[[3]], list())
})

test_that("concepts round-trip through the server's shape", {
  cpt <- rr_concept("drug", c("semaglutide", "GLP-1"), mesh = "Liver Cirrhosis")
  expect_s3_class(cpt, "researchr_concept")
  back <- researchR:::rr_as_concept(
    list(name = "drug", terms = list("semaglutide", "GLP-1"),
         mesh = list("Liver Cirrhosis")))
  expect_identical(back$terms, cpt$terms)
  expect_identical(back$mesh, cpt$mesh)
})
