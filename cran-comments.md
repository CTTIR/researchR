## Submission

This is a new submission.

researchR is a client for 'researchD', a self-hosted literature and signal
surveillance server (<https://github.com/CTTIR/researchD>, MIT licence). All
functions except the offline helpers `rr_concept()` and `rr_feed_url()` need a
running server and an API token. For that reason:

* examples that read from a server run only when `RESEARCHR_URL` is set
  (`@examplesIf`), so they do not run on CRAN;
* the three examples that create, change or delete server-side configuration
  are wrapped in `\dontrun{}` with a comment explaining why, so that running
  the examples can never modify a user's server;
* tests that need a server are skipped on CRAN and unless a test server is
  configured; all other tests mock the HTTP layer and run offline;
* the vignette runs offline and does not contact any server.

## Test environments

* local: macOS (aarch64), R 4.6.1, `R CMD check --as-cran --no-manual`
  (no LaTeX installation is available locally, so the PDF manual was not
  built there)
* GitHub Actions: macOS-latest (release), windows-latest (release),
  ubuntu-latest (devel, release, oldrel-1), with `--as-cran`

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  New submission
