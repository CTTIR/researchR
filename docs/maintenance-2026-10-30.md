# Client maintenance, 30 October 2026

Scope: `researchR`, maintained with the directly connected `CTTIR/researchd`
server. Baseline: `3e1abfeca85dd5d7390a0f19916df9ba73ea3a57` (0.1.0).
The local checkout matched GitHub and had no uncommitted work at audit start.
Version 0.1.1 preserves the public function signatures and API v1 compatibility.

## Purpose and acceptance criteria

The client lets an R user configure surveillance questions and inspect the
server's retained literature, signals and health. It must keep credentials out
of displayed connection objects, preserve result types and timestamps, handle
empty data, surface failures clearly, and remain usable with API v1 servers
that lack new optional fields. It does not perform collection or delivery.

Release acceptance requires a clean package build and check, regression tests
for the defects below, and an actual HTTP integration run against a disposable
server. Passing those checks establishes client behavior under the tested
environment, not the availability or completeness of upstream literature APIs.

## Findings and repairs

| Priority | Confirmed finding | User impact | Repair and verification |
|---|---|---|---|
| Essential | `R/questions.R` parsed ISO timestamps with the default `as.POSIXct()` parser. | `2026-10-30T13:14:15+01:00` became midnight, losing the time and offset. | Explicit ISO/SQLite parsing into UTC, including fractions and offsets; regression tests exercise the public question reader. |
| Essential | Empty questions had nine character columns instead of the populated schema; empty health flags/counters were characters. | New installations and queries with no data broke downstream typed code. | One construction path for questions and shared type conversion for health views; tests cover zero rows in all tabular endpoints. |
| Essential | `R/request.R` passed structured HTTP error text directly into CLI formatting. | FastAPI 422 JSON caused a formatting exception rather than the documented HTTP condition. | Treat server details as text, including arbitrary braces; tests cover structured and plain error bodies. |
| Everyday use | Pagination silently stopped if the server omitted totals, and invalid limits produced confusing failures. | Incomplete or duplicated reads could appear successful. | Validate limits and pagination responses; stop on changed totals or repeated record IDs. Stable ordering still depends on the server, and offset pagination is not a snapshot. |
| Everyday use | URL interpolation did not encode question IDs; incompatible feed flags produced nonexistent routes. | Legacy IDs with spaces or reserved punctuation broke requests; signal/preprint combinations returned 404. | Encode route components, reject slash/dot route segments and invalid variants, and test compatibility with existing ordinary IDs. |
| Everyday use | Every HTTP 503 was described as a disabled API and liveness ignored the HTTP status. | Temporary outages elicited incorrect token advice; an error response containing `ok` looked healthy. | Separate disabled and temporarily unavailable conditions; require HTTP 200 for liveness. |
| Everyday use | Scalars, logical filters, dates and question definitions were insufficiently checked. | Missing/invalid values could become unintended filters, malformed requests or coercion warnings. | Validate before network calls. New/upsert IDs use the server's documented 128-character slug grammar. Read paths retain representable legacy IDs. |
| Core enhancement | New server provenance was not represented as structured client data. | Source contributions were awkward to inspect and absent on empty responses. | A `provenance` list column parses the optional JSON field and supplies empty lists for older servers. |

## Roadmap and tradeoffs

1. **Essential repairs:** exact timestamps, complete typed schemas and reliable
   error translation. Acceptance: public-reader and HTTP regression tests pass.
2. **Dependable everyday use:** validated input, safe feed/path construction and
   checked pagination, with corresponding help and first-use documentation.
   Acceptance: unit tests, package build and check, then live local integration.
3. **Connected release:** verify this client against the accompanying server
   repairs and include both in CI. Acceptance: server/client tests exercise
   creation, updates, deletion, filters, paging, authentication and health over
   HTTP using synthetic state. Root release evidence records this run.
4. **Deferred:** server-backed cursor/snapshot pagination, large-scale retrieval
   benchmarks and additional source adapters. Adding client abstractions cannot
   resolve those server contracts; they belong in a separately validated release.

This patch keeps existing function signatures, accepts the prior API fields,
and adds one optional result column. Rejecting malformed inputs is intentional.
Question writes are full replacements as before; users should supply the full
intended question definition, not assume patch semantics.

## Verification

Baseline: the unmodified 0.1.0 package built and passed `R CMD check
--no-manual`; its synthetic HTTP integration suite passed. That did not cover
the newly reproduced defects.

Executed on macOS ARM64 with R 4.6.1, testthat 3.3.2 and httr2 1.3.0:

| Check | Observed result |
|---|---|
| Offline regression tests in the final package check | 106 checks passed; no failures or warnings. The 12 server-dependent tests explicitly skip without a configured server. |
| `R CMD build /path/to/researchR` | Passed; produced `researchR_0.1.1.tar.gz`, including the built vignette. |
| `R CMD check --no-manual researchR_0.1.1.tar.gz` | `Status: OK`; no errors, warnings or notes. |
| Actual HTTP integration against baseline researchd `ac71e59a1fa6484c2a82d0b32165af4956b6eaf3` | Passed using Python 3.15.0, a disposable database and synthetic credentials. Exercises backward compatibility with a server lacking provenance. |
| Actual HTTP integration against the revised paired server and final isolated harness | Passed, including the server-dependent suite, with an automatically selected local port and a disposable temporary database. Evidence: `/private/tmp/research-pair-audit-20261030/client-harness-final.log`. |
| Install final tarball into a fresh library and exercise first use | Passed: package 0.1.1 loaded from that library; offline connection, feed URL, concept construction, token-safe printing, installed help and built vignette were checked. This offline smoke does not assert live connectivity. |
| Final archive contents | README matches the final checkout, including the isolated-harness instructions; regression tests and built vignette are included. No local environment, credentials, development state or maintenance records are packaged. |
| `git diff --check` | Passed. |

The final package check includes the subsequent URL-percent and numeric-query
regressions and the final README update. Evidence is retained under
`/private/tmp/research-pair-audit-20261030/`: `client-final-release-build.log`,
`client-release-check.log`, `client-final-installed/` and
`client-final-package-manifest.json`. The checked `researchR_0.1.1.tar.gz`
has SHA-256 `8f24f863423db861d2c1cfef8b24d27cef5c28e671a1e86b2e0720042b8ccaaf`.
New remote CI, other operating systems, CRAN
submission and production deployment remain unverified until actually run.
No remote release, submission or production operation was performed here.

## Runnable first use

Prerequisites: R >= 4.1, a running API v1 `researchd` server, its base URL and
API token. The server is installed and operated separately; consult its README
for Docker prerequisites, setup, collection, recovery and source support.

```r
install.packages("remotes")
remotes::install_local("/path/to/researchR", build_vignettes = TRUE)
# Put RESEARCHR_URL and RESEARCHR_TOKEN in your private ~/.Renviron, then restart R.
library(researchR)
conn <- rr_connect()
rr_health(conn)
rr_heartbeat(conn)
rr_source_health(conn)
rr_upsert_question(conn, "exercise_fatigue", "Exercise and fatigue",
  concepts = list(rr_concept("exposure", c("exercise", "physical activity")),
                  rr_concept("outcome", c("fatigue", "tiredness"))),
  sources = list(pubmed = TRUE, europepmc = TRUE), email_enabled = FALSE)
# After the server has completed collection:
rr_records(conn, "exercise_fatigue")
rr_feed_url(conn, "exercise_fatigue")
```

Feeds are public and limited to recent items. The API returns retained records;
an empty response does not prove an absence of relevant research. Keep real
credentials out of scripts, test fixtures, screenshots and issue reports.

## Server naming update

The server repository and local checkout were renamed from `researchd` to
`researchD` on 30 October 2026. Current client links, checkout instructions
and displayed server names use `CTTIR/researchD`. API paths and environment
variable names are unchanged; historical evidence above retains its original
paths and repository names.
