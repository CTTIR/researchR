# researchR <picture class="cttir-readme-logo"><source class="cttir-logo-dark-source" media="(prefers-color-scheme: dark)" srcset="man/figures/logo-with-name-dark.png" /><img src="man/figures/logo-with-name.png" align="right" alt="researchR logo" width="120" height="139" /></picture>

An R client for [**researchD**](https://github.com/CTTIR/researchD), the
self-hosted literature and signal surveillance server it is built for.

`researchD` collects from configured literature sources on a schedule,
deduplicates and classifies hits by study type, and tracks a parallel stream of
non-publication *signals* — trial registrations and results postings,
regulatory decisions, grant awards, SEC filings, patents, press and
investor-relations items. It serves the results as open RSS feeds and manages
them through a browser dashboard.

Coverage depends on enabled adapters, upstream access and the collection
history. Consult the server's [source support and operating instructions](https://github.com/CTTIR/researchD)
before relying on a particular source or signal type. Study-type labels are
screening aids and require review.

This package is the other way in. It talks to the server's REST API so you can
drive the whole thing from R: define the surveillance questions, then pull
what they found straight into tibbles.

```
┌─────────────┐   HTTP + bearer token   ┌──────────────────────────────┐
│  researchR  │ ──────────────────────► │  researchD                   │
│  (this pkg) │ ◄────── tibbles ─────── │  R engine · FastAPI · cron   │
└─────────────┘                         └──────────────────────────────┘
                                              │
                                    open RSS  ▼  →  Zotero, any reader
```

## Installation

```r
# install.packages("remotes")
remotes::install_github("CTTIR/researchR")
```

You also need access to a running `researchD` server, which is distributed
separately under the [MIT license](https://github.com/CTTIR/researchD/blob/main/LICENSE).
The client requires R 4.1 or later. Install from a local source checkout with
`remotes::install_local("/path/to/researchR", build_vignettes = TRUE)`.

## Authentication

`researchD` has two deliberately separate front doors:

| Surface | Who it is for | How it authenticates |
|---|---|---|
| Dashboard (`/`) | you, in a browser | password **+ TOTP second factor** |
| `/api/v1` | this package | bearer token |
| Feeds (`/feed/…`) | Zotero, readers | open, no auth |

A dashboard session is **not** accepted as an API token, and a token will not
open the dashboard. Set the token once on the server as `API_TOKEN` in its
`.env`, and once locally in `~/.Renviron`:

```
RESEARCHR_URL=https://feeds.example.org
RESEARCHR_TOKEN=<the same value as API_TOKEN>
```

Keeping it in `~/.Renviron` rather than in a script is the point — the token is
equivalent to write access to your questions. If the server has no `API_TOKEN`
set, the API surface answers 503 rather than serving unauthenticated, and this
package reports that as a `researchR_disabled` condition.

## Usage

```r
library(researchR)

conn <- rr_connect()          # reads the environment and checks the API token
conn
#> ── researchD connection ──
#> • server: https://feeds.example.org
#> • token: set (64 characters, not shown)
#> • api version: 1
```

For the first connection, use `rr_health(conn)` to check web liveness, then
`rr_heartbeat(conn)` and `rr_source_health(conn)` to inspect collection status.
`rr_connect(check = FALSE)` constructs a connection offline; it does not check
the URL or token against a running server. A successful API connection alone
does not establish that collection or delivery has run.

### Define what to watch

A question is a list of **concepts**, AND-joined at query time; the terms
within one concept are OR-joined. So this asks for
`(semaglutide OR tirzepatide OR "GLP-1") AND (MASH OR NASH OR steatohepatitis)`:

```r
rr_upsert_question(
  conn, "glp1_mash", "GLP-1 receptor agonists in MASH",
  concepts = list(
    rr_concept("drug",    c("semaglutide", "tirzepatide", "GLP-1")),
    rr_concept("disease", c("MASH", "NASH", "steatohepatitis"))),
  sources        = list(pubmed = TRUE, europepmc = TRUE, biorxiv = TRUE),
  from_days_back = 14,
  email_enabled  = TRUE,
  email_cron     = "0 7 * * 1",       # this question's own cadence
  email_to       = "me@example.org")
```

### Pull what it found

```r
rr_records(conn, "glp1_mash", study_type = "rct", since = Sys.Date() - 90)
#> # A tibble: 23 × 18
#>    dedup_key   source  doi        title              study_type is_preprint score
#>    <chr>       <chr>   <chr>      <chr>              <chr>            <int> <dbl>
#>  1 doi:10.10…  pubmed  10.1056/…  Semaglutide in …   rct                  0  4.81
#>  …
```

Reading never triggers a fetch — that is the server's job on its own cron. You
are querying what it has already stored.

### The signals track

```r
rr_signals(conn, "glp1_mash", min_confidence = "reported")
```

Signals carry an explicit confidence tier — `authoritative` (registries,
regulators, SEC), `reported` (official but preliminary), `unverified` (scraped
press or IR: a lead, not a fact). `min_confidence` is a **floor**, so
`"reported"` returns reported *and* authoritative.

### Recall versus precision

Scoring orders results without discarding below-threshold records. A question in `digest_mode = "threshold"`
shows only records above `min_score` in its curated feed and its digest email,
but the full set stays queryable:

```r
rr_records(conn, "glp1_mash")                          # everything stored
rr_records(conn, "glp1_mash", apply_threshold = TRUE)  # what the digest shows
```

The API returns the records currently retained by the server. Configured
retention can remove old records. RSS feeds contain only the server's most
recent `FEED_LIMIT` items, including `all.xml`; use `rr_records()` for a paginated
read of the retained set. If that set changes during pagination, the client
reports `researchR_pagination_changed` rather than silently repeating records.
Retry after the collection run has finished. Offset pagination does not provide
a transactionally frozen snapshot.

Empty responses retain their column types. Times are UTC `POSIXct`, dates are
`Date`, and `provenance` is a list column describing source contributions when
the server provides them. Older servers return an empty provenance list per row.

### Watch for silently dead sources

The worst failure mode of a surveillance system is a source that quietly stops
returning anything, because the symptom — fewer results — looks exactly like a
quiet week in the literature.

```r
rr_source_health(conn)
```

`last_ok` says the fetcher ran; `last_nonempty` says it actually found
something. Both matter.

### Hand a feed to Zotero

Feeds are open, so the URL goes straight into Zotero
(My Library → right-click → New Feed → From URL):

```r
rr_feed_url(conn, "glp1_mash")                      # the curated stream
rr_feed_url(conn, "glp1_mash", all = TRUE)          # everything stored
rr_feed_url(conn, "glp1_mash", study_type = "rct")
rr_feed_url(conn, "glp1_mash", signals = TRUE)
```

## Function reference

| | |
|---|---|
| `rr_connect()`, `rr_health()`, `rr_meta()` | connect; check the server; read its vocabulary |
| `rr_questions()`, `rr_question()` | list and fetch questions |
| `rr_concept()`, `rr_upsert_question()`, `rr_delete_question()` | define and edit them |
| `rr_records()`, `rr_signals()` | pull the two tracks |
| `rr_source_health()` | per-source fetch heartbeat |
| `rr_signal_config()`, `rr_set_signal_config()` | signals-track settings |
| `rr_feed_url()` | build feed URLs |

## Testing

The package's unit tests run offline:

```r
pkgload::load_all(".")
testthat::test_dir("tests/testthat", package = "researchR", load_package = "none")
```

The integration suite needs a live server and is skipped without one. Point it
at a disposable instance:

```r
Sys.setenv(RESEARCHR_TEST_URL = "http://127.0.0.1:8000",
           RESEARCHR_TEST_TOKEN = "…")
```

With adjacent server and client checkouts, `researchD/dev/tests/integration_client.sh`
starts a disposable server, seeds synthetic records, exercises both repositories,
and stops the server. It requires the server's test dependencies and uses an
isolated temporary directory. The client integration tests create and delete questions;
use a disposable instance, never a production database.

The [maintenance record](docs/maintenance-2026-10-30.md) lists the audit findings,
acceptance criteria, verification results and remaining release work.

## License

MIT © Raban Heller

## Documentation and support

The [getting-started vignette](https://github.com/CTTIR/researchR/blob/main/vignettes/getting-started.Rmd) demonstrates offline
connection setup, concept construction and feed URLs, and separates those steps
from requests that need a live server. To install the rendered vignette, use
`remotes::install_github("CTTIR/researchR", build_vignettes = TRUE)`, then
`vignette("getting-started", package = "researchR")`.

Use `help(package = "researchR")` for the API and `citation("researchR")` for
citation details. Report reproducible issues with synthetic inputs at
<https://github.com/CTTIR/researchR/issues>.

## Use of LLM tools

Where applicable, LLM tools were used for narrowly defined, non-authorial tasks:
copyediting, prose refinement, Markdown/LaTeX formatting, boilerplate scaffolding
(such as CI configurations and build scripts), and code refactoring. Tools included
[Chat AI](https://kisski.gwdg.de/leistungen/2-02-llm-service/), the KISSKI
(GWDG) service, and self-hosted **Mistral Small (24B, Apache-2.0)** served through
a local **vLLM architecture** on an Ubuntu Linux workstation with Intel
processors and NVIDIA GPUs. The self-hosted model ran locally, with no data sent
to third parties.

All scientific claims, methodological choices, analyses, interpretations and
conclusions remain the authors' own. Where applicable, LLM-generated material
must be reviewed and revised before inclusion, and references must be verified
against their DOI, arXiv ID or ISBN.

Local software setup: vLLM with Python/PyTorch, plus R and RStudio.

## Branding

Editable R artwork and regeneration instructions are in [tools/branding](https://github.com/CTTIR/researchR/blob/main/tools/branding/README.md).
