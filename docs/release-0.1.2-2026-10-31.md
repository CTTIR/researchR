# researchR 0.1.2 release audit

Re-audit baseline: clean checkout `a3e6e1f` on 31 October 2026. The preceding
0.1.1 revision passed the six-job hosted R check matrix; that result does not
verify the new changes. This release preserves public function signatures,
API v1, environment names and data-column schemas.

## Findings and release scope

| Severity | Confirmed finding and location | User impact and repair |
|---|---|---|
| High | `R/request.R` displayed complete FastAPI validation details. A real baseline-server 422 response contained submitted `api_keys` in `input`. | Invalid configuration could expose source keys in console output or issue reports. Remove echoed input/context and redact submitted keys and bearer tokens, including plain-text, JSON-escaped and URL-encoded errors from older servers. Server-side sanitization complements this client protection. |
| Medium | `R/questions.R` and `R/ops.R` preserved R names on fields defined as JSON arrays. | Named concepts, recipients, delivery channels and scrape targets were rejected with HTTP 422. Strip only array names, preserving values/order and object-valued source/key maps. |
| Medium | `rr_connect(check = TRUE)` accepted unrelated JSON as metadata; missing health collections and malformed structured fields looked empty. | A proxy response or damaged metadata could appear successful and lose information silently. Raise `researchR_protocol_error`; distinguish absent optional metadata from invalid JSON. |
| Medium | Mutating help examples ran whenever `RESEARCHR_URL` was set. | A package check or ordinary example call could change a configured server, including deleting a question. Mark the three write examples `\dontrun`; the vignette already disables its network/write chunks. |
| Low | `R/utils.R` accepted infinite `Date`/`POSIXct` query bounds. | Nonsensical date strings reached the server and could look like valid empty searches. Reject non-finite bounds before requests. |

The release strategy is to repair those existing contracts without adding new
adapters or configuration abstractions. Acceptance requires focused regression
tests, real HTTP compatibility with the preceding server and the updated paired
server, a built source archive, clean package check, and an installation/first-use
smoke from that archive. Invalid metadata now raises an error intentionally;
callers that previously accepted silently empty metadata must repair the server
data rather than treating corruption as absence.

Offset pagination still cannot provide a frozen database snapshot. Upstream
coverage, classification validity, delivery and server deployment remain server
responsibilities. Cursor pagination, broader minimum-R testing and new source
adapters are deferred; they are not needed for these patch repairs.

## Verification evidence

Permanent evidence: `.repository-audit/2026-10-31-research-release/` in the
local GitHub workspace; detailed client logs are under `logs/client/`. Fixtures
contain synthetic credentials and records. No upstream literature retrieval,
real email or external-provider writes are part of these checks.

| Check | Result |
|---|---|
| Baseline offline suite | Passed; real-server tests skipped explicitly. `baseline-unit.log`. |
| Baseline server validation reproduction | Confirmed HTTP 422 contains the submitted synthetic source key in its response. `server-validation-reproduction.log`. |
| Refined offline regressions | Passed, including metadata failures, error condition preservation, redaction and finite dates. `refined-unit.log`. |
| Independent redaction review | Confirmed escaped/encoded secrets survived the original raw-value redactor; repaired JSON, percent-encoding and form-encoding variants. All 26 focused HTTP error checks passed, retaining HTTP and transport condition classes. `encoded-secret-reproduction.log`, `encoded-secret-tests.log`. |
| Real HTTP against preceding server | Passed enhanced suite, including named-array round trips and credential-safe validation failures. `baseline-server-integration.log`. |
| Final real HTTP paired-server integration | Passed the complete client suite against the refined researchD candidate. `paired-server-integration.log`. |
| Source package build and install | Passed with rebuilt vignette; installed archive loads successfully. `build-final.log`, `install-final.log`. |
| Full `R CMD check` | **Status: OK**, including tests, examples, rebuilt vignette and PDF manual on macOS ARM64 / R 4.6.1. `check-package.log`. |
| Independent archive review | Version, final redaction code, documentation and all 15 exports verified; no detected secret patterns, private runtime state or unsafe archive paths. |

Roxygen regenerated the changed help with installed 8.0.0; the package records
8.1.0 from its earlier documentation build. The version warning was observed;
generated differences were reviewed, retaining the existing package-logo layout.

Final archive SHA-256:
`b249540d85fb7c6ec72f905536adfbde0350e7aa627fb1103cba41f5393e2c96`.
The release notes link the exact hosted workflow result; local checks alone
are not a CRAN submission or confirmation of a production server.

## First use

Prerequisites: R 4.1 or later, the package dependencies, and an API v1 researchD
server. Installation from the prepared source archive resolves CRAN dependencies:

```r
install.packages("remotes")
remotes::install_local("/path/to/researchR_0.1.2.tar.gz", upgrade = "never")
library(researchR)
conn <- rr_connect("http://127.0.0.1:8000", token = Sys.getenv("RESEARCHR_TOKEN"))
rr_health(conn)
rr_questions(conn)
rr_source_health(conn)
# After the server has collected records for an existing question:
rr_records(conn, "your_question_id")
```

Set the token privately to the server's `API_TOKEN`; do not paste it into source
files. The server README provides installation, 2FA, question setup and delivery
instructions. `rr_connect(check = FALSE)` constructs an offline client but does
not verify access. Feeds remain public and item-limited.

Release publication and hosted CI for this new revision are separate from local
checks. Their observed outcomes belong in the final publication record.
