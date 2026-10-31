# researchR 0.1.2

* Keep source API keys and bearer tokens out of displayed request errors,
  including validation responses from older servers that echo request input
  and JSON-escaped or URL-encoded credentials in diagnostics.
* Serialize named concept lists, recipient/delivery vectors and scrape-target
  lists as JSON arrays, preserving their order and values.
* Reject malformed API metadata, missing row collections, invalid structured
  metadata and non-finite date bounds instead of reporting a successful
  connection, empty metadata or a misleading filter result.
* Prevent package checks and ordinary example runs from executing the examples
  that create, delete or reconfigure questions. Clarify feed limits and errors.

# researchR 0.1.1

* Preserve the time, fractional seconds and UTC offset of server timestamps.
  Empty question, health and heartbeat responses now retain their full schema
  and column types.
* Validate connection settings, question definitions, filters and pagination
  before requests; encode legacy question IDs safely in paths and feed URLs.
* Detect inconsistent pagination instead of returning silently incomplete or
  duplicated results. Distinguish temporary HTTP 503 responses from a disabled
  API, and render structured server validation errors without interpreting
  their braces as formatting expressions.
* Return record provenance as a list column while retaining compatibility with
  older servers, and clarify first use, feed limits and collection boundaries.

# researchR 0.1.0

* Standardize the named v5 package logo across README, documentation pages
  and available application headers, with matching light and dark artwork.


* Added an executable offline getting-started vignette covering connection
  configuration, concepts and feed URLs, with explicit boundaries for server
  requests and email delivery.
* Added a grouped reference index using the shared CTTIR documentation theme.
