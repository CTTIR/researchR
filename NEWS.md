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
