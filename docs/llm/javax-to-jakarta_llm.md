# Migrating Lift from javax.servlet to jakarta.servlet (LLM copy)

LLM-oriented parallel of `docs/migration/javax-to-jakarta.adoc`.

- Target: Jakarta Servlet 5.0 (Jakarta EE 9). Lift compiles against
  `jakarta.servlet:jakarta.servlet-api:5.0.0` (provided) and is tested on
  embedded Jetty 12 (ee9 environment).
- Containers: Jetty 12 (ee9 environment for Servlet 5.0), Tomcat 10.0+.
  The javax line (Jetty 9, Tomcat 9 and earlier) cannot run the jakarta Lift
  line, and the namespaces cannot be mixed in one application.
- Application code against Lift's APIs is unchanged; the migration lives in
  Lift's servlet adapter layer.
- `web.xml` uses the Jakarta EE namespace, version 5.0 schema, Lift filter
  with `async-supported` true (full example in the human page).
- Uploads go through the container's `getParts()`; commons-fileupload is
  removed. Applications must declare multipart configuration on the servlet
  owning the request (`<multipart-config/>` or
  `registration.setMultipartConfig(...)`); a filter carries none, and
  without configuration the container rejects part parsing
  (`IllegalStateException: No multipart config for servlet`). Lift's own
  limits (`LiftRules.maxMimeSize`, `maxMimeFileSize`) are enforced in the
  ingestion path — an upload beyond them fails with the framework error
  (`UploadIngestionSpec."reject an upload beyond LiftRules.maxMimeSize"`).
  Upload byte-identity is pinned by
  `UploadIngestionSpec."ingest file content byte-identically with correct
  metadata"` (plus empty-file and non-ASCII-filename examples).
- The async provider list registers the standard Servlet 3.0 provider only
  (`AsyncProviderSelectionSpec."register only the standard servlet 3.0
  provider"`).
- All modules migrate in lockstep as one release; framework features and
  behavior are unchanged (all 14 behavior-map claims hold across the
  migration — see `docs/servlet-behavior-test-map.md`).
