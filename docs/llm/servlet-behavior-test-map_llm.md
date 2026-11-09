# Lift servlet behavior: claims and the tests that prove them (LLM copy)

LLM-oriented parallel of `docs/servlet-behavior-test-map.md`. Each row:
behavior statement, then the named specs2 example (cited as
`SpecName."example text"`) that proves it.

1. Long-polling reply body runs off the request thread; client receives the
   reply — `CometLongPollingSpec."run the reply off the request thread and
   deliver it to the client"`.
2. Long-polling reply body runs detached from request state (the reply body
   observes an empty session; the framework's request/session wrapper applies
   to the response rendering, not the reply body) —
   `CometLongPollingSpec."run the reply detached from request state"`.
3. An answer computed after the request thread has returned is still
   delivered to the waiting client — `CometLongPollingSpec."deliver an
   answer computed after the request thread has returned"`.
4. Lift selects an async provider reporting suspend/resume support; the
   provider function vends the standard Servlet 3.0 provider —
   `AsyncProviderSelectionSpec."select the standard servlet 3.0 provider"`
   and `AsyncProviderSelectionSpec."vend a standard servlet 3.0 provider
   from the selected meta"`.
5. Session attribute round-trip through `HTTPServletSession` for arbitrary
   names and values (Scalacheck property) — `HTTPServletSessionSpec."round-
   trip arbitrary attribute names and values through both views"`.
6. Session identity and invalidation delegated to the container session —
   `HTTPServletSessionSpec."delegate identity and invalidation to the
   container session"`.
7. Filter `init` boots Lift; a later request is served —
   `ServletFilterProviderSpec."boot lift and serve a subsequent request"`.
8. Request facts through `HTTPRequestServlet` match the raw servlet request
   (Scalacheck property over header sets; plain-request facts; duplicate
   cookie names) — `HTTPRequestServletSpec."match the raw servlet request
   for arbitrary header sets"`, `HTTPRequestServletSpec."for a plain
   request"`, `HTTPRequestServletSpec."expose cookies including a duplicate
   cookie name"`.
9. Multipart upload ingested with byte-identical content and correct field
   metadata (Scalacheck property over file contents; empty file; non-ASCII
   filename) — `UploadIngestionSpec."ingest file content byte-identically
   with correct metadata"`, `UploadIngestionSpec."ingest an empty file with
   size zero"`, `UploadIngestionSpec."ingest a file whose filename carries
   non-ASCII characters"`.
10. Status, headers, body written through `HTTPResponseServlet` are recorded
    as expected (Scalacheck property over status/header/body triples;
    non-ASCII header value; empty body; set-vs-add semantics for
    `overwrittenReponseHeaders`) — `HTTPResponseServletSpec."record status,
    headers, and body for arbitrary triples"`, `HTTPResponseServletSpec."
    record a header value with non-ASCII characters as written"`,
    `HTTPResponseServletSpec."record a status with an empty body"`,
    `HTTPResponseServletSpec."to it"`.
11. The provider list registers only the standard Servlet 3.0 provider —
    `AsyncProviderSelectionSpec."register only the standard servlet 3.0
    provider"`.
12. Per-request async support is a per-request property —
    `AsyncProviderFailsafeSpec."report no per-request support when the
    container request disallows async"`.
13. A budget-exceeded long-polling answer is still delivered on an
    async-capable container; the client waits for it —
    `AsyncProviderFailsafeSpec."deliver a budget-exceeded answer on an
    async-capable container"`.
14. The end-to-end harness runs HtmlUnit directly, with jwebunit absent
    from the test classpath — `WebappTestHarnessSpec."carry the htmlunit
    client"` and `WebappTestHarnessSpec."carry no jwebunit classes on the
    classpath"`.
15. Lift's own upload limits are enforced in the ingestion path: beyond
    `LiftRules.maxMimeSize` the request fails (framework error); within the
    limit the upload is ingested byte-identically —
    `UploadIngestionSpec."reject an upload beyond LiftRules.maxMimeSize"`
    and `UploadIngestionSpec."ingest an upload at exactly
    LiftRules.maxMimeSize"`.
