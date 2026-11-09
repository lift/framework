# Lift servlet behavior: claims and the tests that prove them

Each statement below describes what Lift's servlet integration does. The test
named next to each statement proves it; run the named test to check the
statement against the code. Tests are specs2 examples, cited as
`SpecName."example text"`. (Model: `docs/mongodb-docker.md`.)

| # | Behavior statement | Proven by |
| --- | --- | --- |
| 1 | A long-polling request runs its reply body on a thread distinct from the request thread and delivers the reply to the waiting client | `CometLongPollingSpec."run the reply off the request thread and deliver it to the client"` |
| 2 | The reply body runs detached from the request's state (the reply body observes an empty session) | `CometLongPollingSpec."run the reply detached from request state"` |
| 3 | An answer computed after the request thread has returned is still delivered to the waiting client | `CometLongPollingSpec."deliver an answer computed after the request thread has returned"` |
| 4 | Lift selects an asynchronous provider that reports suspend/resume support, and the provider function vends the standard Servlet 3.0 provider | `AsyncProviderSelectionSpec."select the standard servlet 3.0 provider"` and `AsyncProviderSelectionSpec."vend a standard servlet 3.0 provider from the selected meta"` |
| 5 | Session attributes set through `HTTPServletSession` are visible in the underlying container session, and attributes set in the container session are visible through the adapter, for arbitrary names and values | `HTTPServletSessionSpec."round-trip arbitrary attribute names and values through both views"` |
| 6 | `HTTPServletSession` delegates identity and invalidation to the container session | `HTTPServletSessionSpec."delegate identity and invalidation to the container session"` |
| 7 | A Lift filter initialization boots Lift and a subsequent request is served by the running LiftServlet | `ServletFilterProviderSpec."boot lift and serve a subsequent request"` |
| 8 | URI, query string, headers, cookies, remote address, and content type read through `HTTPRequestServlet` match the raw servlet request | `HTTPRequestServletSpec."match the raw servlet request for arbitrary header sets"`, `HTTPRequestServletSpec."for a plain request"`, and `HTTPRequestServletSpec."expose cookies including a duplicate cookie name"` |
| 9 | A multipart form upload is ingested with byte-identical file content and correct field metadata (field name, filename, mime type, size) | `UploadIngestionSpec."ingest file content byte-identically with correct metadata"`, `UploadIngestionSpec."ingest an empty file with size zero"`, and `UploadIngestionSpec."ingest a file whose filename carries non-ASCII characters"` |
| 10 | Status, headers, and body written through `HTTPResponseServlet` are recorded as expected, including a status without a body, a non-ASCII header value, and set-vs-add header semantics | `HTTPResponseServletSpec."record status, headers, and body for arbitrary triples"`, `HTTPResponseServletSpec."record a header value with non-ASCII characters as written"`, and `HTTPResponseServletSpec."to it"` (the overwritten-headers example) |
| 11 | The provider list registers only the standard Servlet 3.0 provider | `AsyncProviderSelectionSpec."register only the standard servlet 3.0 provider"` |
| 12 | Per-request async support is a per-request property: a container request reporting asynchronous processing unsupported makes `HTTPRequestServlet.suspendResumeSupport_?` false | `AsyncProviderFailsafeSpec."report no per-request support when the container request disallows async"` |
| 13 | A budget-exceeded long-polling answer is still delivered on an async-capable container, and the client waits for it | `AsyncProviderFailsafeSpec."deliver a budget-exceeded answer on an async-capable container"` |
| 14 | The end-to-end harness runs HtmlUnit directly, with jwebunit absent from the test classpath | `WebappTestHarnessSpec."carry the htmlunit client"` and `WebappTestHarnessSpec."carry no jwebunit classes on the classpath"` |
| 15 | An upload beyond `LiftRules.maxMimeSize` fails the request with the framework error; an upload within the limit is ingested byte-identically | `UploadIngestionSpec."reject an upload beyond LiftRules.maxMimeSize"` and `UploadIngestionSpec."ingest an upload at exactly LiftRules.maxMimeSize"` |

Boundary conditions exercised by the tests above are listed in each test's
comment block; the property-based tests (5, 8, 9, 10) use Scalacheck to cover
arbitrary inputs rather than single examples.
