# Lift servlet provider architecture (LLM copy)

LLM-oriented parallel of `docs/architecture/servlet-provider-architecture.md`.

## Structure

1. Abstraction: `net.liftweb.http.provider.HTTPProvider` (trait) —
   `context`, `bootLift(loader: Box[String])` (runs the `bootloader` init
   parameter class or `DefaultBootstrap`, then creates `LiftServlet` and calls
   `init`), `service(req, resp)(chain)` (loans, `isLiftRequest_?`,
   dispatch to `LiftServlet.service` or chain), `terminate`.
2. Servlet adapters (`net.liftweb.http.provider.servlet`):
   - `ServletFilterProvider` — `Filter` with `HTTPProvider`; `init` captures
     `ServletContext` (`HTTPServletContext`) and boots; `doFilter` wraps
     `TransientRequestVarHandler`/`RequestVarHandler`/`LiftRules.allAround`
     loan wrappers and calls `service`.
   - `HTTPRequestServlet` — wraps `HttpServletRequest`; lazy cookies (touches
     session first to capture the session cookie), headers, params,
     `multipartContent_?` (commons-fileupload `ServletFileUpload`), and the
     suspend/resume delegation.
   - `HTTPResponseServlet` — status recorded locally (`getStatus`), headers
     with set-vs-add semantics from `LiftRules.overwrittenReponseHeaders`,
     cookies via `CookieEncoder`.
   - `HTTPServletSession` — delegation + `SessionToServletBridge` (binds the
     Lift session id into the HTTP session; `valueUnbound` removes it).
3. Async seam: `ServletAsyncProvider` trait — `suspend(timeout): RetryState`
   (SUSPENDED/TIMED_OUT/RESUMED), `resume((Req, LiftResponse))`, `resumeInfo`,
   `suspendResumeSupport_?`. Selection at boot in `LiftRules`:
   `asyncProviderMeta` = first `AsyncProviderMeta` in the list with
   class-level support; `theServletAsyncProvider` vends a provider per
   request. Standard implementation: `Servlet30AsyncProvider` (`startAsync`,
   `setTimeout(0)`, resume → `LiftServlet.sendResponse` + `complete`;
   re-dispatch recognized via `resumeInfo` in `LiftServlet.service`).
3b. Selection list: `LiftRules.asyncProviderMetaList` (read-only accessor)
   holds `Servlet30AsyncProvider` only; extended by `addSyncProvider` before
   boot. Per-request support: `HTTPRequestServlet.suspendResumeSupport_?`
   (meta support AND this request's `isAsyncSupported`).
4. Async entry: `LiftServlet.handleGenericContinuation` (catches
   `ContinuationException` thrown by `RestContinuation.async`): schedules the
   reply body on a scheduler thread (5 ms), then either suspends (async) or
   waits on an `LAFuture` bounded by `cometRequestTimeout`
   (`LiftRules.cometRequestTimeout`, default 120 s, multiplied by 1000 → ms).
   Reply body runs detached from request state; response rendering is wrapped
   in `wrapState(req, session)`.
5. Test harness: `net.liftweb.webapptest.JettyTestServer` (embedded Jetty 9,
   Jetty 11, jakarta) deploys `src/test/webapp` (`web.xml` declares `LiftFilter`); boot in
   `bootstrap.liftweb.Boot` registers `ContainerVarTests`, `LongPollingTests`
   (`/lp/echo`, `/lp/delay`, `/lp/state` — RestContinuation-based, records
   request/reply thread names), `UploadEchoTests` (`/lp/upload` — echoes
   field name, filename, mime, size, SHA-256).

## Proven behavior

`docs/servlet-behavior-test-map.md` maps each claim to its named test.
