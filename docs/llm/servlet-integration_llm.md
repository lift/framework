# Lift servlet integration (LLM copy)

LLM-oriented parallel of `docs/servlet-integration.md`. Read that page for the
human version; this file states the same facts compactly for code-reading
models.

## Facts

1. Lift runs as a servlet filter (`net.liftweb.http.LiftFilter`). Boot happens
   in `HTTPProvider.bootLift` during filter `init` (`ServletFilterProvider`
   captures the `ServletContext` there and hands it to `LiftRules.setContext`).
2. Container types are wrapped into container-neutral types:
   `HTTPRequestServlet` wraps `HttpServletRequest`, `HTTPResponseServlet`
   wraps `HttpServletResponse`, `HTTPServletSession` wraps `HttpSession`,
   `HTTPServletContext` wraps `ServletContext`. All are constructed per request
   by `ServletFilterProvider.doFilter`, which then calls
   `HTTPProvider.service`; requests Lift handles are dispatched to
   `LiftServlet.service`, other requests continue down the filter chain.
3. Long polling: `RestContinuation.async(f)` throws `ContinuationException`;
   `LiftServlet.handleGenericContinuation` catches it. Two mechanisms deliver
   the reply:
   - async path (`req.request.suspendResumeSupport_?` true): calls
     `HTTPRequestServlet.suspend(cometTimeout)` which delegates to the
     `ServletAsyncProvider` selected by `LiftRules.theServletAsyncProvider`
     (`Servlet30AsyncProvider`: `startAsync`, `setTimeout(0)`, on `resume`
     writes via `LiftServlet.sendResponse` then `AsyncContext.complete`; the
     re-dispatched request is recognized by `resumeInfo`).
   - fallback path: an `LAFuture` satisfied by the scheduled work, waited on
     for `cometTimeout` (`LiftRules.cometRequestTimeout` seconds, default 120).
   Both paths schedule `f` on a separate thread (5 ms delay) with the request
   state wrapped (`wrapState`).
4. Provider selection: `LiftRules.asyncProviderMeta` picks the first registered
   `AsyncProviderMeta` whose `suspendResumeSupport_?` is true; per request,
   `HTTPRequestServlet.suspendResumeSupport_?` additionally requires the
   container to report asynchronous support for that request.

## Tests that prove these facts

`docs/servlet-behavior-test-map.md` (and its table) names the proving tests;
the same table is in `../servlet-behavior-test-map.md` one level up.
