# Upgrading from javax Lift (LLM copy)

LLM-oriented parallel of `docs/upgrading-from-javax-lift.md` — the
troubleshooting recipe for the javax-to-jakarta upgrade.

- `NoClassDefFoundError: javax/servlet/...` at boot: a javax-era Lift jar or
  addon is on the classpath of a jakarta application. Every Lift module and
  addon must come from the same line.
- `NoClassDefFoundError: jakarta/servlet/...`: the jakarta Lift line runs in
  a javax container. Move to Jetty 11 (or Jetty 12 EE 9+), Tomcat 10.0+.
- `IllegalStateException: No multipart config for servlet` on upload: add
  `<multipart-config/>` to the `<servlet>` (or
  `registration.setMultipartConfig(...)`); `getParts()` requires it,
  commons-fileupload did not.
- `web.xml` parse errors: the descriptor must use the Jakarta EE namespace
  and version 5.0 schema.
- Detecting a jar's line: its pom carries `javax.servlet:javax.servlet-api`
  (javax) or `jakarta.servlet:jakarta.servlet-api` (jakarta); in bytecode,
  `javax/servlet/...` references under
  `net/liftweb/http/provider/servlet` mean the javax line.
- One line per application classloader; upgrade all Lift modules in
  lockstep.
