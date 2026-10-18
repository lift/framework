# Upgrading from javax Lift

Troubleshooting recipe for moving an application from the javax servlet line
of Lift to the jakarta line. The full change list is in
[Migrating Lift from javax.servlet to jakarta.servlet](migration/javax-to-jakarta.adoc).

## Symptom: `NoClassDefFoundError: javax/servlet/...` at boot

A javax-era Lift jar is on the classpath of a jakarta application, or a
third-party addon built against the javax line is. Every Lift module and
every addon must come from the same line. Detect it: list your dependencies
and look for `net.liftweb` artifacts (and lift-based addons) that predate the
jakarta line; the classpath must not mix them.

## Symptom: `NoClassDefFoundError: jakarta/servlet/...` in a javax container

The jakarta Lift line is running inside a javax-line container (Jetty 9,
Tomcat 9 or earlier). Move to a jakarta container: Jetty 11 (or Jetty 12 with
the EE 9+ environment), Tomcat 10.0+.

## Symptom: `IllegalStateException: No multipart config for servlet` on upload

Lift ingests uploads through the container's `getParts()`, which the
container serves only when the request's servlet carries multipart
configuration. Add `<multipart-config/>` to your `<servlet>` in `web.xml`
(or `registration.setMultipartConfig(...)` for programmatic registration).
This is new: commons-fileupload (used before the migration) bypassed the
container and needed none.

## Symptom: `web.xml` parse errors after the upgrade

The deployment descriptor must declare the Jakarta EE namespace and version
5.0 schema — see the example in
[migration/javax-to-jakarta.adoc](migration/javax-to-jakarta.adoc). The
javax-era schemas (`http://java.sun.com/xml/ns/javaee/...`) belong to the old
line.

## Detecting which line a jar was built against

A Lift jar's servlet line is visible in its dependencies: the javax line
carries `javax.servlet:javax.servlet-api`, the jakarta line
`jakarta.servlet:jakarta.servlet-api`. For a jar without its pom at hand,
inspect the bytecode: `javax/servlet/...` references inside
`net/liftweb/http/provider/servlet` mean the javax line.

## The two lines cannot be mixed

`javax.servlet` and `jakarta.servlet` classes are distinct types: an
application classloader that sees both lines fails at the seam (linkage
errors in the provider layer). Keep one line per application; upgrade all
Lift modules in lockstep (they release together).
