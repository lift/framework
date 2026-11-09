# Lift documentation

Documentation for the Lift Web Framework: how-to guides, tutorials, and
architecture.

## Getting started / tutorials

* [Simply Lift](simply_lift.md) — the framework overview booklet.
* [Getting started tutorial](getting-started-tutorial/1-view-first-development.adoc) —
  view-first development, menus, snippets, forms, Comet actors (parts 1–9).

## Reference

* [CSS selectors](css-selectors.adoc)
* [Dependency injection in Lift](dependency-injection-liftweb-scala.adoc)

## Operations

* [Running the MongoDB-backed tests](mongodb-docker.md) — MongoDB in Docker
  for `lift-mongodb` and `lift-mongodb-record` tests.

## Servlet integration and architecture

* [Servlet integration](servlet-integration.md) — container requirements,
  the Lift types that wrap container types, and the long-polling flow.
* [Servlet behavior test map](servlet-behavior-test-map.md) — each documented
  servlet behavior and the test that proves it.
* [Servlet provider architecture](architecture/servlet-provider-architecture.md) —
  theory of operation of the adapter layer.

## Migration

* [Migrating Lift from javax.servlet to jakarta.servlet](migration/javax-to-jakarta.adoc) —
  what changes for applications on the jakarta line.
* [Upgrading from javax Lift](upgrading-from-javax-lift.md) —
  troubleshooting recipe for the upgrade.

## Migrating between versions

* [2.6 to 3.0: LiftScreen](migration/2.6-to-3.0-lift-screen.adoc)
* [2.6 to 3.0: Paginator](migration/2.6-to-3.0-paginator.adoc)

## For LLMs

LLM-oriented parallel copies live under [llm/](llm/).
