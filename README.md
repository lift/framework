# The Lift Web Framework

[![Build Status](https://travis-ci.org/lift/framework.svg?branch=master)](https://travis-ci.org/lift/framework)

Lift is the most powerful, most secure web framework available today. There are [Seven Things](http://seventhings.liftweb.net/) that distinguish Lift from other web frameworks.

Lift applications are:

- Secure -- Lift apps are resistant to common vulnerabilities including many of the OWASP Top 10
- Developer centric -- Lift apps are fast to build, concise and easy to maintain
- Scalable -- Lift apps are high performance and scale in the real world to handle insane traffic levels
- Interactive like a desktop app -- Lift's Comet support is unparalled and Lift's ajax support is super-easy and very secure

Because Lift applications are written in [Scala](http://www.scala-lang.org), an elegant JVM language, you can still use your favorite Java libraries and deploy to your favorite Servlet Container and app server. Use the code you've already written and deploy to the container you've already configured!

## Getting Started

**Compatibility note:**
As of Lift 3.0, you'll need to be running Java 8 to use Lift. For those using Java 6 or Java 7,
you'll need to use Lift 2.6 until you can upgrade your Java installation.

You can create a new Lift project using your favorite build system by adding Lift as a dependency:

### With sbt

We recommend using the latest sbt version, which is currently 0.13.15, but anything 0.13.6+ will work
with the instructions below.

Create or update your `project/plugins.sbt` file with the `xsbt-web-plugin`:

```scala
addSbtPlugin("com.github.siasia" %% "xsbt-web-plugin" % "3.0.2")
```

Then, add the plugin and Lift to your `build.sbt` file:

```scala
enablePlugins(JettyPlugin)

libraryDependencies ++= {
  val liftVersion = "3.1.0"
  Seq(
    "net.liftweb"       %% "lift-webkit" % liftVersion % "compile",
    "ch.qos.logback" % "logback-classic" % "1.2.5"
  )
}
```

### With Maven

You can use one of the several archetypes -- `lift-archetype-blank`, `lift-archetype-basic`, `lift-archetype-jpa-basic` -- to generate a new Lift project. You must set `archetypeRepository` and `remoteRepositories` to `http://scala-tools.org/repo-releases` or `http://scala-tools.org/repo-snapshots`, depending on whether you are using a release or the latest SNAPSHOT.

Or, you can add Lift to your `pom.xml` like so:

    <dependency>
      <groupId>net.liftweb</groupId>
      <artifactId>lift-webkit_${scala.version}</artifactId>
      <version>3.1.0</version>
    </dependency>

Where `${scala.version}` is `2.11` or `2.12`. Individual patch releases of the Scala compiler (e.g. 2.12.2)
are binary compatible with everything in their release series, so you only need the first two version parts.

You can learn more about Maven integration [on the wiki](http://www.assembla.com/wiki/show/liftweb/Using_Maven).

### Learning Lift

There are lots of resources out there for learning Lift. Some of our favorites include:

* [The Lift Cookbook](http://cookbook.liftweb.net/)
* [Simply Lift](http://simply.liftweb.net)

If you've found one that you particularly enjoy, please open a PR to update this README!

## Issues & Pull Requests

Per our [contributing guidelines][contribfile], Issues on the Lift GitHub project are intended to
describe actionable, already-discussed items. Committers on the project may open issues for
themselves at any time, but non-committers should visit the [Lift mailing
list](https://groups.google.com/forum/#!forum/liftweb) and start a discussion
for any issue that they wish to open.

We will accept issues and pull requests into the Lift codebase if the pull requests meet the following criteria:

* The request is for a [supported version of Lift][supfile]
* The request adheres to our [contributing guidelines][contribfile], including having been discussed
  on the Mailing List if its from a non-committer.

[contribfile]: https://github.com/lift/framework/blob/master/CONTRIBUTING.md
[sigfile]: https://github.com/lift/framework/blob/master/contributors.md

## Project Organization

The Lift Framework is divided into several components that are published independently. This organization enables you to use just the elements of Lift necessary for your project and no more.

### This Repository

This repository, `framework`, contains the following components:

* **core:** Core elements used by Lift projects. If you wish to reuse some of Lift's helpers and
constructs, such as `Box`, this component may be all you need. However, a web application will most
likely require one or more of Lift's other components.
* **web:** This component includes all of Lift's core HTTP and web handling. Including `lift-webkit`
in your build process should be sufficient for basic applications and will include `lift-core` as a
transitive dependency.
* **persistence:** This component includes Mapper and Record, Lift's two ORMs. While you needn't use
either and can use the ORM of your choice, Mapper and Record integrate nicely with Lift's idioms.
Mapper is an ORM for relational databases, while Record is a broader ORM with support for both SQL
databases and NoSQL datastores.

### Other Repostories

There are a variety of other repositories available on the Lift GitHub page. While many are concerned with building Lift or are build program archetypes, there are two you will probably encounter fairly frequently as a Lift user:

#### modules

The [modules](https://github.com/liftmodules) organization contains some of the many add-on modules
that are part of the Lift project. If you don't find a module you need here, consider
looking for it on the [Lift modules directory](https://liftweb.net/lift_modules) or
[creating a module](http://www.assembla.com/spaces/liftweb/wiki/Modules) and sharing it with the
community.

#### examples

The [examples](https://github.com/lift/examples) repository contains the source code for several example Lift applications, including [demo.liftweb.com](http://demo.liftweb.net/).

## Building Lift

If you simply want to use Lift in your project, add Lift as a dependency to your build system or [download the JAR files directly](https://www.liftweb.net/download).

If you wish to build Lift from source, check out this repository and use the included `liftsh` script to build some or all of the components you want.

    git clone https://github.com/lift/framework.git
    cd framework
    ./liftsh +update +publish

There is [additional documentation on the wiki](http://www.assembla.com/spaces/liftweb/wiki/Building_Lift).

## Additional Resources

### Homepage

The main Lift website is [http://www.liftweb.net](http://www.liftweb.net). The site contains information on the latest Lift releases, a getting started guide, links to several Lift online books, and additional information.

### Mailing List

The Lift Google Group is the official place for support and is an active, friendly community to boot! It can be found at [http://groups.google.com/forum/#!forum/liftweb](http://groups.google.com/forum/#!forum/liftweb).

### Wiki

The Lift wiki is hosted on Assembla and can be found at [http://www.assembla.com/spaces/liftweb/wiki/](http://www.assembla.com/spaces/liftweb/wiki/). Anyone is welcome to contribute to the wiki; you must create an account and watch the Lift project in order to create or edit wiki pages.

### ScalaDocs

The ScalaDocs for each release of Lift, in additional to the actual JARs, are available on the Liftweb.net site. You can access the source code–based documentation for releases via the site's homepage or by navigating directly to the URL for the specific release. For instance, the Lift 3.0 release can be accessed at [http://liftweb.net/api/31/api/](http://liftweb.net/api/31/api/).

## License

Lift is open source software released under the **Apache 2.0 license**.

## Continuous Integration

SNAPSHOTs are built by [Travis CI](https://travis-ci.org/lift/framework)
