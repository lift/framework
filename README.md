# The Lift Web Framework

Lift is the most powerful, most secure web framework available today. There are [Seven Things](http://seventhings.liftweb.net/) that distinguish Lift from other web frameworks.

Lift applications are:

- Secure -- Lift apps are resistant to common vulnerabilities including many of the OWASP Top 10
- Developer centric -- Lift apps are fast to build, concise and easy to maintain
- Scalable -- Lift apps are high performance and scale in the real world to handle insane traffic levels
- Interactive like a desktop app -- Lift's Comet support is unparalled and Lift's ajax support is super-easy and very secure

Because Lift applications are written in [Scala](http://www.scala-lang.org), an elegant JVM language, you can still use your favorite Java libraries and deploy to your favorite Servlet Container and app server. Use the code you've already written and deploy to the container you've already configured!

## Pull Requests

We will accept pull requests into the [Lift codebase](https://github.com/lift)
if the pull requests meet the following criteria:

* One or more of the following:
    * Documentation including ScalaDoc comments in code
    * Example code
    * Small changes, enhancements, or bug fixes to Lift's code
* Each pull request must include a signature at the bottom of the
  `/contributors.md` file.

## Getting Started

You can create a new Lift project using your favorite build system by adding Lift as a dependency:

#### sbt 0.11

Create or update your `project/plugins.sbt` file with the `xsbt-web-plugin`:

	libraryDependencies <+= sbtVersion(v => "com.github.siasia" %% "xsbt-web-plugin" % (v+"-0.2.7"))

Then, add the plugin and Lift to your `build.sbt` file:

	seq(webSettings :_*)
	
	libraryDependencies ++= {
		val liftVersion = "2.4"
		Seq(
		  "net.liftweb" %% "lift-webkit" % liftVersion % "compile",
		  "org.mortbay.jetty" % "jetty" % "6.1.22" % "container",
		  "ch.qos.logback" % "logback-classic" % "0.9.26"
		)
	}

You can [learn more on the wiki](http://www.assembla.com/wiki/show/liftweb/Using_SBT).

#### Maven:

You can use one of the several archetypes -- `lift-archetype-blank`, `lift-archetype-basic`, `lift-archetype-jpa-basic` -- to generate a new Lift project. You must set `archetypeRepository` and `remoteRepositories` to `http://scala-tools.org/repo-releases` or `http://scala-tools.org/repo-snapshots`, depending on whether you are using a release or the latest SNAPSHOT.

Or, you can add Lift to your `pom.xml` like so:

    <dependency>
      <groupId>net.liftweb</groupId>
      <artifactId>lift-mapper_${scala.version}</artifactId>
      <version>2.4</version>
    </dependency>

Where `${scala.version}` is `2.8.0`, `2.8.1`, `2.9.1` etc.

You can [learn more on the wiki](http://www.assembla.com/wiki/show/liftweb/Using_Maven).

## Project Organization

The Lift Framework is divided into several Git repositories, which in turn are divided into several components that are published independently. This organization enables you to use just the elements of Lift necessary for your project and no more.

### This Repository

This repository, `framework`, contains the following components:

#### core

Core elements used by Lift projects. If you wish to reuse some of Lift's helpers and constructs, such as `Box`, this component may be all you need. However, a web application will most likely require one or more of Lift's components.

#### web

This component includes all of Lift's core HTTP and web handling. Including `lift-webkit` in your build process should be sufficient for basic applications and will include `lift-core` as a transitive dependency.

#### persistence

This component includes Mapper and Record, Lift's two ORMs. While you needn't use either and can use the ORM of your choice, Mapper and Record integrate nicely with Lift's idioms. Mapper is an ORM for relational databases, while Record is a broader ORM with support for both SQL databases and NoSQL datastores.

### Other Repostories

There are a variety of other repositories available on the Lift GitHub page. While many are concerned with building Lift or are build program archetypes, there are two you will probably encounter fairly frequently as a Lift user:

#### modules

The [modules](https://github.com/liftmodules) repository contains the many add-on modules that are part of the Lift project. If you don't find a module you need here, consider [creating a module](http://www.assembla.com/spaces/liftweb/wiki/Modules) and sharing it with the community.

Please note that the modules project does accept pull requests.

#### examples

The [examples](https://github.com/lift/examples) repository contains the source code for several example Lift applications, including [demo.liftweb.com](http://demo.liftweb.com/).

## Building Lift

If you simply want to use Lift in your project, add Lift as a dependency to your build system or [download the JAR files directly](www.liftweb.net/download).

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

The ScalaDocs for each release of Lift, in additional to the actual JARs, are available on ScalaTools. You can access the source code–based documentation for releases via the site's homepage or by navigating directly to the URL for the specific release. For instance, the Lift 2.4 release can be accessed at [http://scala-tools.org/mvnsites/liftweb-2.4/](http://scala-tools.org/mvnsites/liftweb-2.4/).

## License

Lift is open source software released under the **Apache 2.0 license**. You must be a committer with signed committer agreement to submit patches. You can learn more about Lift's committer policy on the Lift website.

## Continuous Integration

SNAPSHOTs are built at CloudBees: https://lift.ci.cloudbees.com/