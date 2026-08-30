import Dependencies._
import LiftSbtHelpers._

ThisBuild / organization := "net.liftweb"
ThisBuild / version := "3.5.0-jakarta"

// Pin scala-xml to 1.3.x for ALL cross-builds: newer transitive resolutions
// (2.1.0 under the 2.12 graph) changed NoBindingFactoryAdapter.hStack to a
// List and broke HtmlParser's .push usage. 1.3.0 is what Lift 3.5.0 was
// built against (the app pins 1.3.1 for the same reason).
ThisBuild / dependencyOverrides += "org.scala-lang.modules" %% "scala-xml" % "1.3.0"
ThisBuild / homepage := Some(url("http://www.liftweb.net"))
ThisBuild / licenses += (
  "Apache License, Version 2.0",
  url("http://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / startYear := Some(2006)
ThisBuild / organizationName := "WorldWide Conferencing, LLC"

val scala211Version = "2.11.12"
val scala212Version = "2.12.12"
val scala213Version = "2.13.18"

val crossUpTo212 = Seq(scala212Version, scala211Version)
val crossUpTo213 = scala213Version +: crossUpTo212

// Pure Scala 2.13 build for this fork (jakarta branch): the 2.12
// cross-build is dropped — the 3.5.0 sources are 2.13-flavored and the
// 2.12 line is unmaintained. scalaVersion must match the app (2.13.18).
ThisBuild / scalaVersion := scala213Version

ThisBuild / libraryDependencies ++= Seq(
  specs2,
  specs2Matchers,
  specs2Mock,
  scalacheck,
  scalactic,
  scalatest)

ThisBuild / scalacOptions ++= Seq("-deprecation", "-Wunused:imports")
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

// Settings for Sonatype compliance
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
  if (isSnapshot.value) {
    Some(Resolver.sonatypeOssRepos("snapshots").head)
  } else {
    Some(Opts.resolver.sonatypeStaging)
  }
}
ThisBuild / scmInfo := Some(ScmInfo(
  url("https://github.com/lift/framework"),
  "scm:git:https://github.com/lift/framework.git"))
ThisBuild / pomExtra := Developers.toXml

ThisBuild / credentials += Credentials(BuildPaths.getGlobalSettingsDirectory(
  state.value,
  BuildPaths.getGlobalBase(state.value)) / ".credentials")

initialize := {
  printLogo(name.value, version.value, scalaVersion.value)
}

ThisBuild / resolvers ++= Seq(
  "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "releases" at "https://oss.sonatype.org/content/repositories/releases"
)

lazy val liftProjects = core ++ web ++ persistence

lazy val framework =
  liftProject("lift-framework", file("."))
    .aggregate(liftProjects: _*)
    .enablePlugins(ScalaUnidocPlugin)

// Core Projects
// -------------
lazy val core: Seq[ProjectReference] =
  Seq(common, actor, markdown, json, json_scalaz7, json_ext, util)

lazy val common =
  coreProject("common")
    .settings(
      description := "Common Libraties and Utilities",
      libraryDependencies ++= Seq(slf4j_api, logback, slf4j_log4j12, scala_xml, scala_parser)
    )

lazy val actor =
  coreProject("actor")
    .dependsOn(common)
    .settings(
      description := "Simple Actor",
      Test / parallelExecution := false
    )

lazy val markdown =
  coreProject("markdown")
    .settings(
      description := "Markdown Parser",
      Test / parallelExecution := false,
      libraryDependencies ++= Seq(scalatest, scalatest_junit, scala_xml, scala_parser)
    )

lazy val json =
  coreProject("json")
    .settings(
      description := "JSON Library",
      Test / parallelExecution := false,
      libraryDependencies ++= Seq(scalap(scalaVersion.value), paranamer, scala_xml)
    )

lazy val documentationHelpers =
  coreProject("documentation-helpers")
    .settings(description := "Documentation Helpers")
    .dependsOn(util)

lazy val json_scalaz7 =
  coreProject("json-scalaz7")
    .dependsOn(json)
    .settings(
      description := "JSON Library based on Scalaz 7",
      libraryDependencies ++= Seq(scalaz7)
    )

lazy val json_ext =
  coreProject("json-ext")
    .dependsOn(common, json)
    .settings(
      description := "Extentions to JSON Library",
      libraryDependencies ++= Seq(commons_codec, joda_time, joda_convert)
    )

lazy val util =
  coreProject("util")
    .dependsOn(actor, json, markdown)
    .settings(
      description := "Utilities Library",
      Test / parallelExecution := false,
      libraryDependencies ++= Seq(
        scala_compiler(scalaVersion.value),
        joda_time,
        joda_convert,
        commons_codec,
        javamail,
        log4j,
        htmlparser,
        xerces,
        jbcrypt
      )
    )

// Web Projects
// ------------
lazy val web: Seq[ProjectReference] =
  Seq(testkit, webkit)

lazy val testkit =
  webProject("testkit")
    .dependsOn(util)
    .settings(
      description := "Testkit for Webkit Library",
      libraryDependencies ++= Seq(commons_httpclient, servlet_api)
    )

lazy val webkit =
  webProject("webkit")
    .dependsOn(util, testkit % "provided")
    .settings(
      description := "Webkit Library",
      Test / parallelExecution := false,
      libraryDependencies ++= Seq(
        commons_fileupload,
        rhino,
        servlet_api,
        specs2Prov,
        specs2MatchersProv,
        jetty11,
        jettywebapp,
        jwebunit,
        mockito_scalatest,
        jquery,
        jasmineCore,
        jasmineAjax
      ),
      libraryDependencies ++= {
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, scalaMajor)) if scalaMajor >= 13 => Seq(scala_parallel_collections)
          case _ => Seq.empty
        }
      },
      Test / initialize := {
        System.setProperty(
          "net.liftweb.webapptest.src.test.webapp",
          ((Test / sourceDirectory).value / "webapp").absString
        )
      },
      Compile / unmanagedSourceDirectories += {
        (Compile / sourceDirectory).value / ("scala_" + scalaBinaryVersion.value)
      },
      Test / unmanagedSourceDirectories += {
        (Test / sourceDirectory).value / ("scala_" + scalaBinaryVersion.value)
      },
      Compile / compile := (Compile / compile).dependsOn(WebKeys.assets).value,
      /**
       * This is to ensure that the tests in net.liftweb.webapptest run last so that other tests
       * (MenuSpec in particular) run before the SiteMap is set.
       */
      Test / testGrouping := {
        (Test / definedTests).map { tests =>
          import Tests._

          val (webapptests, others) = tests.partition { test =>
            test.name.startsWith("net.liftweb.webapptest")
          }

          Seq(
            new Group("others", others, InProcess),
            new Group("webapptests", webapptests, InProcess)
          )
        }.value
      }
    )
    .enablePlugins(SbtWeb)

// Persistence Projects
// --------------------
// record, squeryl_record, mongodb, mongodb_record are 2.12-only and are
// dropped from this pure-2.13 fork.
lazy val persistence: Seq[ProjectReference] =
  Seq(db, proto, mapper)

lazy val db =
  persistenceProject("db")
    .dependsOn(util, webkit)
    .settings(libraryDependencies += mockito_scalatest)

lazy val proto =
  persistenceProject("proto")
    .dependsOn(webkit)

lazy val mapper =
  persistenceProject("mapper")
    .dependsOn(db, proto)
    .settings(
      description := "Mapper Library",
      Test / parallelExecution := false,
      libraryDependencies ++= Seq(h2, derby, jbcrypt),
      Test / initialize := {
        System.setProperty(
          "derby.stream.error.file",
          ((Test / crossTarget).value / "derby.log").absolutePath
        )
      }
    )

lazy val record =
  persistenceProject("record")
    .dependsOn(proto)
    .settings(libraryDependencies ++= Seq(jbcrypt))

lazy val squeryl_record =
  persistenceProject("squeryl-record")
    .dependsOn(record, db)
    .settings(libraryDependencies ++= Seq(h2, squeryl))

lazy val mongodb =
  persistenceProject("mongodb")
    .dependsOn(json_ext, util)
    .settings(
      Test / parallelExecution := false,
      libraryDependencies ++= Seq(mongo_java_driver, mongo_java_driver_async),
      Test / initialize := {
        System.setProperty(
          "java.util.logging.config.file",
          ((Test / resourceDirectory).value / "logging.properties").absolutePath
        )
      }
    )

lazy val mongodb_record =
  persistenceProject("mongodb-record")
    .dependsOn(record, mongodb)
    .settings(
      Test / parallelExecution := false
    )
