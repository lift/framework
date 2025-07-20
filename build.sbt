import Dependencies._
import LiftSbtHelpers._

ThisBuild / organization         := "net.liftweb"
ThisBuild / version              := "4.0.0-SNAPSHOT"
ThisBuild / homepage             := Some(url("https://www.liftweb.net"))
ThisBuild / licenses             += ("Apache License, Version 2.0", url("https://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / startYear            := Some(2006)
ThisBuild / organizationName     := "WorldWide Conferencing, LLC"

val scala213Version = "2.13.16"


ThisBuild / scalaVersion         := scala213Version

ThisBuild / libraryDependencies ++= Seq(specs2, specs2Matchers, specs2Mock, scalacheck, scalactic, scalatest)

ThisBuild / scalacOptions       ++= Seq("-deprecation")

// Settings for Sonatype compliance
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
  if (isSnapshot.value) {
    Some(Opts.resolver.sonatypeSnapshots)
  } else {
    Some(Opts.resolver.sonatypeStaging)
  }
}
ThisBuild / scmInfo   := Some(ScmInfo(url("https://github.com/lift/framework"), "scm:git:https://github.com/lift/framework.git"))
ThisBuild / pomExtra  := Developers.toXml

ThisBuild / credentials += Credentials(BuildPaths.getGlobalSettingsDirectory(state.value, BuildPaths.getGlobalBase(state.value)) / ".credentials")

initialize := {
  printLogo(name.value, version.value, scalaVersion.value)
}

ThisBuild / resolvers  ++= Seq(
  "snapshots"     at "https://oss.sonatype.org/content/repositories/snapshots",
  "releases"      at "https://oss.sonatype.org/content/repositories/releases"
)

lazy val liftProjects = core ++ web

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
      description := "Common Libraries and Utilities",
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
      libraryDependencies ++= Seq(scalap(scalaVersion.value), paranamer,  scala_xml)
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
        * This is to ensure that the tests in net.liftweb.webapptest run last
        * so that other tests (MenuSpec in particular) run before the SiteMap
        * is set.
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
      },

    )
    .enablePlugins(SbtWeb)

// Persistence Projects
// --------------------
lazy val persistence: Seq[ProjectReference] =
  Seq(db, proto, mapper, record, mongodb, mongodb_record)

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
