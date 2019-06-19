import Dependencies._
import LiftSbtHelpers._

organization in ThisBuild          := "net.liftweb"
version in ThisBuild               := "3.4.0-SNAPSHOT"
homepage in ThisBuild              := Some(url("http://www.liftweb.net"))
licenses in ThisBuild              += ("Apache License, Version 2.0", url("http://www.apache.org/licenses/LICENSE-2.0.txt"))
startYear in ThisBuild             := Some(2006)
organizationName in ThisBuild      := "WorldWide Conferencing, LLC"
scalaVersion in ThisBuild          := "2.13.0"
crossScalaVersions in ThisBuild    := Seq("2.13.0","2.12.6", "2.11.11")

libraryDependencies in ThisBuild ++= Seq(specs2, specs2Matchers, specs2Mock, scalacheck, scalatest)

// Settings for Sonatype compliance
pomIncludeRepository in ThisBuild := { _ => false }
publishTo in ThisBuild := {
  if (isSnapshot.value) {
    Some(Opts.resolver.sonatypeSnapshots)
  } else {
    Some(Opts.resolver.sonatypeStaging)
  }
}
scmInfo in ThisBuild   := Some(ScmInfo(url("https://github.com/lift/framework"), "scm:git:https://github.com/lift/framework.git"))
pomExtra in ThisBuild  := Developers.toXml

credentials in ThisBuild += Credentials(BuildPaths.getGlobalSettingsDirectory(state.value, BuildPaths.getGlobalBase(state.value)) / ".credentials")

initialize := {
  printLogo(name.value, version.value, scalaVersion.value)
}

resolvers  in ThisBuild  ++= Seq(
  "snapshots"     at "https://oss.sonatype.org/content/repositories/snapshots",
  "releases"      at "https://oss.sonatype.org/content/repositories/releases"
)

lazy val liftProjects = core ++ web ++ persistence

lazy val framework =
  liftProject("lift-framework", file("."))
    .aggregate(liftProjects: _*)
    .settings(scalacOptions in (Compile, doc) ++= Seq(scalaVersion.value).flatMap {
      case v if v.startsWith("2.12") =>
        Seq("-no-java-comments")
      case _ =>
        Seq()
    }) //workaround for scala/scala-dev#249
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
      scalacOptions in (Compile, doc) ++= Seq(scalaVersion.value).flatMap {
        case v if v.startsWith("2.12") =>
          Seq("-no-java-comments")
        case _ =>
          Seq()
      },  //workaround for scala/scala-dev#249
      description := "Simple Actor",
      parallelExecution in Test := false
    )

lazy val markdown =
  coreProject("markdown")
    .settings(
      description := "Markdown Parser",
      parallelExecution in Test := false,
      libraryDependencies ++= Seq(scalatest, junit, scala_xml, scala_parser)
    )

lazy val json =
  coreProject("json")
    .settings(
      description := "JSON Library",
      parallelExecution in Test := false,
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
      parallelExecution in Test := false,
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
      parallelExecution in Test := false,
      libraryDependencies ++= Seq(
        commons_fileupload,
        rhino,
        servlet_api,
        specs2Prov,
        specs2MatchersProv,
        jetty6,
        jwebunit,
        mockito_all,
        jquery,
        jasmineCore,
        jasmineAjax
      ),
      initialize in Test := {
        System.setProperty(
          "net.liftweb.webapptest.src.test.webapp",
          ((sourceDirectory in Test).value / "webapp").absString
        )
      },
      unmanagedSourceDirectories in Compile += {
        (sourceDirectory in Compile).value / ("scala_" + scalaBinaryVersion.value)
      },
      unmanagedSourceDirectories in Test += {
        (sourceDirectory in Test).value / ("scala_" + scalaBinaryVersion.value)
      },
      compile in Compile := (compile in Compile).dependsOn(WebKeys.assets).value,
      /**
        * This is to ensure that the tests in net.liftweb.webapptest run last
        * so that other tests (MenuSpec in particular) run before the SiteMap
        * is set.
        */
      testGrouping in Test := {
        (definedTests in Test).map { tests =>
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

      scalacOptions in (Compile, doc) ++= {
        if (scalaVersion.value.startsWith("2.12")) {
          Seq("-no-java-comments")
        } else {
          Seq()
        } //workaround for scala/scala-dev#249
      }
    )
    .enablePlugins(SbtWeb)

// Persistence Projects
// --------------------
lazy val persistence: Seq[ProjectReference] =
  Seq(db, proto, mapper, record, squeryl_record, mongodb, mongodb_record)

lazy val db =
  persistenceProject("db")
    .dependsOn(util, webkit)
    .settings(libraryDependencies += mockito_all)

lazy val proto =
  persistenceProject("proto")
    .dependsOn(webkit)

lazy val mapper =
  persistenceProject("mapper")
    .dependsOn(db, proto)
    .settings(
      description := "Mapper Library",
      parallelExecution in Test := false,
      libraryDependencies ++= Seq(h2, derby, jbcrypt),
      initialize in Test := {
        System.setProperty(
          "derby.stream.error.file",
          ((crossTarget in Test).value / "derby.log").absolutePath
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
      parallelExecution in Test := false,
      libraryDependencies ++= Seq(mongo_java_driver, mongo_java_driver_async),
      initialize in Test := {
        System.setProperty(
          "java.util.logging.config.file",
          ((resourceDirectory in Test).value / "logging.properties").absolutePath
        )
      }
    )

lazy val mongodb_record =
  persistenceProject("mongodb-record")
    .dependsOn(record, mongodb)
    .settings(parallelExecution in Test := false)
