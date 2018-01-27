import Dependencies._
import LiftSbtHelpers._

organization in ThisBuild          := "net.liftweb"
version in ThisBuild               := "3.2.0"
homepage in ThisBuild              := Some(url("http://www.liftweb.net"))
licenses in ThisBuild              += ("Apache License, Version 2.0", url("http://www.apache.org/licenses/LICENSE-2.0.txt"))
startYear in ThisBuild             := Some(2006)
organizationName in ThisBuild      := "WorldWide Conferencing, LLC"
scalaVersion in ThisBuild          := "2.12.2"
crossScalaVersions in ThisBuild    := Seq("2.12.2", "2.11.11")

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
pomExtra in ThisBuild  :=  Developers.toXml

credentials in ThisBuild += Credentials(BuildPaths.getGlobalSettingsDirectory(state.value, BuildPaths.getGlobalBase(state.value)) / ".credentials")

initialize <<= (name, version, scalaVersion).apply(printLogo)

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
    .settings(aggregatedSetting(sources in(Compile, doc)),
              aggregatedSetting(dependencyClasspath in(Compile, doc)),
              publishArtifact := false)

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
      libraryDependencies ++= Seq(scalap(scalaVersion.value), paranamer)
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
        xerces
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
        specs2.copy(configurations = Some("provided")),
        specs2Matchers.copy(configurations = Some("provided")),
        jetty6,
        jwebunit,
        mockito_all,
        jquery,
        jasmineCore,
        jasmineAjax
      ),
      initialize in Test <<= (sourceDirectory in Test) { src =>
        System.setProperty("net.liftweb.webapptest.src.test.webapp", (src / "webapp").absString)
      },
      unmanagedSourceDirectories in Compile <+= (sourceDirectory in Compile, scalaBinaryVersion) {
        (sourceDirectory, binaryVersion) =>
          sourceDirectory / ("scala_" + binaryVersion)
      },
      unmanagedSourceDirectories in Test <+= (sourceDirectory in Test, scalaBinaryVersion) {
        (sourceDirectory, binaryVersion) =>
          sourceDirectory / ("scala_" + binaryVersion)
      },
      (compile in Compile) <<= (compile in Compile) dependsOn (WebKeys.assets),
      /**
        * This is to ensure that the tests in net.liftweb.webapptest run last
        * so that other tests (MenuSpec in particular) run before the SiteMap
        * is set.
        */
      testGrouping in Test <<= (definedTests in Test).map { tests =>
        import Tests._

        val (webapptests, others) = tests.partition { test =>
          test.name.startsWith("net.liftweb.webapptest")
        }

        Seq(
          new Group("others", others, InProcess),
          new Group("webapptests", webapptests, InProcess)
        )
      },

      scalacOptions in (Compile, doc) ++= {
        if (scalaVersion.value.startsWith("2.12")) {
          Seq("-no-java-comments")
        } else {
          Seq()
        } //workaround for scala/scala-dev#249
      }
    )
    .settings(yuiCompressor.Plugin.yuiSettings: _*)
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
      libraryDependencies ++= Seq(h2, derby),
      initialize in Test <<= (crossTarget in Test) { ct =>
        System.setProperty("derby.stream.error.file", (ct / "derby.log").absolutePath)
      }
    )

lazy val record =
  persistenceProject("record")
    .dependsOn(proto)

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
      initialize in Test <<= (resourceDirectory in Test) { rd =>
        System.setProperty("java.util.logging.config.file", (rd / "logging.properties").absolutePath)
      }
    )

lazy val mongodb_record =
  persistenceProject("mongodb-record")
    .dependsOn(record, mongodb)
    .settings(parallelExecution in Test := false)
