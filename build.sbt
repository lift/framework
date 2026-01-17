import Dependencies._
import LiftSbtHelpers._

ThisBuild / organization         := "net.liftweb"
ThisBuild / version              := "4.0.0-SNAPSHOT"
ThisBuild / homepage             := Some(url("https://www.liftweb.net"))
ThisBuild / licenses             += ("Apache License, Version 2.0", url("https://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / startYear            := Some(2006)
ThisBuild / organizationName     := "WorldWide Conferencing, LLC"

val scala213Version = "2.13.18"
val scala3LTSVersion = "3.3.7"

ThisBuild / scalaVersion         := scala213Version
ThisBuild / crossScalaVersions   := Seq(scala213Version, scala3LTSVersion)

ThisBuild / libraryDependencies ++= Seq(
  specs2(scalaVersion.value),
  specs2Matchers(scalaVersion.value),
  scalacheck(scalaVersion.value),
  scalactic,
  scalatest
)

ThisBuild / scalacOptions       ++= Seq("-deprecation")

// Settings for Sonatype compliance
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
  val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
  if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
  else localStaging.value
}
ThisBuild / publishMavenStyle := true

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
  Seq(common, actor, markdown, util)

lazy val common =
  coreProject("common")
    .settings(
      description := "Common Libraries and Utilities",
      libraryDependencies ++= Seq(slf4j_api, logback, slf4j_log4j12, scala_xml, scala_parser, scalamock)
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
      libraryDependencies ++= Seq(scalatest, scala_xml, scala_parser)
    )

lazy val documentationHelpers =
  coreProject("documentation-helpers")
    .settings(description := "Documentation Helpers")
    .dependsOn(util)

lazy val util =
  coreProject("util")
    .dependsOn(actor, markdown)
    .settings(
      description := "Utilities Library",
      Test / parallelExecution := false,
      libraryDependencies ++= Seq(
        scala_compiler(scalaVersion.value),
        joda_time,
        joda_convert,
        commons_codec,
        log4j,
        htmlparser,
        xerces,
        json4s_native,
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
      libraryDependencies ++= Seq(commons_httpclient, servlet_api, json4s_native, json4s_xml)
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
        specs2Prov(scalaVersion.value),
        specs2MatchersProv(scalaVersion.value),
        jetty11,
	      jettywebapp,
        jwebunit,
        mockito_scalatest(scalaVersion.value),
        jquery,
        jasmineCore,
        jasmineAjax,
        specs2Mock(scalaVersion.value)
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
