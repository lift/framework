/*
 * Copyright 2012-2015 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import sbt._
import Keys._
import net.liftweb.sbt.LiftBuildPlugin._
import Dependencies._

import com.typesafe.sbt.web.SbtWeb
import com.typesafe.sbt.web.SbtWeb.autoImport._

/**
 * Pattern-matches an attributed file, extracting its module organization,
 * name, and revision if available in its attributes.
 */
object MatchingModule {
  def unapply(file: Attributed[File]): Option[(String,String,String)] = {
    file.get(moduleID.key).map { moduleInfo =>
      (moduleInfo.organization, moduleInfo.name, moduleInfo.revision)
    }
  }
}

object BuildDef extends Build {

  /**
   * A helper that returns the revision and JAR file for a given dependency.
   * Useful when trying to attach API doc URI information.
   */
  def findManagedDependency(classpath: Seq[Attributed[File]],
                            organization: String,
                            name: String): Option[(String,File)] = {
    classpath.collectFirst {
      case entry @ MatchingModule(moduleOrganization, moduleName, revision)
          if moduleOrganization == organization &&
             moduleName.startsWith(name) =>
        (revision, entry.data)
    }
  }

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
      .settings(description := "Common Libraties and Utilities",
                libraryDependencies ++= Seq(slf4j_api, logback, slf4j_log4j12, scala_xml, scala_parser)
      )

  lazy val actor =
    coreProject("actor")
        .dependsOn(common)
        .settings(scalacOptions in (Compile, doc) ++= Seq(scalaVersion.value).flatMap {
          case v if v.startsWith("2.12") =>
            Seq("-no-java-comments")
          case _ =>
            Seq()
        }) //workaround for scala/scala-dev#249
        .settings(description := "Simple Actor",
                  parallelExecution in Test := false)

  lazy val markdown =
    coreProject("markdown")
        .settings(description := "Markdown Parser",
                  parallelExecution in Test := false,
                  libraryDependencies <++= scalaVersion { sv => Seq(scalatest, junit, scala_xml, scala_parser) }
      )

  lazy val json =
    coreProject("json")
        .settings(description := "JSON Library",
                  parallelExecution in Test := false,
                  libraryDependencies <++= scalaVersion { sv => Seq(scalap(sv), paranamer) })

  lazy val documentationHelpers =
    coreProject("documentation-helpers")
        .settings(description := "Documentation Helpers")
        .dependsOn(util)

  lazy val json_scalaz7 =
    coreProject("json-scalaz7")
        .dependsOn(json)
        .settings(description := "JSON Library based on Scalaz 7",
                  libraryDependencies ++= Seq(scalaz7))

  lazy val json_ext =
    coreProject("json-ext")
        .dependsOn(common, json)
        .settings(description := "Extentions to JSON Library",
                  libraryDependencies ++= Seq(commons_codec, joda_time, joda_convert))

  lazy val util =
    coreProject("util")
        .dependsOn(actor, json, markdown)
        .settings(description := "Utilities Library",
                  parallelExecution in Test := false,
                  libraryDependencies <++= scalaVersion {sv =>  Seq(scala_compiler(sv), joda_time,
                    joda_convert, commons_codec, javamail, log4j, htmlparser, xerces)}
                  )

  // Web Projects
  // ------------
  lazy val web: Seq[ProjectReference] =
    Seq(testkit, webkit)

  lazy val testkit =
    webProject("testkit")
        .dependsOn(util)
        .settings(description := "Testkit for Webkit Library",
                  libraryDependencies ++= Seq(commons_httpclient, servlet_api))
  lazy val webkit =
    webProject("webkit")
        .dependsOn(util, testkit % "provided")
        .settings(scalacOptions in (Compile, doc) ++= {
          if (scalaVersion.value.startsWith("2.12")) {
            Seq("-no-java-comments")
          } else {
            Seq()
          }
        }) //workaround for scala/scala-dev#249
        .settings(libraryDependencies ++= Seq(mockito_all, jquery, jasmineCore, jasmineAjax))
        .settings(yuiCompressor.Plugin.yuiSettings: _*)
        .settings(description := "Webkit Library",
                  parallelExecution in Test := false,
                  libraryDependencies <++= scalaVersion { sv =>
                    Seq(commons_fileupload, rhino, servlet_api, specs2.copy(configurations = Some("provided")), specs2Matchers.copy(configurations = Some("provided")), jetty6,
                      jwebunit)
                  },
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
                  })
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
        .settings(description := "Mapper Library",
                  parallelExecution in Test := false,
                  libraryDependencies ++= Seq(h2, derby),
                  initialize in Test <<= (crossTarget in Test) { ct =>
                    System.setProperty("derby.stream.error.file", (ct / "derby.log").absolutePath)
                  })

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
        .settings(parallelExecution in Test := false,
                  libraryDependencies += mongo_driver,
                  initialize in Test <<= (resourceDirectory in Test) { rd =>
                    System.setProperty("java.util.logging.config.file", (rd / "logging.properties").absolutePath)
                  })

  lazy val mongodb_record =
    persistenceProject("mongodb-record")
        .dependsOn(record, mongodb)
        .settings(parallelExecution in Test := false)


  def coreProject = liftProject("core") _
  def webProject = liftProject("web") _
  def persistenceProject = liftProject("persistence") _

  /** Project definition helper that simplifies creation of `ProjectReference`.
    *
    * It is a convenience method to create a Lift `ProjectReference` module by having the boilerplate for most common
    * activities tucked in.
    *
    * @param base     the base path location of project module.
    * @param prefix   the prefix of project module.
    * @param module   the name of the project module. Typically, a project id is of the form lift-`module`.
    */
  def liftProject(base: String, prefix: String = "lift-")(module: String): Project =
    liftProject(id = if (module.startsWith(prefix)) module else prefix + module,
                base = file(base) / module.stripPrefix(prefix))

  def liftProject(id: String, base: File): Project = {
    Project(id, base)
      .settings(liftBuildSettings: _*)
      .settings(scalacOptions ++= List("-feature", "-language:implicitConversions"))
      .settings(
        autoAPIMappings := true,
        apiMappings ++= {
          val cp: Seq[Attributed[File]] = (fullClasspath in Compile).value

          findManagedDependency(cp, "org.scala-lang.modules", "scala-xml").map {
            case (revision, file)  =>
              (file -> url("http://www.scala-lang.org/api/" + version))
          }.toMap
        }
      )
  }
}
