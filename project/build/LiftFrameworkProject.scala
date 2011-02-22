/*
 * Copyright 2010-2011 WorldWide Conferencing, LLC
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
import net.liftweb.sbt._


class LiftFrameworkProject(info: ProjectInfo) extends ParentProject(info) with LiftParentProject {

  import CompileScope._
  import ProvidedScope._


  // Core projects
  // -------------
  lazy val common   = coreProject("common", slf4j_api, logback, log4j)()
  lazy val actor    = coreProject("actor")(common)
  lazy val json     = coreProject("json", paranamer)()
  lazy val json_ext = coreProject("json-ext", commons_codec, joda_time)(common, json)
  lazy val util     = coreProject("util", joda_time, commons_codec, javamail, log4j, htmlparser)(actor, json)


  // Web projects
  // ------------
  lazy val testkit = webProject("testkit", commons_httpclient, servlet_api)(util)
  lazy val webkit  = webkitProject("webkit", commons_fileupload, servlet_api, TestScope.jetty6, TestScope.jwebunit)(util, testkit)
  lazy val wizard  = webProject("wizard")(webkit, db)


  // Persistence projects
  // --------------------
  lazy val db             = persistenceProject("db")(util)
  lazy val proto          = persistenceProject("proto")(webkit)
  lazy val jpa            = persistenceProject("jpa", scalajpa, persistence_api)(webkit)
  lazy val mapper         = persistenceProject("mapper", RuntimeScope.h2database, RuntimeScope.derby)(db, proto)
  lazy val record         = persistenceProject("record")(mapper)
  lazy val ldap           = persistenceProject("ldap", TestScope.apacheds)(mapper)
  lazy val couchdb        = persistenceProject("couchdb", dispatch_http)(record)
  lazy val squeryl_record = persistenceProject("squeryl-record", RuntimeScope.h2database, squeryl)(record)
  lazy val mongodb        = persistenceProject("mongodb", mongo_driver)(json_ext)
  lazy val mongodb_record = persistenceProject("mongodb-record", dispatch_http)(record, mongodb)


  // Framework apidocs
  // -----------------
  lazy val framework_doc = project(".", "lift-framework-doc", new DefaultProject(_) with LiftDefaultDocProject)


  private def coreProject = frameworkProject("core") _
  private def webProject = frameworkProject("web") _
  private def persistenceProject = frameworkProject("persistence") _

  private def frameworkProject(base: String)(path: String, libs: ModuleID*)(deps: Project*) =
    project(base / path, "lift-" + path, new FrameworkProject(_, libs: _*), deps: _*)

  // Webkit Project has testkit dependency in non-default scope -- needs special treatment
  // so that it doesn't fall over.
  // Ref: http://groups.google.com/group/simple-build-tool/browse_thread/thread/40d8ecafbf03f901
  private def webkitProject(path: String, libs: ModuleID*)(deps: Project*) =
    project("web" / path, "lift-" + path, new FrameworkProject(_, libs: _*) {
      // HACK: Specs needed in 'provided' scope, this will lead to duplications in testclasspath though
      override def libraryDependencies =
        super.libraryDependencies ++ Seq("org.scala-tools.testing" %% "specs" % specsVersion % "provided")

      // HACK: Manipulate deliverProjectDependencies to move testkit dependency from 'compile' (default) to 'provided' scope
      override def deliverProjectDependencies =
        testkit.projectID % "provided" :: super.deliverProjectDependencies.toList - testkit.projectID
    }, deps: _*)


  // Default base
  // ------------
  class FrameworkProject(info: ProjectInfo, libs: ModuleID*) extends DefaultProject(info) with LiftDefaultProject {

    override def libraryDependencies = super.libraryDependencies ++ libs ++ Seq(TestScope.junit)

    // FIXME: Build fails with -Xcheckinit -Xwarninit
    override def compileOptions = super.compileOptions.toList -- compileOptions("-Xcheckinit", "-Xwarninit").toList

    // System property hack for derby.log, webapptests
    override def testAction =
      super.testAction dependsOn
      task {
        System.setProperty("net.liftweb.webapptest.src.test.webapp", (testSourcePath / "webapp").absString)
        System.setProperty("apacheds.working.dir", (outputPath / "apacheds").absolutePath)
        None
      }

    // FIXME: breaks with SBT
    override def testOptions =
      // TestFilter((name: String) => name.startsWith("net.liftweb.mapper")) ::
      ExcludeTests(
        // Web tests
        "net.liftweb.webapptest.OneShot" :: "net.liftweb.webapptest.ToHeadUsages" :: "net.liftweb.http.SnippetSpec" :: Nil) ::
      super.testOptions.toList
  }

}
