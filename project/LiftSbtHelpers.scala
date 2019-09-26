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

object LiftSbtHelpers {
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
      .settings(scalacOptions ++= List("-feature", "-language:implicitConversions", "-deprecation"))
      .settings(
        autoAPIMappings := true,
        apiMappings ++= {
          val cp: Seq[Attributed[File]] = (fullClasspath in Compile).value

          findManagedDependency(cp, "org.scala-lang.modules", "scala-xml").map {
            case (revision, file)  =>
              (file -> url("https://scala.github.io/scala-xml/api/" + revision))
          }.toMap
        }
      )
  }


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
}
