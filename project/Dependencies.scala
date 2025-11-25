/*
 * Copyright 2011-2018 WorldWide Conferencing, LLC
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

object Dependencies {

  type ModuleMap = String => ModuleID

  lazy val slf4jVersion = "1.7.36"

  // Compile scope:
  // Scope available in all classpath, transitive by default.
  lazy val commons_codec          = "commons-codec"              % "commons-codec"      % "1.19.0"
  lazy val commons_fileupload     = "org.apache.commons"         % "commons-fileupload2-jakarta-servlet6" % "2.0.0-M4"
  lazy val commons_httpclient     = "commons-httpclient"         % "commons-httpclient" % "3.1"
  lazy val javamail               = "jakarta.mail"                 % "jakarta.mail-api"         % "2.1.5"
  lazy val jbcrypt                = "org.mindrot"                % "jbcrypt"            % "0.4"
  lazy val joda_time              = "joda-time"                  % "joda-time"          % "2.14.0"
  lazy val joda_convert           = "org.joda"                   % "joda-convert"       % "3.0.1"
  lazy val json4s_ext             = "org.json4s"                %% "json4s-ext"         % "4.0.7"
  lazy val json4s_native          = "org.json4s"                %% "json4s-native"      % "4.0.7"
  lazy val json4s_xml             = "org.json4s"                %% "json4s-xml"         % "4.0.7"
  lazy val htmlparser             = "nu.validator"               % "htmlparser"         % "1.4.16"
  lazy val mongo_java_driver      = "org.mongodb"                % "mongodb-driver"     % "3.12.7"
  lazy val mongo_java_driver_async  = "org.mongodb"              % "mongodb-driver-async" % "3.12.7"
  lazy val paranamer              = "com.thoughtworks.paranamer" % "paranamer"          % "2.8.3"
  lazy val scalajpa               = "org.scala-libs"             % "scalajpa"           % "1.5"
  lazy val scalap: ModuleMap      = "org.scala-lang"             % "scalap"             % _
  lazy val scalaz7_core           = "org.scalaz"                %% "scalaz-core"        % "7.3.8"
  lazy val slf4j_api              = "org.slf4j"                  % "slf4j-api"          % slf4jVersion
  lazy val scala_xml              = "org.scala-lang.modules"     %% "scala-xml"         % "2.4.0"
  lazy val scala_parallel_collections =  "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"
  lazy val rhino                  = "org.mozilla"                % "rhino"              % "1.7.15"
  lazy val scala_parser           = "org.scala-lang.modules"     %% "scala-parser-combinators" % "2.4.0"
  lazy val xerces                 = "xerces" % "xercesImpl" % "2.12.0"

  lazy val scala_compiler: ModuleMap = (version: String) => {
    if (version.startsWith("2")) {
      "org.scala-lang"         % "scala-compiler"    % version
    } else {
      "org.scala-lang"         % "scala3-compiler_3" % version
    }
  }

  // Aliases
  lazy val mongo_driver = mongo_java_driver
  lazy val scalaz7 = scalaz7_core


  // Provided scope:
  // Scope provided by container, available only in compile and test classpath, non-transitive by default.
  lazy val logback         = "ch.qos.logback"    % "logback-classic"       % "1.2.13"       % Provided
  lazy val log4j           = "log4j"             % "log4j"                 % "1.2.17"       % Provided
  lazy val slf4j_log4j12   = "org.slf4j"         % "slf4j-log4j12"         % slf4jVersion   % Provided
  lazy val servlet_api     = "jakarta.servlet"   % "jakarta.servlet-api"   % "6.1.0"        % Provided
  lazy val jquery          = "org.webjars.bower" % "jquery"                % "1.11.3"       % Provided
  lazy val jasmineCore     = "org.webjars.bower" % "jasmine-core"          % "2.4.1"        % Provided
  lazy val jasmineAjax     = "org.webjars.bower" % "jasmine-ajax"          % "3.2.0"        % Provided


  // Test scope:
  // Scope available only in test classpath, non-transitive by default.
  lazy val jetty11     = "org.eclipse.jetty"        % "jetty-servlet"            % "11.0.15"  % Test
  lazy val jettywebapp = "org.eclipse.jetty"        % "jetty-webapp"             % "11.0.15"  % Test
  lazy val jwebunit    = "net.sourceforge.jwebunit" % "jwebunit-htmlunit-plugin" % "2.5"      % Test
  lazy val derby       = "org.apache.derby"         % "derby"                    % "10.7.1.1" % Test
  lazy val h2database  = "com.h2database"           % "h2"                       % "1.2.147"  % Test

  // Specs2 versions differ between Scala 2 and Scala 3
  def specs2Version(scalaVersion: String): String = {
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, 13)) => "4.21.0"
      case Some((3, _))  => "5.6.4"
      case _             => "4.21.0"
    }
  }

  lazy val specs2: ModuleMap = (version: String) => "org.specs2" %% "specs2-core" % specs2Version(version) % Test
  lazy val scalacheck: ModuleMap = (version: String) => "org.specs2" %% "specs2-scalacheck" % specs2Version(version) % Test
  lazy val specs2Prov: ModuleMap = (version: String) => "org.specs2" %% "specs2-core" % specs2Version(version) % Provided
  lazy val specs2Matchers: ModuleMap = (version: String) => "org.specs2" %% "specs2-matcher-extra" % specs2Version(version) % Test
  lazy val specs2MatchersProv: ModuleMap = (version: String) => "org.specs2" %% "specs2-matcher-extra" % specs2Version(version) % Provided
  lazy val specs2Mock: ModuleMap = (version: String) => {
    CrossVersion.partialVersion(version) match {
      case Some((2, 13)) => "org.specs2" %% "specs2-mock" % specs2Version(version) % Test
      case Some((3, _))  => "org.scalatestplus" %% "mockito-5-18" % "3.2.19.0" % Test
      case _             => "org.specs2" %% "specs2-mock" % specs2Version(version) % Test
    }
  }

  lazy val scalactic       = "org.scalactic"     %% "scalactic"  % "3.2.19"   % Test
  lazy val scalatest       = "org.scalatest"     %% "scalatest"  % "3.2.19"   % Test
  lazy val scalatest_junit = "org.scalatestplus" %% "junit-4-12" % "3.1.2.0" % Test
  lazy val mockito_scalatest: ModuleMap = (version: String) => {
    CrossVersion.partialVersion(version) match {
      case Some((2, 13)) => "org.mockito" %% "mockito-scala-scalatest" % "1.14.3" % Test
      case Some((3, _))  => "org.scalatestplus" %% "mockito-5-18" % "3.2.19.0" % Test
      case _             => "org.mockito" %% "mockito-scala-scalatest" % "1.14.3" % Test
    }
  }

  lazy val scalamock = "org.scalamock" %% "scalamock" % "7.4.1" % Test

  // Aliases
  lazy val h2 = h2database

}
