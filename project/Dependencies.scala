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

  lazy val slf4jVersion = "1.7.25"

  // Compile scope:
  // Scope available in all classpath, transitive by default.
  lazy val commons_codec          = "commons-codec"              % "commons-codec"      % "1.11"
  lazy val commons_fileupload     = "commons-fileupload"         % "commons-fileupload" % "1.3.3"
  lazy val commons_httpclient     = "commons-httpclient"         % "commons-httpclient" % "3.1"
  lazy val javamail               = "javax.mail"                 % "mail"               % "1.4.7"
  lazy val jbcrypt                = "org.mindrot"                % "jbcrypt"            % "0.4"
  lazy val joda_time              = "joda-time"                  % "joda-time"          % "2.10"
  lazy val joda_convert           = "org.joda"                   % "joda-convert"       % "2.1"
  lazy val htmlparser             = "nu.validator"               % "htmlparser"         % "1.4.12"
  lazy val mongo_java_driver      = "org.mongodb"                % "mongodb-driver"     % "3.12.7"
  lazy val mongo_java_driver_async  = "org.mongodb"              % "mongodb-driver-async" % "3.12.7"
  lazy val paranamer              = "com.thoughtworks.paranamer" % "paranamer"          % "2.8"
  lazy val scalajpa               = "org.scala-libs"             % "scalajpa"           % "1.5"
  lazy val scalap: ModuleMap      = "org.scala-lang"             % "scalap"             % _
  lazy val scala_compiler: ModuleMap = "org.scala-lang"          % "scala-compiler"     % _
  lazy val scalaz7_core           = "org.scalaz"                %% "scalaz-core"        % "7.2.28"
  lazy val squeryl                = "org.squeryl"               %% "squeryl"            % "0.9.5-7"
  lazy val slf4j_api              = "org.slf4j"                  % "slf4j-api"          % slf4jVersion
  lazy val scala_xml              = "org.scala-lang.modules"     %% "scala-xml"         % "1.3.0"
  lazy val scala_parallel_collections =  "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"
  lazy val rhino                  = "org.mozilla"                % "rhino"              % "1.7.10"
  lazy val scala_parser           = "org.scala-lang.modules"     %% "scala-parser-combinators" % "1.1.2"
  lazy val xerces                 = "xerces" % "xercesImpl" % "2.11.0"

  // Aliases
  lazy val mongo_driver = mongo_java_driver
  lazy val scalaz7 = scalaz7_core


  // Provided scope:
  // Scope provided by container, available only in compile and test classpath, non-transitive by default.
  lazy val logback         = "ch.qos.logback"    % "logback-classic"       % "1.2.3"        % Provided
  lazy val log4j           = "log4j"             % "log4j"                 % "1.2.17"       % Provided
  lazy val slf4j_log4j12   = "org.slf4j"         % "slf4j-log4j12"         % slf4jVersion   % Provided
  lazy val persistence_api = "javax.persistence" % "persistence-api"       % "1.0.2"        % Provided
  lazy val servlet_api     = "javax.servlet"     % "javax.servlet-api"     % "3.1.0"        % Provided
  lazy val jquery          = "org.webjars.bower" % "jquery"                % "1.11.3"       % Provided
  lazy val jasmineCore     = "org.webjars.bower" % "jasmine-core"          % "2.4.1"        % Provided
  lazy val jasmineAjax     = "org.webjars.bower" % "jasmine-ajax"          % "3.2.0"        % Provided

  // @nowarn was removed from version 2.5.0.
  // From 2.4.0, @nowarn has problems with string argument when silencer is used.
  lazy val scala_compat    = "org.scala-lang.modules" % "scala-collection-compat" % "2.3.2" % Provided cross CrossVersion.binary


  // Test scope:
  // Scope available only in test classpath, non-transitive by default.
  lazy val jetty6     = "org.mortbay.jetty"        % "jetty"                    % "6.1.26"   % Test
  lazy val jwebunit   = "net.sourceforge.jwebunit" % "jwebunit-htmlunit-plugin" % "2.5"      % Test
  lazy val derby      = "org.apache.derby"         % "derby"                    % "10.7.1.1" % Test
  lazy val h2database = "com.h2database"           % "h2"                       % "1.2.147"  % Test

  lazy val specs2      = "org.specs2"        %% "specs2-core"          % "4.9.4"         % Test
  lazy val scalacheck  = "org.specs2"        %% "specs2-scalacheck"    % specs2.revision % Test
  lazy val specs2Prov  = "org.specs2"        %% "specs2-core"          % specs2.revision % Provided
  lazy val specs2Matchers = "org.specs2"     %% "specs2-matcher-extra" % specs2.revision % Test
  lazy val specs2MatchersProv = "org.specs2" %% "specs2-matcher-extra" % specs2.revision % Provided
  lazy val specs2Mock  = "org.specs2"        %% "specs2-mock"          % specs2.revision % Test

  lazy val scalactic       = "org.scalactic"     %% "scalactic"  % "3.1.2"   % Test
  lazy val scalatest       = "org.scalatest"     %% "scalatest"  % "3.1.2"   % Test
  lazy val scalatest_junit = "org.scalatestplus" %% "junit-4-12" % "3.1.2.0" % Test
  lazy val mockito_scalatest = "org.mockito" %% "mockito-scala-scalatest" % "1.14.3" % Test

  // Aliases
  lazy val h2 = h2database

}
