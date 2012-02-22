import Dependencies._

version in ThisBuild               := "2.5-SNAPSHOT"

crossScalaVersions in ThisBuild    := Seq("2.9.1", "2.9.0-1", "2.9.0", /*"2.8.2", */"2.8.1", "2.8.0")

libraryDependencies in ThisBuild <++= scalaVersion { sv => Seq(/*specs2, */specs(sv), scalacheck(sv)) }

pomExtra in ThisBuild              ~= { _ ++ (
                                        <scm>
                                          <url>http://github.com/lift/framework</url>
                                          <connection>scm:git:git@github.com:lift/framework.git</connection>
                                        </scm>
                                        <developers>
                                          <developer>
                                            <id>indrajitr</id>
                                            <name>Indrajit Raychaudhuri</name>
                                          </developer>
                                        </developers>)}
