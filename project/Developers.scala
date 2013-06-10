/*
 * Copyright 2013 WorldWide Conferencing, LLC
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


object Developers {
  lazy val members = Map(
    "andreak" -> "Andreas Joseph Krogh",
    "bwmcadams" -> "Brendan W. McAdams",
    "davewhittaker" -> "Dave Whittaker",
    "davidB" -> "David Bernard",
    "dcbriccetti" -> "Dave Briccetti",
    "dchenbecker" -> "Derek Chen-Becker",
    "fmpwizard" -> "Diego Medina",
    "dpp" -> "David Pollak",
    "Dridus" -> "Ross Mellgren",
    "dlouwers" -> "Dirk Louwers",
    "eltimn" -> "Tim Nelson",
    "fbettag" -> "Franz Bettag",
    "harryh" -> "Harry Heymann",
    "hoffrocket" -> "Jon Hoffman",
    "indrajitr" -> "Indrajit Raychaudhuri",
    "jeppenejsum" -> "Jeppe Nejsum Madsen",
    "jgoday" -> "Javier Goday",
    "jonifreeman" -> "Joni Freeman",
    "jorgeortiz85" -> "Jorge Ortiz",
    "lkuczera" -> "Łukasz Kuczera",
    "mads379" -> "Mads Hartmann Jensen",
    "mariusdanciu" -> "Marius Danciu",
    "max-l" -> "Maxime Lévesque",
    "nafg" -> "Naftoli Gugenheim",
    "pr1001" -> "Peter Robinett",
    "rusho" -> "Ján Raška",
    "timperrett" -> "Timothy Perrett",
    "tjweir" -> "Tyler Weir",
    "tuhlmann" -> "Torsten Uhlmann",
    "vdichev" -> "Vassil Dichev",
    "chenkelmann" -> "Christoph Henkelmann"
  )

  def toXml =
    <developers>
      {members map { m =>
        <developer>
          <id>{m._1}</id>
          <name>{m._2}</name>
          <url>http://github.com/{m._1}</url>
        </developer>
      }}
    </developers>
}
