import sbt._

object PluginDef extends Build {
  lazy val root              = Project("plugins", file(".")) dependsOn(buildPlugin)
  lazy val buildPlugin       = uri("git://github.com/indrajitr/sbt-lift-build-plugin.git#c78f617f62")
}
