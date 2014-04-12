import sbt._

object PluginDef extends Build {
  lazy val root              = Project("plugins", file(".")) dependsOn(buildPlugin)
  lazy val buildPlugin       = uri("git://github.com/lift/sbt-lift-build.git#724fb133beac77bbd06d3fb8ea086a1c88ee2a7d")
}
