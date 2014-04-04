import sbt._

object PluginDef extends Build {
  lazy val root              = Project("plugins", file(".")) dependsOn(buildPlugin, yuiPlugin)
  lazy val buildPlugin       = uri("git://github.com/nafg/sbt-lift-build.git#master")
  lazy val yuiPlugin = uri("git://github.com/nafg/sbt-yui-compressor#patch-1")
}
