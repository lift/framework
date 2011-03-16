import sbt._
class Plugins(info: ProjectInfo) extends PluginDefinition(info) {

  // Add ScalaToolsSnapshots
  lazy val snapshots = ScalaToolsSnapshots

  // Add plugin
  lazy val a = "net.liftweb" % "lift-sbt" % "2.3-RC1"
}
