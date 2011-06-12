import sbt._
class Plugins(info: ProjectInfo) extends PluginDefinition(info) {

  // Add ScalaToolsSnapshots
  lazy val snapshots = ScalaToolsSnapshots

  // Add plugin
  lazy val liftsbt = "net.liftweb" % "lift-sbt" % "2.4-M2"
  lazy val bnd4sbt = "com.weiglewilczek.bnd4sbt" % "bnd4sbt" % "1.0.2"
}
