import sbt._
class Plugins(info: ProjectInfo) extends PluginDefinition(info) {

  // Add ScalaToolsSnapshots
  lazy val snapshots = ScalaToolsSnapshots

  // Add plugin
  lazy val a = "net.liftweb" % "lift-sbt" % "2.3"
  
  val sbtIdeaRepo = "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"
  val sbtIdea = "com.github.mpeltonen" % "sbt-idea-plugin" % "0.4.0"
}
