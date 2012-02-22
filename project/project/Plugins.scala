import sbt._

object PluginDef extends Build {
  lazy val root              = Project("plugins", file(".")) dependsOn(buildPlugin, gpgPlugin)
  lazy val buildPlugin       = uri("git://github.com/indrajitr/sbt-lift-build-plugin")
  lazy val gpgPlugin         = uri("git://github.com/sbt/xsbt-gpg-plugin")
}
