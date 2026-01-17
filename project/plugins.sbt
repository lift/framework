DefaultOptions.addPluginResolvers
resolvers += Resolver.typesafeRepo("releases")

addSbtPlugin("com.typesafe.sbt" % "sbt-web" % "1.4.4")
//addSbtPlugin("org.jetbrains" % "sbt-idea-plugin" % "2.1.3")

lazy val buildPlugin = RootProject(uri("https://github.com/lift/sbt-lift-build.git#01af51e838d2162ebeae56505a635860392b09a6"))
lazy val root        = (project in file(".")).dependsOn(buildPlugin)

addSbtPlugin("com.github.sbt" % "sbt-unidoc" % "0.5.0")
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.6.4")
addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.3.1")
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.12.2")
