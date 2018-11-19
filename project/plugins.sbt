DefaultOptions.addPluginResolvers
resolvers += Resolver.typesafeRepo("releases")

addSbtPlugin("com.typesafe.sbt" % "sbt-web" % "1.4.3")
addSbtPlugin("org.jetbrains" % "sbt-idea-plugin" % "2.1.3")

lazy val buildPlugin         = RootProject(uri("git://github.com/lift/sbt-lift-build.git#f9c52bda7b43a98b9f8805c654c713d99db0a58f"))
lazy val yuiCompressorPlugin = RootProject(uri("git://github.com/indrajitr/sbt-yui-compressor.git#89304ec0c988183d1f1a889e665e0269fe513031"))

lazy val root = (project in file(".")).dependsOn(buildPlugin /*, yuiCompressorPlugin*/)
