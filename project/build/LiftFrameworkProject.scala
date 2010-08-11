import java.net.URL
import java.util.{Calendar => C}
import java.util.jar.Attributes.Name._
import scala.xml.NodeSeq
import sbt._


/**
 * Top level project definition for Lift.
 * This is the entry point for the build definitions of the Lift sub-projects.
 */
class LiftFrameworkProject(info: ProjectInfo) extends LiftParentProject(info) {

  lazy val base         = project("lift-base",        "lift-base",        new LiftBaseProject(_))
  lazy val persistence  = project("lift-persistence", "lift-persistence", new LiftPersistenceProject(_),  base)
  lazy val modules      = project("lift-modules",     "lift-modules",     new LiftModulesProject(_),      persistence)

  /**
   *  Lift Base Components
   */
  class LiftBaseProject(info: ProjectInfo) extends LiftParentProject(info) {

    // Lift Base Subprojects
    lazy val common = project("lift-common",  "lift-common",  new LiftCommonProject(_))
    lazy val actor  = project("lift-actor",   "lift-actor",   new LiftActorProject(_),  common)
    lazy val json   = project("lift-json",    "lift-json",    new LiftJsonProject(_))
    lazy val util   = project("lift-util",    "lift-util",    new LiftUtilProject(_),   actor, json)
    lazy val webkit = project("lift-webkit",  "lift-webkit",  new LiftWebkitProject(_), util)

    class LiftCommonProject(info: ProjectInfo)  extends LiftDefaultProject(info)
    class LiftActorProject(info:  ProjectInfo)  extends LiftDefaultProject(info)
    class LiftJsonProject(info:   ProjectInfo)  extends LiftDefaultProject(info)
    class LiftUtilProject(info:   ProjectInfo)  extends LiftDefaultProject(info)
    class LiftWebkitProject(info: ProjectInfo)  extends LiftDefaultProject(info)
  }

  /**
   *  Lift Persistence Components
   */
  class LiftPersistenceProject(info: ProjectInfo) extends LiftParentProject(info) {

    // Lift Persistence Subprojects
    lazy val mapper         = project("lift-mapper",          "lift-mapper",          new LiftMapperProject(_))
    lazy val jpa            = project("lift-jpa",             "lift-jpa",             new LiftJpaProject(_))
    lazy val record         = project("lift-record",          "lift-record",          new LiftRecordProject(_),         mapper)
    lazy val couchdb        = project("lift-couchdb",         "lift-couchdb",         new LiftCouchDBProject(_),        record)
    lazy val mongodb        = project("lift-mongodb",         "lift-mongodb",         new LiftMongoDBProject(_))
    lazy val mongodbRecord  = project("lift-mongodb-record",  "lift-mongodb-record",  new LiftMongoDBRecordProject(_),  mongodb, record)

    class LiftMapperProject       (info: ProjectInfo) extends LiftDefaultProject(info)
    class LiftRecordProject       (info: ProjectInfo) extends LiftDefaultProject(info)
    class LiftJpaProject          (info: ProjectInfo) extends LiftDefaultProject(info)
    class LiftCouchDBProject      (info: ProjectInfo) extends LiftDefaultProject(info)
    class LiftMongoDBProject      (info: ProjectInfo) extends LiftDefaultProject(info)
    class LiftMongoDBRecordProject(info: ProjectInfo) extends LiftDefaultProject(info)
  }

  /**
   *  Lift Modules
   */
  class LiftModulesProject(info: ProjectInfo) extends LiftParentProject(info) {

    // Lift Modules Subprojects
    lazy val testkit      = project("lift-testkit",       "lift-testkit",       new LiftTestkitProject(_))
    lazy val wizard       = project("lift-wizard",        "lift-wizard",        new LiftWizardProject(_))
    lazy val widgets      = project("lift-widgets",       "lift-widgets",       new LiftWidgetsProject(_))
    lazy val machine      = project("lift-machine",       "lift-machine",       new LiftMachineProject(_))
    lazy val scalate      = project("lift-scalate",       "lift-scalate",       new LiftScalateProject(_))
    lazy val textile      = project("lift-textile",       "lift-textile",       new LiftTextileProject(_))
    lazy val facebook     = project("lift-facebook",      "lift-facebook",      new LiftFacebookProject(_))
    lazy val amqp         = project("lift-amqp",          "lift-amqp",          new LiftAMQPProject(_))
    lazy val xmpp         = project("lift-xmpp",          "lift-xmpp",          new LiftXMPPProject(_))
    lazy val openid       = project("lift-openid",        "lift-openid",        new LiftOpenIdProject(_))
    lazy val oauth        = project("lift-oauth",         "lift-oauth",         new LiftOauthProject(_))
    lazy val oauthMapper  = project("lift-oauth-mapper",  "lift-oauth-mapper",  new LiftOauthMapperProject(_), oauth)
    lazy val paypal       = project("lift-paypal",        "lift-paypal",        new LiftPaypalProject(_))
    lazy val jta          = project("lift-jta",           "lift-jta",           new LiftJTAProject(_))
    lazy val imaging      = project("lift-imaging",       "lift-imaging",       new LiftImagingProject(_))
    lazy val ldap         = project("lift-ldap",          "lift-ldap",          new LiftLDAPProject(_))

    class LiftTestkitProject    (info: ProjectInfo) extends LiftDefaultProject(info)
    class LiftWizardProject     (info: ProjectInfo) extends LiftDefaultProject(info)
    class LiftWidgetsProject    (info: ProjectInfo) extends LiftDefaultProject(info)
    class LiftMachineProject    (info: ProjectInfo) extends LiftDefaultProject(info)
    class LiftScalateProject    (info: ProjectInfo) extends LiftDefaultProject(info)
    class LiftTextileProject    (info: ProjectInfo) extends LiftDefaultProject(info)
    class LiftFacebookProject   (info: ProjectInfo) extends LiftDefaultProject(info)
    class LiftAMQPProject       (info: ProjectInfo) extends LiftDefaultProject(info)
    class LiftXMPPProject       (info: ProjectInfo) extends LiftDefaultProject(info)
    class LiftOpenIdProject     (info: ProjectInfo) extends LiftDefaultProject(info)
    class LiftOauthProject      (info: ProjectInfo) extends LiftDefaultProject(info)
    class LiftOauthMapperProject(info: ProjectInfo) extends LiftDefaultProject(info)
    class LiftPaypalProject     (info: ProjectInfo) extends LiftDefaultProject(info)
    class LiftJTAProject        (info: ProjectInfo) extends LiftDefaultProject(info)
    class LiftImagingProject    (info: ProjectInfo) extends LiftDefaultProject(info)
    class LiftLDAPProject       (info: ProjectInfo) extends LiftDefaultProject(info)

  }

}


/**
 * Basic trait common for all the Lift project definitions.
 * This trait would be applied to both LiftParentProject as well as LiftDefaultProject.
 */
trait BasicLiftProject extends BasicDependencyProject {

  // Custom format for java.net.URL
  implicit lazy val urlFormat = new SimpleFormat[URL] { def fromString(s: String) = new URL(s) }

  // Additional user-defined properties that optionally can be defined for the project.
  lazy val projectUrl               = propertyOptional[URL](new URL("http://www.liftweb.net"), true)
  lazy val projectInceptionyear     = propertyOptional[Int](C.getInstance().get(C.YEAR), true)
  lazy val projectOrganizationName  = propertyOptional[String](organization, true)
  lazy val projectOrganizationUrl   = propertyOptional[URL](projectUrl.value, true)

  lazy val projectLicenseName       = propertyOptional[String]("Apache License, Version 2.0", true)
  lazy val projectLicenseUrl        = propertyOptional[URL](
    if (isApacheLic) new URL("http://www.apache.org/licenses/LICENSE-2.0.txt")
    else projectUrl.value, true)
  lazy val projectLicenseDistribution = propertyOptional[String]("repo", true)

  private def isEmptyLic = projectLicenseName.value.trim.isEmpty
  private def isApacheLic = projectLicenseName.value.startsWith("Apache") // TODO: Enrich

  lazy val publishRemote = propertyOptional[Boolean](false, true)

//  override def disableCrossPaths = true

  // Add ScalaToolsSnapshots if this project is on snapshot
  override def repositories =
    if (version.toString.endsWith("-SNAPSHOT")) super.repositories + ScalaToolsSnapshots
    else super.repositories

  // Add Maven Local repository for SBT to search for
  // This is useful for resolving parent POM only; SBT does the needful of looking into (maven-local) for dependencies.
  //val mavenLocal = "Local Maven Repository" at "file://" + (Path.userHome / ".m2" / "repository").absolutePath

  // Set up publish repository (the tuple avoids SBT's ReflectiveRepositories detection)
  private lazy val snapshotPublishRepo = ("Distribution Repository for Snapshots" -> "http://nexus.scala-tools.org/content/repositories/snapshots/")
  private lazy val releasePublishRepo  = ("Distribution Repository for Releases"  -> "http://nexus.scala-tools.org/content/repositories/releases/")

  val publishTo =
    if (version.toString.endsWith("-SNAPSHOT")) snapshotPublishRepo._1 at snapshotPublishRepo._2
    else releasePublishRepo._1 at releasePublishRepo._2

  Credentials(Path.userHome / ".ivy2" / ".credentials", log)

  // Tell SBT to publish to local Maven repository unless publish.remote=true
  private lazy val localDestRepo = Resolver.file("maven-local", Path.userHome / ".m2" / "repository" asFile)
  override def defaultPublishRepository =
    if (!publishRemote.value) Some(localDestRepo)
    else super.defaultPublishRepository

  // Enrich the generated POM
  // TODO: add <url/> conditionally via pomPostProcess
  private lazy val pomBasic: NodeSeq =
    (<url>{projectUrl.value}</url>
    <inceptionYear>{projectInceptionyear.value}</inceptionYear>
    <organization>
      <name>{projectOrganizationName.value}</name>
      <url>{projectOrganizationUrl.value}</url>
    </organization>)

  private lazy val pomLic: NodeSeq =
    if (isEmptyLic) NodeSeq.Empty
    else
      (<licenses>
         <license>
           <name>{projectLicenseName.value}</name>
           <url>{projectLicenseUrl.value}</url>
           <distribution>{projectLicenseDistribution.value}</distribution>
           <comments>{name} is licensed under {projectLicenseName.value}</comments>
         </license>
       </licenses>)

  override def pomExtra = pomBasic ++ pomLic

  // Enforce Maven managedStyle [FIXME: this is necessary to publish pom during 'sbt publish', but it breaks 'sbt update' when using dependencies from pom.xml]
  // override def managedStyle = ManagedStyle.Maven

}


/**
 * ParentProject specialized for Lift.
 */
abstract class LiftParentProject(info: ProjectInfo) extends ParentProject(info) with BasicLiftProject {

  // Disable dependencies on sub-projects
  override def deliverProjectDependencies = Nil

}


/**
 * DefaultProject specialized for Lift.
 */
abstract class LiftDefaultProject(info: ProjectInfo) extends DefaultProject(info) with BasicLiftProject {

  // Compile options [TODO: add additional debug options conditionally]
  override def compileOptions =
//    super.compileOptions ++ Seq(Unchecked) ++ Seq("-Xmigration", "-encoding", "utf8").map(x => CompileOption(x))
    Seq(Unchecked) ++ Seq("-Xmigration", "-encoding", "utf8").map(x => CompileOption(x))

  // Test options
  // override def testOptions = super.testOptions ++ TODO

  // Package options
  lazy val extraSpecificationEntries = ManifestAttributes(
    (SPECIFICATION_TITLE.toString,    name),
    (SPECIFICATION_VERSION.toString,  version.toString),
    (SPECIFICATION_VENDOR.toString,   projectOrganizationName.value))

  lazy val extraImplementationEntries = ManifestAttributes(
    (IMPLEMENTATION_TITLE.toString,     name),
    (IMPLEMENTATION_VERSION.toString,   version.toString),
    (IMPLEMENTATION_VENDOR_ID.toString, organization),
    (IMPLEMENTATION_VENDOR.toString,    projectOrganizationName.value),
    (IMPLEMENTATION_URL.toString,       projectUrl.value.toString))

  override def packageOptions =
    super.packageOptions ++ Seq(extraSpecificationEntries, extraImplementationEntries)

  // Document options
  private lazy val docBottom =
    "Copyright (c) " + projectInceptionyear.value + "-" + C.getInstance().get(C.YEAR) + " " +
    projectOrganizationName.value + ". All Rights Reserved."

  // TODO: comply with scaladoc for 2.8
//  override def documentOptions =
//    super.documentOptions ++ Seq(LinkSource, documentBottom(docBottom), documentCharset("utf8"))

  // Make `package` depend on `test`
  override def packageAction = super.packageAction dependsOn testAction

  // Publish sources [FIXME: this breaks 'sbt update' when using dependencies from pom.xml]
  // override def packageSrcJar = defaultJarPath("-sources.jar")
  // lazy val sourceArtifact = Artifact.sources(artifactID)
  // override def packageToPublishActions = super.packageToPublishActions ++ Seq(packageSrc)

}
