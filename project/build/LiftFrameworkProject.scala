import java.net.URL
import java.util.Calendar
import java.util.jar.Attributes
import scala.xml.NodeSeq
import sbt._

class LiftFrameworkProject(info: ProjectInfo) extends ParentProject(info) with BasicLiftProject {

  lazy val base         = project("lift-base",        "lift-base",        new LiftBaseProject(_))
  lazy val persistence  = project("lift-persistence", "lift-persistence", new LiftPersistenceProject(_),  base)
  lazy val modules      = project("lift-modules",     "lift-modules",     new LiftModulesProject(_),      base, persistence)

  /**
   *  Lift Base Components
   */
  class LiftBaseProject(info: ProjectInfo) extends ParentProject(info) with BasicLiftProject {

    // Lift Base Subprojects
    lazy val common = project("lift-common",  "lift-common",  new LiftCommonProject(_))
    lazy val actor  = project("lift-actor",   "lift-actor",   new LiftActorProject(_),  common)
    lazy val json   = project("lift-json",    "lift-json",    new LiftJsonProject(_),   common)
    lazy val util   = project("lift-util",    "lift-util",    new LiftUtilProject(_),   actor)
    lazy val webkit = project("lift-webkit",  "lift-webkit",  new LiftWebkitProject(_), json, util)

    class LiftCommonProject(info: ProjectInfo)  extends LiftDefaultProject(info)
    class LiftActorProject(info:  ProjectInfo)  extends LiftDefaultProject(info)
    class LiftJsonProject(info:   ProjectInfo)  extends LiftDefaultProject(info)
    class LiftUtilProject(info:   ProjectInfo)  extends LiftDefaultProject(info)
    class LiftWebkitProject(info: ProjectInfo)  extends LiftDefaultProject(info)
  }

  /**
   *  Lift Persistence Components
   */
  class LiftPersistenceProject(info: ProjectInfo) extends ParentProject(info) with BasicLiftProject {

    // Lift Persistence Subprojects
    lazy val mapper         = project("lift-mapper",          "lift-mapper",          new LiftMapperProject(_))
    lazy val jpa            = project("lift-jpa",             "lift-jpa",             new LiftJpaProject(_),            mapper)
    lazy val record         = project("lift-record",          "lift-record",          new LiftRecordProject(_),         mapper)
    lazy val couchdb        = project("lift-couchdb",         "lift-couchdb",         new LiftCouchDBProject(_),        record)
    lazy val mongodb        = project("lift-mongodb",         "lift-mongodb",         new LiftMongoDBProject(_),        record)
    lazy val mongodbRecord  = project("lift-mongodb-record",  "lift-mongodb-record",  new LiftMongoDBRecordProject(_),  mongodb)

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
  class LiftModulesProject(info: ProjectInfo) extends ParentProject(info) with BasicLiftProject {

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
    lazy val oauthMapper  = project("lift-oauth-mapper",  "lift-oauth-mapper",  new LiftOauthMapperProject(_))
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
 * This trait applies to both ParentProject as well as DefaultProject.
 */
trait BasicLiftProject extends BasicManagedProject {

  // Custom format for java.net.URL
  implicit lazy val urlFormat = new SimpleFormat[URL] { def fromString(s: String) = new URL(s) }

  // Additional user-defined properties that optionally can be defined for the project.
  lazy val projectUrl               = propertyOptional[URL](new URL("http://www.liftweb.net"), true)
  lazy val projectInceptionyear     = propertyOptional[Int](Calendar.getInstance().get(Calendar.YEAR), true)
  lazy val projectOrganizationName  = propertyOptional[String](organization, true)
  lazy val projectOrganizationUrl   = propertyOptional[URL](projectUrl.value, true)

  lazy val projectLicenseName       = propertyOptional[String]("Apache License, Version 2.0", true)
  lazy val projectLicenseUrl        = propertyOptional[URL](
    if (isApacheLic) new URL("http://www.apache.org/licenses/LICENSE-2.0.txt")
    else projectUrl.value, true)
  lazy val projectLicenseDistribution = propertyOptional[String]("repo", true)

  private def isEmptyLic = projectLicenseName.value.trim.isEmpty
  private def isApacheLic = projectLicenseName.value.startsWith("Apache") // TODO: Enrich

//  override def disableCrossPaths = true

  // Add ScalaToolsSnapshots if this project is on snapshot
  //  val snapshots = ScalaToolsSnapshots
  override def repositories = version.toString match {
    case s if s.endsWith("-SNAPSHOT") => super.repositories + ScalaToolsSnapshots
    case _ => super.repositories
  }

  // Add Maven Local repository for SBT to search for
  val mavenLocal = "Local Maven Repository" at "file://"+Path.userHome+"/.m2/repository"

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

}

/**
 * DefaultProject specialized for Lift.
 */
abstract class LiftDefaultProject(info: ProjectInfo) extends DefaultProject(info) with BasicLiftProject {

  // Compile options
  override def compileOptions =
    super.compileOptions ++ Seq(Unchecked) ++ Seq("-Xcheckinit", "-Xmigration", "-encoding", "utf8").map(x => CompileOption(x))

  // Test options
  // override def testOptions = super.testOptions ++ TODO

  // Package options
  lazy val extraSpecificationEntries = ManifestAttributes(
    (Attributes.Name.SPECIFICATION_TITLE.toString,    name),
    (Attributes.Name.SPECIFICATION_VERSION.toString,  version.toString),
    (Attributes.Name.SPECIFICATION_VENDOR.toString,   projectOrganizationName.value))

  lazy val extraImplementationEntries = ManifestAttributes(
    (Attributes.Name.IMPLEMENTATION_TITLE.toString,     name),
    (Attributes.Name.IMPLEMENTATION_VERSION.toString,   version.toString),
    (Attributes.Name.IMPLEMENTATION_VENDOR_ID.toString, organization),
    (Attributes.Name.IMPLEMENTATION_VENDOR.toString,    projectOrganizationName.value),
    (Attributes.Name.IMPLEMENTATION_URL.toString,       projectUrl.value.toString))

  override def packageOptions =
    super.packageOptions ++ Seq(extraSpecificationEntries, extraImplementationEntries)

  // Document options
  private lazy val docBottom =
    "Copyright (c) " + projectInceptionyear.value + "-" + Calendar.getInstance().get(Calendar.YEAR) + " " +
    projectOrganizationName.value + ". All Rights Reserved."

  override def documentOptions =
    super.documentOptions ++ Seq(LinkSource, documentBottom(docBottom), documentCharset("utf8"))

}
