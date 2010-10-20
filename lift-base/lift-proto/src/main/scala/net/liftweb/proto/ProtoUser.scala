/*
 * Copyright 2006-2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb {
package proto {

import _root_.net.liftweb.http._
import js._
import JsCmds._
import _root_.scala.xml.{NodeSeq, Node, Text, Elem}
import _root_.scala.xml.transform._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.util.Mailer._
import S._

/**
 * A prototypical user class with abstractions to the underlying storage
 */
trait ProtoUser {
  /**
   * The underlying record for the User
   */
  type TheUserType

  /**
   * Bridges from TheUserType to methods used in this class
   */
  protected trait UserBridge {
    /**
     * Convert the user's primary key to a String
     */
    def userIdAsString: String

    /**
     * Return the user's first name
     */
    def getFirstName: String

    /**
     * Return the user's last name
     */
    def getLastName: String

    /**
     * Get the user's email
     */
    def getEmail: String

    /**
     * Is the user a superuser
     */
    def superUser_? : Boolean

    /**
     * Has the user been validated?
     */
    def validated_? : Boolean

    /**
     * Does the supplied password match the actual password?
     */
    def testPassword(toTest: Box[String]): Boolean

    /**
     * Set the validation flag on the user and return the user
     */
    def setValidated(validation: Boolean): TheUserType

    /**
     * Set the unique ID for this user to a new value
     */
    def resetUniqueId(): TheUserType

    /**
     * Return the unique ID for the user
     */
    def getUniqueId(): String

    /**
     * Validate the user
     */
    def validate: List[FieldError]

    /**
     * Given a list of string, set the password
     */
    def setPasswordFromListString(in: List[String]): TheUserType

    /**
     * Save the user to backing store
     */
    def save(): Boolean

    /**
     * Get a nice name for the user
     */
    def niceName: String = (getFirstName, getLastName, getEmail) match {
      case (f, l, e) if f.length > 1 && l.length > 1 => f+" "+l+" ("+e+")"
      case (f, _, e) if f.length > 1 => f+" ("+e+")"
      case (_, l, e) if l.length > 1 => l+" ("+e+")"
      case (_, _, e) => e
    }
    
    /**
     * Get a short name for the user
     */
    def shortName: String = (getFirstName, getLastName) match {
      case (f, l) if f.length > 1 && l.length > 1 => f+" "+l
      case (f, _) if f.length > 1 => f
      case (_, l) if l.length > 1 => l
      case _ => getEmail
    }
    
    /**
     * Get an email link
     */
    def niceNameWEmailLink = <a href={"mailto:"+urlEncode(getEmail)}>{niceName}</a>

  }

  /**
   * Convert an instance of TheUserType to the Bridge trait
   */
  protected implicit def typeToBridge(in: TheUserType): UserBridge

  /**
   * Get a nice name for the user
   */
  def niceName(inst: TheUserType): String = inst.niceName

  /**
   * Get a nice name for the user
   */
  def shortName(inst: TheUserType): String = inst.shortName

  /**
   * Get an email link for the user
   */
  def niceNameWEmailLink(inst: TheUserType): Elem = inst.niceNameWEmailLink

  /**
   * A generic representation of a field.  For example, this represents the
   * abstract "name" field and is used along with an instance of TheCrudType
   * to compute the BaseField that is the "name" field on the specific instance
   * of TheCrudType
   */
  type FieldPointerType

  /**
   * Based on a FieldPointer, build a FieldPointerBridge
   */
  protected implicit def buildFieldBridge(from: FieldPointerType): FieldPointerBridge
  
  protected trait FieldPointerBridge {
    /**
     * What is the display name of this field?
     */
    def displayHtml: NodeSeq

    /**
     * Does this represent a pointer to a Password field
     */
    def isPasswordField_? : Boolean
  }

  /**
   * The list of fields presented to the user at sign-up
   */
  def signupFields: List[FieldPointerType]


  /**
   * The list of fields presented to the user for editing
   */
  def editFields: List[FieldPointerType]

  /**
   * What template are you going to wrap the various nodes in
   */
  def screenWrap: Box[Node] = Empty

  val basePath: List[String] = "user_mgt" :: Nil
  def signUpSuffix = "sign_up"
  lazy val signUpPath = thePath(signUpSuffix)
  def loginSuffix = "login"
  lazy val loginPath = thePath(loginSuffix)
  def lostPasswordSuffix = "lost_password"
  lazy val lostPasswordPath = thePath(lostPasswordSuffix)
  def passwordResetSuffix = "reset_password"
  lazy val passwordResetPath = thePath(passwordResetSuffix)
  def changePasswordSuffix = "change_password"
  lazy val changePasswordPath = thePath(changePasswordSuffix)
  def logoutSuffix = "logout"
  lazy val logoutPath = thePath(logoutSuffix)
  def editSuffix = "edit"
  lazy val editPath = thePath(editSuffix)
  def validateUserSuffix = "validate_user"
  lazy val validateUserPath = thePath(validateUserSuffix)

  def homePage = "/"

  object loginRedirect extends SessionVar[Box[String]](Empty)



  case class MenuItem(name: String, path: List[String],
                      loggedIn: Boolean) {
    lazy val endOfPath = path.last
    lazy val pathStr: String = path.mkString("/", "/", "")
    lazy val display = name match {
      case null | "" => false
      case _ => true
    }
  }

  def thePath(end: String): List[String] = basePath ::: List(end)

  /**
   * Return the URL of the "login" page
   */
  def loginPageURL = loginPath.mkString("/","/", "")

  def notLoggedIn_? = !loggedIn_?

  lazy val testLogginIn = If(loggedIn_? _, S.??("must.be.logged.in")) ;

  lazy val testSuperUser = If(superUser_? _, S.??("must.be.super.user"))

  def loginFirst = If(
    loggedIn_? _,
    () => {
      import net.liftweb.http.{RedirectWithState, RedirectState}
      val uri = S.uriAndQueryString
      RedirectWithState(
        loginPageURL,
        RedirectState( ()=>{loginRedirect.set(uri)})
      )
    }
  )

  /**
   * Is there a user logged in and are they a superUser?
   */
  def superUser_? : Boolean = currentUser.map(_.superUser_?) openOr false

  /**
   * The menu item for login (make this "Empty" to disable)
   */
  def loginMenuLoc: Box[Menu] =
    Full(Menu(Loc("Login", loginPath, S.??("login"), loginMenuLocParams)))

  /**
   * The LocParams for the menu item for login.
   * Overwrite in order to add custom LocParams. Attention: Not calling super will change the default behavior!
   */
  protected def loginMenuLocParams: List[LocParam[Unit]] =
    If(notLoggedIn_? _, S.??("already.logged.in")) ::
    Template(() => wrapIt(login)) ::
    Nil

  /**
   * The menu item for logout (make this "Empty" to disable)
   */
  def logoutMenuLoc: Box[Menu] =
    Full(Menu(Loc("Logout", logoutPath, S.??("logout"), logoutMenuLocParams)))

  /**
   * The LocParams for the menu item for logout.
   * Overwrite in order to add custom LocParams. Attention: Not calling super will change the default behavior!
   */
  protected def logoutMenuLocParams: List[LocParam[Unit]] =
    Template(() => wrapIt(logout)) ::
    testLogginIn ::
    Nil

  /**
   * The menu item for creating the user/sign up (make this "Empty" to disable)
   */
  def createUserMenuLoc: Box[Menu] =
    Full(Menu(Loc("CreateUser", signUpPath, S.??("sign.up"), createUserMenuLocParams)))

  /**
   * The LocParams for the menu item for creating the user/sign up.
   * Overwrite in order to add custom LocParams. Attention: Not calling super will change the default behavior!
   */
  protected def createUserMenuLocParams: List[LocParam[Unit]] =
    Template(() => wrapIt(signupFunc.map(_()) openOr signup)) ::
    If(notLoggedIn_? _, S.??("logout.first")) ::
    Nil

  /**
   * The menu item for lost password (make this "Empty" to disable)
   */
  def lostPasswordMenuLoc: Box[Menu] =
    Full(Menu(Loc("LostPassword", lostPasswordPath, S.??("lost.password"), lostPasswordMenuLocParams))) // not logged in

  /**
   * The LocParams for the menu item for lost password.
   * Overwrite in order to add custom LocParams. Attention: Not calling super will change the default behavior!
   */
  protected def lostPasswordMenuLocParams: List[LocParam[Unit]] =
    Template(() => wrapIt(lostPassword)) ::
    If(notLoggedIn_? _, S.??("logout.first")) ::
    Nil

  /**
   * The menu item for resetting the password (make this "Empty" to disable)
   */
  def resetPasswordMenuLoc: Box[Menu] =
    Full(Menu(Loc("ResetPassword", (passwordResetPath, true), S.??("reset.password"), resetPasswordMenuLocParams))) //not Logged in

  /**
   * The LocParams for the menu item for resetting the password.
   * Overwrite in order to add custom LocParams. Attention: Not calling super will change the default behavior!
   */
  protected def resetPasswordMenuLocParams: List[LocParam[Unit]] =
    Hidden ::
    Template(() => wrapIt(passwordReset(snarfLastItem))) ::
    If(notLoggedIn_? _, S.??("logout.first")) ::
    Nil

  /**
   * The menu item for editing the user (make this "Empty" to disable)
   */
  def editUserMenuLoc: Box[Menu] =
    Full(Menu(Loc("EditUser", editPath, S.??("edit.user"), editUserMenuLocParams)))

  /**
   * The LocParams for the menu item for editing the user.
   * Overwrite in order to add custom LocParams. Attention: Not calling super will change the default behavior!
   */
  protected def editUserMenuLocParams: List[LocParam[Unit]] =
    Template(() => wrapIt(editFunc.map(_()) openOr edit)) ::
    testLogginIn ::
    Nil

  /**
   * The menu item for changing password (make this "Empty" to disable)
   */
  def changePasswordMenuLoc: Box[Menu] =
    Full(Menu(Loc("ChangePassword", changePasswordPath, S.??("change.password"), changePasswordMenuLocParams)))

  /**
   * The LocParams for the menu item for changing password.
   * Overwrite in order to add custom LocParams. Attention: Not calling super will change the default behavior!
   */
  protected def changePasswordMenuLocParams: List[LocParam[Unit]] =
    Template(() => wrapIt(changePassword)) ::
    testLogginIn ::
    Nil

  /**
   * The menu item for validating a user (make this "Empty" to disable)
   */
  def validateUserMenuLoc: Box[Menu] =
    Full(Menu(Loc("ValidateUser", (validateUserPath, true), S.??("validate.user"), validateUserMenuLocParams)))

  /**
   * The LocParams for the menu item for validating a user.
   * Overwrite in order to add custom LocParams. Attention: Not calling super will change the default behavior!
   */
  protected def validateUserMenuLocParams: List[LocParam[Unit]] =
    Hidden ::
    Template(() => wrapIt(validateUser(snarfLastItem))) ::
    If(notLoggedIn_? _, S.??("logout.first")) ::
    Nil

/**
* An alias for the sitemap property
*/
def menus: List[Menu] = sitemap // issue 182

  lazy val sitemap: List[Menu] =
  List(loginMenuLoc, logoutMenuLoc, createUserMenuLoc,
       lostPasswordMenuLoc, resetPasswordMenuLoc,
       editUserMenuLoc, changePasswordMenuLoc,
       validateUserMenuLoc).flatten(a => a)


  def skipEmailValidation = false

  def userMenu: List[Node] = {
    val li = loggedIn_?
    ItemList.
    filter(i => i.display && i.loggedIn == li).
    map(i => (<a href={i.pathStr}>{i.name}</a>))
  }

  protected def snarfLastItem: String =
  (for (r <- S.request) yield r.path.wholePath.last) openOr ""

  lazy val ItemList: List[MenuItem] =
  List(MenuItem(S.??("sign.up"), signUpPath, false),
       MenuItem(S.??("log.in"), loginPath, false),
       MenuItem(S.??("lost.password"), lostPasswordPath, false),
       MenuItem("", passwordResetPath, false),
       MenuItem(S.??("change.password"), changePasswordPath, true),
       MenuItem(S.??("log.out"), logoutPath, true),
       MenuItem(S.??("edit.profile"), editPath, true),
       MenuItem("", validateUserPath, false))

  // def requestLoans: List[LoanWrapper] = Nil // List(curUser)

  var onLogIn: List[TheUserType => Unit] = Nil

  var onLogOut: List[Box[TheUserType] => Unit] = Nil

  /**
   * This function is given a chance to log in a user
   * programmatically when needed
   */
  var autologinFunc: Box[()=>Unit] = Empty

  def loggedIn_? = {
    if(!currentUserId.isDefined)
      for(f <- autologinFunc) f()
    currentUserId.isDefined
  }

  def logUserIdIn(id: String) {
    curUser.remove()
    curUserId(Full(id))
  }
  def logUserIn(who: TheUserType) {
    curUser.remove()
    curUserId(Full(who.userIdAsString))
    onLogIn.foreach(_(who))
  }

  def logoutCurrentUser = logUserOut()

  def logUserOut() {
    onLogOut.foreach(_(curUser))
    curUserId.remove()
    curUser.remove()
    S.request.foreach(_.request.session.terminate)
  }

  private object curUserId extends SessionVar[Box[String]](Empty)

  def currentUserId: Box[String] = curUserId.is

  private object curUser extends RequestVar[Box[TheUserType]](currentUserId.flatMap(userFromStringId))  with CleanRequestVarOnSessionTransition

  /**
   * Given a String representing the User ID, find the user
   */
  protected def userFromStringId(id: String): Box[TheUserType]

  def currentUser: Box[TheUserType] = curUser.is

  def signupXhtml(user: TheUserType) = {
    (<form method="post" action={S.uri}><table><tr><td
              colspan="2">{ S.??("sign.up") }</td></tr>
          {localForm(user, false, signupFields)}
          <tr><td>&nbsp;</td><td><user:submit/></td></tr>
                                        </table></form>)
  }


  def signupMailBody(user: TheUserType, validationLink: String) = {
    (<html>
        <head>
          <title>{S.??("sign.up.confirmation")}</title>
        </head>
        <body>
          <p>{S.??("dear")} {user.getFirstName},
            <br/>
            <br/>
            {S.??("sign.up.validation.link")}
            <br/><a href={validationLink}>{validationLink}</a>
            <br/>
            <br/>
            {S.??("thank.you")}
          </p>
        </body>
     </html>)
  }

  def signupMailSubject = S.??("sign.up.confirmation")

  def sendValidationEmail(user: TheUserType) {
    val resetLink = S.hostAndPath+"/"+validateUserPath.mkString("/")+
    "/"+urlEncode(user.getUniqueId())

    val email: String = user.getEmail

    val msgXml = signupMailBody(user, resetLink)

    Mailer.sendMail(From(emailFrom),Subject(signupMailSubject),
                    (To(user.getEmail) :: xmlToMailBodyType(msgXml) ::
                     (bccEmail.toList.map(BCC(_)))) :_* )
  }

  protected object signupFunc extends RequestVar[Box[() => NodeSeq]](Empty)

  /**
   * Override this method to do something else after the user signs up
   */
  protected def actionsAfterSignup(theUser: TheUserType) {
    theUser.setValidated(skipEmailValidation).resetUniqueId()
    theUser.save
    if (!skipEmailValidation) {
      sendValidationEmail(theUser)
      S.notice(S.??("sign.up.message"))
    } else {
      S.notice(S.??("welcome"))
      logUserIn(theUser)
    }
  }

  /**
   * Override this method to validate the user signup (eg by adding captcha verification)
   */
  def validateSignup(user: TheUserType): List[FieldError] = user.validate

  /**
   * Create a new instance of the User
   */
  protected def createNewUserInstance(): TheUserType
  
  def signup = {
    val theUser: TheUserType = createNewUserInstance()
    val theName = signUpPath.mkString("")

    def testSignup() {
      validateSignup(theUser) match {
        case Nil =>
          actionsAfterSignup(theUser)
          S.redirectTo(homePage)

        case xs => S.error(xs) ; signupFunc(Full(innerSignup _))
      }
    }

    def innerSignup = bind("user",
                           signupXhtml(theUser),
                           "submit" -> SHtml.submit(S.??("sign.up"), testSignup _))

    innerSignup
  }

  def emailFrom = "noreply@"+S.hostName

  def bccEmail: Box[String] = Empty

  def testLoggedIn(page: String): Boolean =
  ItemList.filter(_.endOfPath == page) match {
    case x :: xs if x.loggedIn == loggedIn_? => true
    case _ => false
  }


  def validateUser(id: String): NodeSeq = findUserByUniqueId(id) match {
    case Full(user) if !user.validated_? =>
      user.setValidated(true).resetUniqueId().save
      S.notice(S.??("account.validated"))
      logUserIn(user)
      S.redirectTo(homePage)

    case _ => S.error(S.??("invalid.validation.link")); S.redirectTo(homePage)
  }

  def loginXhtml = {
    (<form method="post" action={S.uri}><table><tr><td
              colspan="2">{S.??("log.in")}</td></tr>
          <tr><td>{S.??("email.address")}</td><td><user:email /></td></tr>
          <tr><td>{S.??("password")}</td><td><user:password /></td></tr>
          <tr><td><a href={lostPasswordPath.mkString("/", "/", "")}
                >{S.??("recover.password")}</a></td><td><user:submit /></td></tr></table>
     </form>)
  }
  
  /**
   * Given an email address, find the user
   */
  protected def findUserByEmail(email: String): Box[TheUserType]

  /**
   * Given a unique id, find the user
   */
  protected def findUserByUniqueId(id: String): Box[TheUserType]

  def login = {
    if (S.post_?) {
      S.param("username").
      flatMap(username => findUserByEmail(username)) match {
        case Full(user) if user.validated_? &&
          user.testPassword(S.param("password")) =>
          S.notice(S.??("logged.in"))
          logUserIn(user)
          //S.redirectTo(homePage)
          val redir = loginRedirect.is match {
            case Full(url) =>
              loginRedirect(Empty)
              url
            case _ =>
              homePage
          }
          S.redirectTo(redir)

        case Full(user) if !user.validated_? =>
          S.error(S.??("account.validation.error"))

        case _ => S.error(S.??("invalid.credentials"))
      }
    }

    bind("user", loginXhtml,
         "email" -> (FocusOnLoad(<input type="text" name="username"/>)),
         "password" -> (<input type="password" name="password"/>),
         "submit" -> (<input type="submit" value={S.??("log.in")}/>))
  }

  def lostPasswordXhtml = {
    (<form method="post" action={S.uri}>
        <table><tr><td
              colspan="2">{S.??("enter.email")}</td></tr>
          <tr><td>{S.??("email.address")}</td><td><user:email /></td></tr>
          <tr><td>&nbsp;</td><td><user:submit /></td></tr>
        </table>
     </form>)
  }

  def passwordResetMailBody(user: TheUserType, resetLink: String) = {
    (<html>
        <head>
          <title>{S.??("reset.password.confirmation")}</title>
        </head>
        <body>
          <p>{S.??("dear")} {user.getFirstName},
            <br/>
            <br/>
            {S.??("click.reset.link")}
            <br/><a href={resetLink}>{resetLink}</a>
            <br/>
            <br/>
            {S.??("thank.you")}
          </p>
        </body>
     </html>)
  }

  def passwordResetEmailSubject = S.??("reset.password.request")

  def sendPasswordReset(email: String) {
    findUserByEmail(email) match {
      case Full(user) if user.validated_? =>
        user.resetUniqueId().save
        val resetLink = S.hostAndPath+
        passwordResetPath.mkString("/", "/", "/")+urlEncode(user.getUniqueId())

        val email: String = user.getEmail

        val msgXml = passwordResetMailBody(user, resetLink)
        Mailer.sendMail(From(emailFrom),Subject(passwordResetEmailSubject),
                        (To(user.getEmail) :: xmlToMailBodyType(msgXml) ::
                         (bccEmail.toList.map(BCC(_)))) :_*)

        S.notice(S.??("password.reset.email.sent"))
        S.redirectTo(homePage)

      case Full(user) =>
        sendValidationEmail(user)
        S.notice(S.??("account.validation.resent"))
        S.redirectTo(homePage)

      case _ => S.error(S.??("email.address.not.found"))
    }
  }

  def lostPassword = {
    bind("user", lostPasswordXhtml,
         "email" -> SHtml.text("", sendPasswordReset _),
         "submit" -> <input type="submit" value={S.??("send.it")} />)
  }

  def passwordResetXhtml = {
    (<form method="post" action={S.uri}>
        <table><tr><td colspan="2">{S.??("reset.your.password")}</td></tr>
          <tr><td>{S.??("enter.your.new.password")}</td><td><user:pwd/></td></tr>
          <tr><td>{S.??("repeat.your.new.password")}</td><td><user:pwd/></td></tr>
          <tr><td>&nbsp;</td><td><user:submit/></td></tr>
        </table>
     </form>)
  }

  def passwordReset(id: String) =
  findUserByUniqueId(id) match {
    case Full(user) =>
      def finishSet() {
        user.validate match {
          case Nil => S.notice(S.??("password.changed"))
            user.save
            logUserIn(user); S.redirectTo(homePage)

          case xs => S.error(xs)
        }
      }
      user.resetUniqueId().save

      bind("user", passwordResetXhtml,
           "pwd" -> SHtml.password_*("",(p: List[String]) =>
          user.setPasswordFromListString(p)),
           "submit" -> SHtml.submit(S.??("set.password"), finishSet _))
    case _ => S.error(S.??("password.link.invalid")); S.redirectTo(homePage)
  }

  def changePasswordXhtml = {
    (<form method="post" action={S.uri}>
        <table><tr><td colspan="2">{S.??("change.password")}</td></tr>
          <tr><td>{S.??("old.password")}</td><td><user:old_pwd /></td></tr>
          <tr><td>{S.??("new.password")}</td><td><user:new_pwd /></td></tr>
          <tr><td>{S.??("repeat.password")}</td><td><user:new_pwd /></td></tr>
          <tr><td>&nbsp;</td><td><user:submit /></td></tr>
        </table>
     </form>)
  }

  def changePassword = {
    val user = currentUser.open_! // we can do this because the logged in test has happened
    var oldPassword = ""
    var newPassword: List[String] = Nil

    def testAndSet() {
      if (!user.testPassword(Full(oldPassword))) S.error(S.??("wrong.old.password"))
      else {
        user.setPasswordFromListString(newPassword)
        user.validate match {
          case Nil => user.save; S.notice(S.??("password.changed")); S.redirectTo(homePage)
          case xs => S.error(xs)
        }
      }
    }

    bind("user", changePasswordXhtml,
         "old_pwd" -> SHtml.password("", s => oldPassword = s),
         "new_pwd" -> SHtml.password_*("", LFuncHolder(s => newPassword = s)),
         "submit" -> SHtml.submit(S.??("change"), testAndSet _))
  }

  def editXhtml(user: TheUserType) = {
    (<form method="post" action={S.uri}>
        <table><tr><td colspan="2">{S.??("edit")}</td></tr>
          {localForm(user, true, editFields)}
          <tr><td>&nbsp;</td><td><user:submit/></td></tr>
        </table>
     </form>)
  }

  object editFunc extends RequestVar[Box[() => NodeSeq]](Empty)

  def edit = {
    val theUser: TheUserType = currentUser.open_! // we know we're logged in
    val theName = editPath.mkString("")

    def testEdit() {
      theUser.validate match {
        case Nil =>
          theUser.save
          S.notice(S.??("profile.updated"))
          S.redirectTo(homePage)

        case xs => S.error(xs) ; editFunc(Full(innerEdit _))
      }
    }

    def innerEdit = bind("user", editXhtml(theUser),
                         "submit" -> SHtml.submit(S.??("edit"), testEdit _))

    innerEdit
  }

  def logout = {
    logoutCurrentUser
    S.redirectTo(homePage)
  }

  /**
   * Given an instance of TheCrudType and FieldPointerType, convert
   * that to an actual instance of a BaseField on the instance of TheCrudType
   */
  protected def computeFieldFromPointer(instance: TheUserType, pointer: FieldPointerType): Box[BaseField]



  protected def localForm(user: TheUserType, ignorePassword: Boolean, fields: List[FieldPointerType]): NodeSeq = {
    for {
      pointer <- fields
      field <- computeFieldFromPointer(user, pointer).toList
      if field.show_? && (!ignorePassword || !pointer.isPasswordField_?)
      form <- field.toForm.toList
    } yield <tr><td>{field.displayName}</td><td>{form}</td></tr>
  }

  protected def wrapIt(in: NodeSeq): NodeSeq =
  screenWrap.map(new RuleTransformer(new RewriteRule {
        override def transform(n: Node) = n match {
          case e: Elem if "bind" == e.label && "lift" == e.prefix => in
          case _ => n
        }
      })) openOr in
}

/*
trait MegaProtoUser[T <: MegaProtoUser[T]] extends ProtoUser[T] {
  self: T =>
  object uniqueId extends MappedUniqueId(this, 32) {
    override def dbIndexed_? = true
    override def writePermission_?  = true
  }

  object validated extends MappedBoolean[T](this) {
    override def defaultValue = false
    override val fieldId = Some(Text("txtValidated"))
  }

  object locale extends MappedLocale[T](this) {
    override def displayName = fieldOwner.localeDisplayName
    override val fieldId = Some(Text("txtLocale"))
  }

  object timezone extends MappedTimeZone[T](this) {
    override def displayName = fieldOwner.timezoneDisplayName
    override val fieldId = Some(Text("txtTimeZone"))
  }

  def timezoneDisplayName = ??("time.zone")

  def localeDisplayName = ??("locale")

}*/

}
}
