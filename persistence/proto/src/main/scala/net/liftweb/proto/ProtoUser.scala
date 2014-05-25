/*
 * Copyright 2006-2011 WorldWide Conferencing, LLC
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

package net.liftweb
package proto

import net.liftweb.http._
import js._
import JsCmds._
import scala.xml.{NodeSeq, Node, Text, Elem}
import scala.xml.transform._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import net.liftweb.util.Helpers._
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.util.Mailer._
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
    def save: Boolean

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

  /**
   * The base path for the user related URLs.  Override this
   * method to change the base path
   */
  def basePath: List[String] = "user_mgt" :: Nil

  /**
   * The path suffix for the sign up screen
   */
  def signUpSuffix: String = "sign_up"

  /**
   * The computed path for the sign up screen
   */
  lazy val signUpPath = thePath(signUpSuffix)

  /**
   * The path suffix for the login screen
   */
  def loginSuffix = "login"

  /**
   * The computed path for the login screen
   */
  lazy val loginPath = thePath(loginSuffix)

  /**
   * The path suffix for the lost password screen
   */
  def lostPasswordSuffix = "lost_password"

  /**
   * The computed path for the lost password screen
   */
  lazy val lostPasswordPath = thePath(lostPasswordSuffix)

  /**
   * The path suffix for the reset password screen
   */
  def passwordResetSuffix = "reset_password"

  /**
   * The computed path for the reset password screen
   */
  lazy val passwordResetPath = thePath(passwordResetSuffix)

  /**
   * The path suffix for the change password screen
   */
  def changePasswordSuffix = "change_password"

  /**
   * The computed path for change password screen
   */
  lazy val changePasswordPath = thePath(changePasswordSuffix)

  /**
   * The path suffix for the logout screen
   */
  def logoutSuffix = "logout"

  /**
   * The computed pat for logout
   */
  lazy val logoutPath = thePath(logoutSuffix)

  /**
   * The path suffix for the edit screen
   */
  def editSuffix = "edit"

  /**
   * The computed path for the edit screen
   */
  lazy val editPath = thePath(editSuffix)

  /**
   * The path suffix for the validate user screen
   */
  def validateUserSuffix = "validate_user"

  /**
   * The calculated path to the user validation screen
   */
  lazy val validateUserPath = thePath(validateUserSuffix)

  /**
   * The application's home page
   */
  def homePage = "/"

  /**
   * If you want to redirect a user to a different page after login,
   * put the page here
   */
  object loginRedirect extends SessionVar[Box[String]](Empty) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  /**
   * A helper class that holds menu items for the path
   */
  case class MenuItem(name: String, path: List[String],
                      loggedIn: Boolean) {
    lazy val endOfPath = path.last
    lazy val pathStr: String = path.mkString("/", "/", "")
    lazy val display = name match {
      case null | "" => false
      case _ => true
    }
  }

  /**
   * Calculate the path given a suffix by prepending the basePath to the suffix
   */
  protected def thePath(end: String): List[String] = basePath ::: List(end)

  /**
   * Return the URL of the "login" page
   */
  def loginPageURL = loginPath.mkString("/","/", "")

  /**
   * Inverted loggedIn_?
   */
  def notLoggedIn_? = !loggedIn_?

  /**
   * A Menu.LocParam to test if the user is logged in
   */
  lazy val testLogginIn = If(loggedIn_? _, S.?("must.be.logged.in")) ;

  /**
   * A Menu.LocParam to test if the user is a super user
   */
  lazy val testSuperUser = If(superUser_? _, S.?("must.be.super.user"))

  /**
   * A Menu.LocParam for testing if the user is logged in and if they're not,
   * redirect them to the login page
   */
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
    Full(Menu(Loc("Login" + menuNameSuffix, loginPath, S.?("login"), loginMenuLocParams ::: globalUserLocParams)))


  /**
   * If you want to include a LocParam (e.g. LocGroup) on all the
   * User menus, add them here
   */
  protected def globalUserLocParams: List[LocParam[Unit]] = Nil

  /**
   * The LocParams for the menu item for login.
   * Overwrite in order to add custom LocParams. Attention: Not calling super will change the default behavior!
   */
  protected def loginMenuLocParams: List[LocParam[Unit]] =
    If(notLoggedIn_? _, S.?("already.logged.in")) ::
    Template(() => wrapIt(login)) ::
    Nil

  /**
   * If you have more than 1 ProtoUser in your application, you'll need to distinguish the menu names.
   * Do so by changing the menu name suffix so that there are no name clashes
   */
  protected def menuNameSuffix: String = ""

  /**
   * The menu item for logout (make this "Empty" to disable)
   */
  def logoutMenuLoc: Box[Menu] =
    Full(Menu(Loc("Logout" + menuNameSuffix, logoutPath, S.?("logout"), logoutMenuLocParams ::: globalUserLocParams)))

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
    Full(Menu(Loc("CreateUser" + menuNameSuffix, signUpPath, S.?("sign.up"), createUserMenuLocParams ::: globalUserLocParams)))

  /**
   * The LocParams for the menu item for creating the user/sign up.
   * Overwrite in order to add custom LocParams. Attention: Not calling super will change the default behavior!
   */
  protected def createUserMenuLocParams: List[LocParam[Unit]] =
    Template(() => wrapIt(signupFunc.map(_()) openOr signup)) ::
    If(notLoggedIn_? _, S.?("logout.first")) ::
    Nil

  /**
   * The menu item for lost password (make this "Empty" to disable)
   */
  def lostPasswordMenuLoc: Box[Menu] =
    Full(Menu(Loc("LostPassword" + menuNameSuffix, lostPasswordPath, S.?("lost.password"), lostPasswordMenuLocParams ::: globalUserLocParams))) // not logged in

  /**
   * The LocParams for the menu item for lost password.
   * Overwrite in order to add custom LocParams. Attention: Not calling super will change the default behavior!
   */
  protected def lostPasswordMenuLocParams: List[LocParam[Unit]] =
    Template(() => wrapIt(lostPassword)) ::
    If(notLoggedIn_? _, S.?("logout.first")) ::
    Nil

  /**
   * The menu item for resetting the password (make this "Empty" to disable)
   */
  def resetPasswordMenuLoc: Box[Menu] =
    Full(Menu(Loc("ResetPassword" + menuNameSuffix, (passwordResetPath, true), S.?("reset.password"), resetPasswordMenuLocParams ::: globalUserLocParams))) //not Logged in

  /**
   * The LocParams for the menu item for resetting the password.
   * Overwrite in order to add custom LocParams. Attention: Not calling super will change the default behavior!
   */
  protected def resetPasswordMenuLocParams: List[LocParam[Unit]] =
    Hidden ::
    Template(() => wrapIt(passwordReset(snarfLastItem))) ::
    If(notLoggedIn_? _, S.?("logout.first")) ::
    Nil

  /**
   * The menu item for editing the user (make this "Empty" to disable)
   */
  def editUserMenuLoc: Box[Menu] =
    Full(Menu(Loc("EditUser" + menuNameSuffix, editPath, S.?("edit.user"), editUserMenuLocParams ::: globalUserLocParams)))

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
    Full(Menu(Loc("ChangePassword" + menuNameSuffix, changePasswordPath, S.?("change.password"), changePasswordMenuLocParams ::: globalUserLocParams)))

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
    Full(Menu(Loc("ValidateUser" + menuNameSuffix, (validateUserPath, true), S.?("validate.user"), validateUserMenuLocParams ::: globalUserLocParams)))

  /**
   * The LocParams for the menu item for validating a user.
   * Overwrite in order to add custom LocParams. Attention: Not calling super will change the default behavior!
   */
  protected def validateUserMenuLocParams: List[LocParam[Unit]] =
    Hidden ::
    Template(() => wrapIt(validateUser(snarfLastItem))) ::
    If(notLoggedIn_? _, S.?("logout.first")) ::
    Nil

  /**
   * An alias for the sitemap property
   */
  def menus: List[Menu] = sitemap // issue 182

  /**
   * Insert this LocParam into your menu if you want the
   * User's menu items to be inserted at the same level
   * and after the item
   */
  final case object AddUserMenusAfter extends Loc.LocParam[Any]

  /**
   * replace the menu that has this LocParam with the User's menu
   * items
   */
  final case object AddUserMenusHere extends Loc.LocParam[Any]

  /**
   * Insert this LocParam into your menu if you want the
   * User's menu items to be children of that menu
   */
  final case object AddUserMenusUnder extends Loc.LocParam[Any]

  private lazy val AfterUnapply = SiteMap.buildMenuMatcher(_ == AddUserMenusAfter)
  private lazy val HereUnapply = SiteMap.buildMenuMatcher(_ == AddUserMenusHere)
  private lazy val UnderUnapply = SiteMap.buildMenuMatcher(_ == AddUserMenusUnder)

  /**
   * The SiteMap mutator function
   */
  def sitemapMutator: SiteMap => SiteMap = SiteMap.sitemapMutator {
    case AfterUnapply(menu) => menu :: sitemap
    case HereUnapply(_) => sitemap
    case UnderUnapply(menu) => List(menu.rebuild(_ ::: sitemap))
  }(SiteMap.addMenusAtEndMutator(sitemap))

  lazy val sitemap: List[Menu] =
  List(loginMenuLoc, createUserMenuLoc,
       lostPasswordMenuLoc, resetPasswordMenuLoc,
       editUserMenuLoc, changePasswordMenuLoc,
       validateUserMenuLoc, logoutMenuLoc).flatten(a => a)


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
  List(MenuItem(S.?("sign.up"), signUpPath, false),
       MenuItem(S.?("log.in"), loginPath, false),
       MenuItem(S.?("lost.password"), lostPasswordPath, false),
       MenuItem("", passwordResetPath, false),
       MenuItem(S.?("change.password"), changePasswordPath, true),
       MenuItem(S.?("log.out"), logoutPath, true),
       MenuItem(S.?("edit.profile"), editPath, true),
       MenuItem("", validateUserPath, false))

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

  def logUserIn(who: TheUserType, postLogin: () => Nothing): Nothing = {
    if (destroySessionOnLogin) {
      S.session.openOrThrowException("we have a session here").destroySessionAndContinueInNewSession(() => {
        logUserIn(who)
        postLogin()
      })
    } else {
      logUserIn(who)
      postLogin()
    }
  }

  def logUserIn(who: TheUserType) {
    curUserId.remove()
    curUser.remove()
    curUserId(Full(who.userIdAsString))
    curUser(Full(who))
    onLogIn.foreach(_(who))
  }

  def logoutCurrentUser = logUserOut()

  def logUserOut() {
    onLogOut.foreach(_(curUser))
    curUserId.remove()
    curUser.remove()
    S.session.foreach(_.destroySession())
  }

  /**
   * There may be times when you want to be another user
   * for some stack frames.  Here's how to do it.
   */
  def doWithUser[T](u: Box[TheUserType])(f: => T): T =
    curUserId.doWith(u.map(_.userIdAsString)) {
      curUser.doWith(u) {
        f
      }
    }


  private object curUserId extends SessionVar[Box[String]](Empty) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }


  def currentUserId: Box[String] = curUserId.get

  private object curUser extends RequestVar[Box[TheUserType]](currentUserId.flatMap(userFromStringId))  with CleanRequestVarOnSessionTransition  {
    override lazy val __nameSalt = Helpers.nextFuncName
  }


  /**
   * Given a String representing the User ID, find the user
   */
  protected def userFromStringId(id: String): Box[TheUserType]

  def currentUser: Box[TheUserType] = curUser.get

  def signupXhtml(user: TheUserType) = {
    (<form method="post" action={S.uri}><table><tr><td
              colspan="2">{ S.?("sign.up") }</td></tr>
          {localForm(user, false, signupFields)}
          <tr><td>&nbsp;</td><td><input type="submit" /></td></tr>
                                        </table></form>)
  }


  def signupMailBody(user: TheUserType, validationLink: String): Elem = {
    (<html>
        <head>
          <title>{S.?("sign.up.confirmation")}</title>
        </head>
        <body>
          <p>{S.?("dear")} {user.getFirstName},
            <br/>
            <br/>
            {S.?("sign.up.validation.link")}
            <br/><a href={validationLink}>{validationLink}</a>
            <br/>
            <br/>
            {S.?("thank.you")}
          </p>
        </body>
     </html>)
  }

  def signupMailSubject = S.?("sign.up.confirmation")

  /**
   * Send validation email to the user.  The XHTML version of the mail
   * body is generated by calling signupMailBody.  You can customize the
   * mail sent to users by override generateValidationEmailBodies to
   * send non-HTML mail or alternative mail bodies.
   */
  def sendValidationEmail(user: TheUserType) {
    val resetLink = S.hostAndPath+"/"+validateUserPath.mkString("/")+
    "/"+urlEncode(user.getUniqueId())

    val email: String = user.getEmail

    val msgXml = signupMailBody(user, resetLink)

    Mailer.sendMail(From(emailFrom),Subject(signupMailSubject),
                    (To(user.getEmail) :: 
                     generateValidationEmailBodies(user, resetLink) :::
                     (bccEmail.toList.map(BCC(_)))) :_* )
  }

  /**
   * Generate the mail bodies to send with the valdiation link.
   * By default, just an HTML mail body is generated by calling signupMailBody
   * but you can send additional or alternative mail by override this method.
   */
  protected def generateValidationEmailBodies(user: TheUserType,
                                              resetLink: String):
  List[MailBodyType] = List(xmlToMailBodyType(signupMailBody(user, resetLink)))

  protected object signupFunc extends RequestVar[Box[() => NodeSeq]](Empty) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }


  /**
   * Override this method to do something else after the user signs up
   */
  protected def actionsAfterSignup(theUser: TheUserType, func: () => Nothing): Nothing = {
    theUser.setValidated(skipEmailValidation).resetUniqueId()
    theUser.save
    if (!skipEmailValidation) {
      sendValidationEmail(theUser)
      S.notice(S.?("sign.up.message"))
      func()
    } else {
      logUserIn(theUser, () => {      
        S.notice(S.?("welcome"))
        func()
      })
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

  /**
   * If there's any mutation to do to the user on creation for
   * signup, override this method and mutate the user.  This can
   * be used to pull query parameters from the request and assign
   * certain fields. . Issue #722
   *
   * @param user the user to mutate
   * @return the mutated user
   */
  protected def mutateUserOnSignup(user: TheUserType): TheUserType = user
  
  def signup = {
    val theUser: TheUserType = mutateUserOnSignup(createNewUserInstance())
    val theName = signUpPath.mkString("")

    def testSignup() {
      validateSignup(theUser) match {
        case Nil =>
          actionsAfterSignup(theUser, () => S.redirectTo(homePage))

        case xs => S.error(xs) ; signupFunc(Full(innerSignup _))
      }
    }

    def innerSignup = {
      ("type=submit" #> signupSubmitButton(S ? "sign.up", testSignup _)) apply signupXhtml(theUser)
    }

    innerSignup
  }

  def signupSubmitButton(name: String, func: () => Any = () => {}): NodeSeq = {
    standardSubmitButton(name, func)
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
      logUserIn(user, () => {
        S.notice(S.?("account.validated"))
        S.redirectTo(homePage)
      })

    case _ => S.error(S.?("invalid.validation.link")); S.redirectTo(homePage)
  }

  /**
   * How do we prompt the user for the username.  By default,
   * it's S.?("email.address"), you can can change it to something else
   */
  def userNameFieldString: String = S.?("email.address")

  /**
   * The string that's generated when the user name is not found.  By
   * default: S.?("email.address.not.found")
   */
  def userNameNotFoundString: String = S.?("email.address.not.found")

  def loginXhtml = {
    (<form method="post" action={S.uri}><table><tr><td
              colspan="2">{S.?("log.in")}</td></tr>
          <tr><td>{userNameFieldString}</td><td><input type="text" class="email" /></td></tr>
          <tr><td>{S.?("password")}</td><td><input type="password" class="password" /></td></tr>
          <tr><td><a href={lostPasswordPath.mkString("/", "/", "")}
                >{S.?("recover.password")}</a></td><td><input type="submit" /></td></tr></table>
     </form>)
  }
  
  /**
   * Given an username (probably email address), find the user
   */
  protected def findUserByUserName(username: String): Box[TheUserType]

  /**
   * Given a unique id, find the user
   */
  protected def findUserByUniqueId(id: String): Box[TheUserType]

  /**
   * By default, destroy the session on login.
   * Change this is some of the session information needs to
   * be preserved.
   */
  protected def destroySessionOnLogin = true

  /**
   * If there's any state that you want to capture pre-login
   * to be set post-login (the session is destroyed),
   * then set the state here.  Just make a function
   * that captures the state... that function will be applied
   * post login.
   */
  protected def capturePreLoginState(): () => Unit = () => {}

  def login = {
    if (S.post_?) {
      S.param("username").
      flatMap(username => findUserByUserName(username)) match {
        case Full(user) if user.validated_? &&
          user.testPassword(S.param("password")) => {
            val preLoginState = capturePreLoginState()
            val redir = loginRedirect.get match {
              case Full(url) =>
                loginRedirect(Empty)
              url
              case _ =>
                homePage
            }

            logUserIn(user, () => {
              S.notice(S.?("logged.in"))

              preLoginState()

              S.redirectTo(redir)
            })
          }

        case Full(user) if !user.validated_? =>
          S.error(S.?("account.validation.error"))

        case _ => S.error(S.?("invalid.credentials"))
      }
    }

    val bind =
      ".email" #> FocusOnLoad(<input type="text" name="username"/>) &
      ".password" #> <input type="password" name="password"/> &
      "type=submit" #> loginSubmitButton(S.?("log.in"))

    bind(loginXhtml)
  }

  def loginSubmitButton(name: String, func: () => Any = () => {}): NodeSeq = {
    standardSubmitButton(name, func)
  }

  def standardSubmitButton(name: String,  func: () => Any = () => {}) = {
    SHtml.submit(name, func)
  }

  def lostPasswordXhtml = {
    (<form method="post" action={S.uri}>
        <table><tr><td
              colspan="2">{S.?("enter.email")}</td></tr>
          <tr><td>{userNameFieldString}</td><td><input type="text" class="email" /></td></tr>
          <tr><td>&nbsp;</td><td><input type="submit" /></td></tr>
        </table>
     </form>)
  }

  def passwordResetMailBody(user: TheUserType, resetLink: String): Elem = {
    (<html>
        <head>
          <title>{S.?("reset.password.confirmation")}</title>
        </head>
        <body>
          <p>{S.?("dear")} {user.getFirstName},
            <br/>
            <br/>
            {S.?("click.reset.link")}
            <br/><a href={resetLink}>{resetLink}</a>
            <br/>
            <br/>
            {S.?("thank.you")}
          </p>
        </body>
     </html>)
  }

  /**
   * Generate the mail bodies to send with the password reset link.
   * By default, just an HTML mail body is generated by calling
   * passwordResetMailBody
   * but you can send additional or alternative mail by overriding this method.
   */
  protected def generateResetEmailBodies(user: TheUserType,
                                         resetLink: String):
  List[MailBodyType] = 
    List(xmlToMailBodyType(passwordResetMailBody(user, resetLink)))


  def passwordResetEmailSubject = S.?("reset.password.request")

  /**
   * Send password reset email to the user.  The XHTML version of the mail
   * body is generated by calling passwordResetMailBody.  You can customize the
   * mail sent to users by overriding generateResetEmailBodies to
   * send non-HTML mail or alternative mail bodies.
   */
  def sendPasswordReset(email: String) {
    findUserByUserName(email) match {
      case Full(user) if user.validated_? =>
        user.resetUniqueId().save
        val resetLink = S.hostAndPath+
        passwordResetPath.mkString("/", "/", "/")+urlEncode(user.getUniqueId())

        val email: String = user.getEmail

        Mailer.sendMail(From(emailFrom),Subject(passwordResetEmailSubject),
                        (To(user.getEmail) ::
                         generateResetEmailBodies(user, resetLink) :::
                         (bccEmail.toList.map(BCC(_)))) :_*)

        S.notice(S.?("password.reset.email.sent"))
        S.redirectTo(homePage)

      case Full(user) =>
        sendValidationEmail(user)
        S.notice(S.?("account.validation.resent"))
        S.redirectTo(homePage)

      case _ => S.error(userNameNotFoundString)
    }
  }

  def lostPassword = {
    val bind =
      ".email" #> SHtml.text("", sendPasswordReset _) &
      "type=submit" #> lostPasswordSubmitButton(S.?("send.it"))

    bind(lostPasswordXhtml)
  }

  def lostPasswordSubmitButton(name: String, func: () => Any = () => {}): NodeSeq = {
    standardSubmitButton(name, func)
  }

  def passwordResetXhtml = {
    (<form method="post" action={S.uri}>
        <table><tr><td colspan="2">{S.?("reset.your.password")}</td></tr>
          <tr><td>{S.?("enter.your.new.password")}</td><td><input type="password" /></td></tr>
          <tr><td>{S.?("repeat.your.new.password")}</td><td><input type="password" /></td></tr>
          <tr><td>&nbsp;</td><td><input type="submit" /></td></tr>
        </table>
     </form>)
  }

  def passwordReset(id: String) =
  findUserByUniqueId(id) match {
    case Full(user) =>
      def finishSet() {
        user.validate match {
          case Nil => S.notice(S.?("password.changed"))
            user.resetUniqueId().save
            logUserIn(user, () => S.redirectTo(homePage))

          case xs => S.error(xs)
        }
      }

      val bind = {
        "type=password" #> SHtml.password_*("", { p: List[String] =>
          user.setPasswordFromListString(p)
        }) &
        "type=submit" #> resetPasswordSubmitButton(S.?("set.password"), finishSet _)
      }

      bind(passwordResetXhtml)
    case _ => S.error(S.?("password.link.invalid")); S.redirectTo(homePage)
  }

  def resetPasswordSubmitButton(name: String, func: () => Any = () => {}): NodeSeq = {
    standardSubmitButton(name, func)
  }

  def changePasswordXhtml = {
    (<form method="post" action={S.uri}>
        <table><tr><td colspan="2">{S.?("change.password")}</td></tr>
          <tr><td>{S.?("old.password")}</td><td><input type="password" class="old-password" /></td></tr>
          <tr><td>{S.?("new.password")}</td><td><input type="password" class="new-password" /></td></tr>
          <tr><td>{S.?("repeat.password")}</td><td><input type="password" class="new-password" /></td></tr>
          <tr><td>&nbsp;</td><td><input type="submit" /></td></tr>
        </table>
     </form>)
  }

  def changePassword = {
    val user = currentUser.openOrThrowException("we can do this because the logged in test has happened")
    var oldPassword = ""
    var newPassword: List[String] = Nil

    def testAndSet() {
      if (!user.testPassword(Full(oldPassword))) S.error(S.?("wrong.old.password"))
      else {
        user.setPasswordFromListString(newPassword)
        user.validate match {
          case Nil => user.save; S.notice(S.?("password.changed")); S.redirectTo(homePage)
          case xs => S.error(xs)
        }
      }
    }

    val bind = {
      ".old-password" #> SHtml.password("", s => oldPassword = s) &
      ".new-password" #> SHtml.password_*("", LFuncHolder(s => newPassword = s)) &
      "type=submit" #> changePasswordSubmitButton(S.?("change"), testAndSet _)
    }

    bind(changePasswordXhtml)
  }

  def changePasswordSubmitButton(name: String, func: () => Any = () => {}): NodeSeq = {
    standardSubmitButton(name, func)
  }

  def editXhtml(user: TheUserType) = {
    (<form method="post" action={S.uri}>
        <table><tr><td colspan="2">{S.?("edit")}</td></tr>
          {localForm(user, true, editFields)}
          <tr><td>&nbsp;</td><td><input type="submit" /></td></tr>
        </table>
     </form>)
  }

  object editFunc extends RequestVar[Box[() => NodeSeq]](Empty) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }


  /**
   * If there's any mutation to do to the user on retrieval for
   * editing, override this method and mutate the user.  This can
   * be used to pull query parameters from the request and assign
   * certain fields. Issue #722
   *
   * @param user the user to mutate
   * @return the mutated user
   */
  protected def mutateUserOnEdit(user: TheUserType): TheUserType = user

  def edit = {
    val theUser: TheUserType = 
      mutateUserOnEdit(currentUser.openOrThrowException("we know we're logged in"))

    val theName = editPath.mkString("")

    def testEdit() {
      theUser.validate match {
        case Nil =>
          theUser.save
          S.notice(S.?("profile.updated"))
          S.redirectTo(homePage)

        case xs => S.error(xs) ; editFunc(Full(innerEdit _))
      }
    }

    def innerEdit = {
      ("type=submit" #> editSubmitButton(S.?("save"), testEdit _)) apply editXhtml(theUser)
    }

    innerEdit
  }

  def editSubmitButton(name: String, func: () => Any = () => {}): NodeSeq = {
    standardSubmitButton(name, func)
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

