/*
 * Copyright 2008-2010 WorldWide Conferencing, LLC
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
package openid {

import _root_.net.liftweb._
import mapper._
import http._
import js._
import JsCmds._
import sitemap.{Loc, Menu}
import common._
import util._
import Helpers._

import _root_.org.openid4java.consumer._
import _root_.org.openid4java.discovery.Identifier;


trait MetaOpenIDProtoUser[ModelType <: OpenIDProtoUser[ModelType]] extends MetaMegaProtoUser[ModelType] {
  self: ModelType =>

  private val logger = Logger(classOf[MetaOpenIDProtoUser[ModelType]])
  import logger._
  
  override def signupFields: List[FieldPointerType] =
    List(nickname, firstName, lastName, locale, timezone)

  override def fieldOrder: List[FieldPointerType] = 
    List(nickname, firstName, lastName, locale, timezone)

  /**
   * Create a new user with the specified openId
   * The default implementation tries to extract attributes from
   * the VerificationReuslt, but not all providers supply this information (most notably Yahoo)
   * 
   * Override this method to change how a new user should be created
   */
  def createNewUser(openId: String, result: Box[VerificationResult]): ModelType = {
    logger.debug("Creating new user for openId: %s".format(openId))
  
    val u = self.create.openId(openId)
    
    // Set default values
    u.nickname("change"+Helpers.randomInt(1000000000)).firstName("Unknown").
      lastName("Unknown").password(Helpers.randomString(15)).
      email(Helpers.randomInt(100000000)+"unknown@unknown.com")
    
    // Try to extract parameters from response
    result foreach {res => 
      import WellKnownAttributes._
      
      val attrs = WellKnownAttributes.attributeValues(res.getAuthResponse)
      
      attrs.get(Email) map {e => u.email(trace("Extracted email",e))}
      
      self.findAll(By(email, u.email.is)) map {existing =>
        info("Cannot register new user, email %s already exists with openId ".format(existing.email.is,existing.openId.is))
        S.error("A user with email %s already exists with a different OpenID.".format(u.email.is))
        S.redirectTo(homePage)
      }
      
      attrs.get(Nickname) map {nick => u.nickname(trace("Extracted nickname",nick))}
      
      // Try to construct first/last from fullname
      val (first, last) = attrs.get(FullName) map {full => 
        trace("Calculated first/lastname",full.trim.lastIndexOf(' ') match {
          case -1 => (Some(full), None)
          case n => (Some(full.substring(0,n)), Some(full.substring(n+1)))
        })
      } getOrElse ((None, None))
      
      attrs.get(FirstName) orElse first map {f => u.firstName(trace("Extracted firstName",f)) }
      attrs.get(LastName) orElse last map {l => u.lastName(trace("Extracted lastName",l)) }
      attrs.get(TimeZone)  map {tz => u.timezone(trace("Extracted timeZone",tz)) }
    }
    u
  }

  /**
   * Called on successfull login
   */
  def findOrCreate(openId: String, result: Box[VerificationResult]): ModelType = find(By(this.openId, openId)) match {
    case Full(u) =>
      logger.debug("Found existing user for openId:%s, user:%s".format(openId,u))
      u
    case _ =>
      val u = createNewUser(openId, result)
      logger.debug("Saving new user for openId:%s, user:%s".format(openId,u))
      u.saveMe
  }

  // no need for these menu items with OpenID
  /**
   * The menu item for creating the user/sign up (make this "Empty" to disable)
   */
  override def createUserMenuLoc: Box[Menu] = Empty

  /**
   * The menu item for lost password (make this "Empty" to disable)
   */
  override def lostPasswordMenuLoc: Box[Menu] = Empty

  /**
   * The menu item for resetting the password (make this "Empty" to disable)
   */
  override def resetPasswordMenuLoc: Box[Menu] = Empty

  /**
   * The menu item for changing password (make this "Empty" to disable)
   */
  override def changePasswordMenuLoc: Box[Menu] = Empty
  
  /**
   * The menu item for validating a user (make this "Empty" to disable)
   */
  override def validateUserMenuLoc: Box[Menu] = Empty

  override def loginXhtml =
  <form method="post" action={S.uri}>
    <table>
      <tr>
        <td colspan="2">{S.??("log.in")}</td>
      </tr>
      <tr>
        <td>OpenID</td><td><user:openid /></td>
      </tr>
      <tr>
        <td>&nbsp;</td><td><user:submit /></td>
      </tr>
    </table>
  </form>

  def openIDVendor: OpenIDVendor

  def performLogUserIn(openid: Box[Identifier], fo: Box[VerificationResult], exp: Box[Exception]): LiftResponse = {
      (openid, exp) match {
        case (Full(id), _) =>
          val user = self.findOrCreate(id.getIdentifier, fo)
          logUserIn(user)
          S.notice(S.??("Welcome ")+user.niceName)

        case (_, Full(exp)) =>
          warn("Got an exception", exp)
          S.error("Got an exception: "+exp.getMessage)


        case _ =>
          warn("Unable to login using OpenID")
          S.error("Unable to log you in: "+fo)
      }

      val redir = loginRedirect.is match {
        case Full(url) =>
          loginRedirect(Empty)
          url
        case _ =>
          homePage
      }
    
      info("OpenID login complete, redirecting to: %s".format(redir))      
      RedirectResponse(redir)
    }

  override def login = {
    if (S.post_?) {
      debug("Processing OpenID login request:"+S.request)
      S.param(openIDVendor.PostParamName).
      foreach(username => {
        logger.debug("Trying to do OpenID login for user:"+username)
        openIDVendor.loginAndRedirect(username, performLogUserIn)
      })
    }


    Helpers.bind("user", loginXhtml,
                 "openid" -> (FocusOnLoad(<input type="text" name={openIDVendor.PostParamName}/>)),
                 "submit" -> (<input type="submit" value={S.??("log.in")}/>))
  }

  private[openid] def findByNickname(str: String): List[ModelType] = findAll(By(nickname, str))
}

object ValidNickName {
  val is = """^[a-z][a-z0-9_]*$""".r
  def apply(in: String): Boolean = is.findFirstIn(in).isDefined
}
/**
 * An OpenID friendly extension to ProtoUser
 */
trait OpenIDProtoUser[T <: OpenIDProtoUser[T]] extends MegaProtoUser[T] {
  self: T =>

 override def getSingleton: MetaOpenIDProtoUser[T]

  object openId extends MappedString(this, 512) {
    override def dbIndexed_? = true
  }

  object nickname extends MappedPoliteString(this, 64) {
    override def dbIndexed_? = true

  def deDupUnderscore(in: String): String = in.indexOf("__") match {
    case -1 => in
    case pos => deDupUnderscore(in.substring(0, pos)+in.substring(pos + 1))
  }

    override def setFilter = notNull _ :: toLower _ :: trim _ ::
    deDupUnderscore _ :: super.setFilter

    private def validateNickname(str: String): List[FieldError] = {
      val others = getSingleton.findByNickname(str).
      filter(_.id.is != fieldOwner.id.is)
      others.map(u => FieldError(this, <xml:group>Duplicate nickname: {str}</xml:group>))
    }

  private def validText(str: String): List[FieldError] =
  if (ValidNickName(str)) Nil
  else List(FieldError(this,
                       <xml:group>Invalid nickname.  Must start with
                        a letter and contain only letters,
                        numbers or "_"</xml:group>))

    override def validations = validText _ :: validateNickname _ :: super.validations
  }

  override def niceName: String = nickname
}

}
}
