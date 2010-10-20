/*
 * Copyright 2010 WorldWide Conferencing, LLC
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
package ldap {

import javax.naming.directory.{Attributes}
import scala.util.matching.{Regex}
import scala.xml.{Elem, NodeSeq}
import net.liftweb.http.{LiftResponse, RedirectResponse, S, SessionVar}
import net.liftweb.http.js.{JsCmds}
import net.liftweb.mapper.{BaseOwnedMappedField,
                           MappedString,
                           MetaMegaProtoUser,
                           MegaProtoUser}
import net.liftweb.sitemap.{Menu}
import net.liftweb.util.{Helpers}
import net.liftweb.common.{Box, Empty, Full}

import Helpers._

import scala.util.matching.{Regex}
import scala.xml.{Elem, NodeSeq}

trait MetaLDAPProtoUser[ModelType <: LDAPProtoUser[ModelType]] extends MetaMegaProtoUser[ModelType] {
    self: ModelType =>

    override def signupFields: List[FieldPointerType] = uid ::
        cn :: dn :: Nil

    override def fieldOrder: List[FieldPointerType] = uid ::
        cn :: dn :: Nil

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

    override def editUserMenuLoc: Box[Menu] = Empty

    /**
     * User search sentence
     */
    def ldapUserSearch: String = "(uid=%s)"

    /**
     * Error messages
     */
    def loginErrorMessage: String = "Unable to login with : %s"

  def commonNameAttributeName = "cn"
  def uidAttributeName = "uid"

    override def loginXhtml : Elem = {
        <form method="post" action={S.uri}>
            <table>
                <tr>
                    <td colspan="2">{S.??("log.in")}</td>
                </tr>
                <tr>
                    <td>Username</td><td><user:name /></td>
                </tr>
                <tr>
                    <td>Password</td><td><user:password /></td>
                </tr>
                <tr>
                    <td>&nbsp;</td><td><user:submit /></td>
                </tr>
            </table>
        </form>
    }

    def ldapVendor: SimpleLDAPVendor = SimpleLDAPVendor

    override def login : NodeSeq = {
        if (S.post_?) {
            if (!ldapLogin(S.param("username").openOr(""),
                           S.param("password").openOr("")))
                S.error(loginErrorMessage.format(S.param("username").openOr("")))
        }

        Helpers.bind("user", loginXhtml,
                    "name" -> (JsCmds.FocusOnLoad(<input type="text" name="username"/>)),
                    "password" -> (JsCmds.FocusOnLoad(<input type="password" name="password"/>)),
                    "submit" -> (<input type="submit" value={S.??("log.in")}/>))
    }

    def ldapLogin(username: String, password: String): Boolean = {
        def _getUserAttributes(dn: String) = ldapVendor.attributesFromDn(dn)

        val users = ldapVendor.search(ldapUserSearch.format(username))

        if (users.size >= 1) {
            val userDn = users(0)
            if (ldapVendor.bindUser(userDn, password)) {
                val completeDn = userDn + "," + ldapVendor.parameters().get("ldap.base").getOrElse("")
                logUserIn(this)

                bindAttributes(_getUserAttributes(completeDn))

                setRoles(completeDn, ldapVendor)
                S.redirectTo(homePage)
            }
            else return false
        }
        else return false

        return true
    }

    def bindAttributes(attrs: Attributes) = {
        for {
            theCn <- Box !! attrs.get(commonNameAttributeName).get
            theUid <- Box !! attrs.get(uidAttributeName).get
        }
        {
            cn(theCn.toString)
            uid(theUid.toString)
        }
    }
}

trait LDAPProtoUser[T <: LDAPProtoUser[T]] extends MegaProtoUser[T] {
    self: T =>
    /**
     * User Roles LDAP search filter
     */
    def rolesSearchFilter: String = "(&(objectclass=groupofnames)(member=%s))"

    /**
     * Regular expression to get user roles names
     */
    def rolesNameRegex = ".*cn=(.[^,]*),ou=.*"

    object ldapRoles extends SessionVar[List[String]](List())

    override def getSingleton: MetaLDAPProtoUser[T]

    object uid extends MappedString(this, 64) {
        override def dbIndexed_? = true
    }

    object dn extends MappedString(this, 64) {
        override def dbIndexed_? = true
    }

    object cn extends MappedString(this, 64) {
        override def dbIndexed_? = true
    }

    def getRoles: List[String] = {
        return ldapRoles.get
    }

    def setRoles(userDn: String, ldapVendor: LDAPVendor) {
        def getGroupNameFromDn(dn: String): String = {
            val regex = new Regex(rolesNameRegex)

            val regex(groupName) = dn
            return groupName
        }

        // Search for user roles
        val filter = rolesSearchFilter.format(userDn)

        val groups = ldapVendor.search(filter)
        groups foreach { g => ldapRoles.set(ldapRoles.get :+ getGroupNameFromDn(g)) }
    }
}

}
}
