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

import common.Full

import _root_.org.specs._
import _root_.org.specs.runner.JUnit3
import _root_.org.specs.runner.ConsoleRunner

import javax.naming.CommunicationException

import org.apache.mina.util.AvailablePortFinder
import org.apache.directory.server.core.DefaultDirectoryService
import org.apache.directory.server.core.partition.impl.btree.jdbm.{JdbmIndex,JdbmPartition}
import org.apache.directory.server.ldap.LdapServer
import org.apache.directory.server.protocol.shared.transport.TcpTransport
import org.apache.directory.server.xdbm.Index
import org.apache.directory.server.core.entry.ServerEntry
import org.apache.directory.shared.ldap.name.LdapDN

class LdapSpecsAsTest extends JUnit3(LdapSpecs)
object LdapSpecsRunner extends ConsoleRunner(LdapSpecs)

object LdapSpecs extends Specification {
  val ROOT_DN = "dc=ldap,dc=liftweb,dc=net"

  // Thanks to Francois Armand for pointing this utility out!
  val service_port = AvailablePortFinder.getNextAvailable(40000)
  val service = new DefaultDirectoryService
  val ldap = new LdapServer


  /*
   * The following is taken from:
   * http://directory.apache.org/apacheds/1.5/41-embedding-apacheds-into-an-application.html
   * http://stackoverflow.com/questions/1560230/running-apache-ds-embedded-in-my-application
   */
  doBeforeSpec {
    try {
      // Disable changelog
      service.getChangeLog.setEnabled(false)

      // Set up a partition
      val partition = new JdbmPartition
      partition.setId("lift-ldap")
      partition.setSuffix(ROOT_DN)
      service.addPartition(partition)

      // Index attributes (gnarly type due to poor type inferencing)
      val indices : java.util.Set[Index[_,ServerEntry]] = new java.util.HashSet()

      List("objectClass", "ou", "uid", "sn").foreach {
        attr : String => indices.add(new JdbmIndex(attr))
      }

      partition.setIndexedAttributes(indices)

      // Set up the transport to use our "available" port
      ldap.setTransports(new TcpTransport(service_port))
      ldap.setDirectoryService(service)

      service.startup()

      // Inject the root entry if it does not already exist
      if ( !service.getAdminSession().exists(partition.getSuffixDn)) {
        println("Adding root entry")
        val rootEntry = service.newEntry(new LdapDN(ROOT_DN))
        rootEntry.add( "objectClass", "top", "domain", "extensibleObject" );
        rootEntry.add( "dc", "ldap" );
        service.getAdminSession().add( rootEntry );
      }

      addTestData()

      ldap.start()

      println("Started LDAP server on port " + service_port)
    } catch {
      case e => e.printStackTrace
    }
  }

  "LDAPVendor" should {
    shareVariables()

    object myLdap extends LDAPVendor

    myLdap.configure(Map("ldap.url" -> "ldap://localhost:%d/".format(service_port),
                         "ldap.base" -> "dc=ldap,dc=liftweb,dc=net"))

    "handle simple lookups" in {
      myLdap.search("objectClass=person") must_== List("cn=Test User")
    }

    "handle simple authentication" in {
      myLdap.bindUser("cn=Test User", "letmein")
    }

    "attempt reconnects" in {
      object badLdap extends LDAPVendor
      badLdap.configure()

      // Make sure that we use a port where LDAP won't live
      badLdap.ldapUrl.doWith("ldap://localhost:2") {
        // Let's not make this spec *too* slow
        badLdap.retryInterval.doWith(1000) {
          badLdap.search("objectClass=person") must throwA[CommunicationException]
        }
      }
    }
  }


  doAfterSpec {
    ldap.stop()
    service.shutdown()
    println("Stopped server")
  }

  def addTestData() {
    val username = new LdapDN("cn=Test User," + ROOT_DN)
    if (! service.getAdminSession().exists(username)) {
      println("Adding test user")
      // Add a test user. This will be used for searching and binding
      val entry = service.newEntry(username)
      entry.add("objectClass", "person", "organizationalPerson")
      entry.add("cn", "Test User")
      entry.add("sn", "User")
      entry.add("userpassword", "letmein")
      service.getAdminSession.add(entry)
    }
  }
}


}} // Close nested packages
