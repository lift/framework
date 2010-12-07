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

import java.io.{InputStream, FileInputStream}
import java.util.{Hashtable, Properties}

import javax.naming.{AuthenticationException,CommunicationException,Context,NamingException}
import javax.naming.directory.{Attributes, BasicAttributes, SearchControls}
import javax.naming.ldap.{InitialLdapContext,LdapName}

import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._

import _root_.net.liftweb.util.{ControlHelpers,Props,SimpleInjector,ThreadGlobal}
import _root_.net.liftweb.common.{Box,Empty,Full,Loggable}

/**
 * A simple extension to LDAPVendor to provide configuration
 * methods. The class, parameters* methods and variable are now
 * deprecated in favor of the configure methods on LDAPVendor.
 * See LDAPVendor for more details.
 *
 * @see LDAPVendor
 */
@deprecated("Instantiate directly from LDAPVendor")
object SimpleLDAPVendor extends LDAPVendor {
  @deprecated("Use the configure(filename : String) method")
  def parametersFromFile(filename: String) : Map[String, String] = {
    val input = new FileInputStream(filename)
    val params = parametersFromStream(input)
    input.close()
    params
  }

  @deprecated("Use the configure(stream : InputStream method")
  def parametersFromStream(stream: InputStream) : Map[String, String] = {
    val p = new Properties()
    p.load(stream)
    
    propertiesToMap(p)
  }

  @deprecated("Use the configure() method")
  def setupFromBoot = configure()
}

/**
 * This class provides functionality to allow us to search and
 * bind (authenticate) a username from a ldap server.
 *
 * To configure the LDAP Vendor parameters, use one of the configure
 * methods to provide a Map of string parameters.
 *
 * The primary parameters (with defaults) are:
 * <ul>
 *  <li>ldap.url - The LDAP Server url : "ldap://localhost"</li>
 *  <li>ldap.base - The base DN from the LDAP Server : ""</li>
 *  <li>ldap.userName - The LDAP user dn to perform search operations : ""</li>
 *  <li>ldap.password - The LDAP user password : ""</li>
 *  <li>ldap.authType - The schema to use for authentication : "simple"</li>
 *  <li>ldap.initial_context_factory - the factory class to use for
 *      initial contexts : "com.sun.jndi.ldap.LdapCtxFactory"</li>
 *  <li>
 * </ul>
 *
 * Optionally, you can set the following parameters to control context testing
 * and reconnect attempts:
 *
 * <ul>
 *   <li>lift-ldap.testLookup - A DN to attempt to look up to validate the
 *       current context. Defaults to no testing</li>
 *   <li>lift-ldap.retryInterval - How many milliseconds to wait between connection
 *       attempts due to communications failures. Defaults to 5000</li>
 *   <li>lift-ldap.maxRetries - The maxiumum number of attempts to make to set up
 *       the context before aborting. Defaults to 6</li>
 * </ul>
 *
 * In addition to configuration via a Map or Properties file, fine-grained control
 * over behaviors can be specified via Inject values corresponding to each
 * of the properties.
 *
 * To use LDAPVendor, you can simply create an object extending it
 * and configure:
 *
 * <pre id="code" class="scala">
 * object myLdap extends LDAPVendor
 * myLdap.configure()
 * </pre>
 *
 */
class LDAPVendor extends Loggable with SimpleInjector {
  // =========== Constants ===============
  final val KEY_URL = "ldap.url"
  final val KEY_BASE_DN = "ldap.base"
  final val KEY_USER = "ldap.userName"
  final val KEY_PASSWORD = "ldap.password"
  final val KEY_AUTHTYPE = "ldap.authType"
  final val KEY_FACTORY = "ldap.initial_context_factory"
  final val KEY_LOOKUP = "lift-ldap.testLookup"
  final val KEY_RETRY_INTERVAL = "lift-ldap.retryInterval"
  final val KEY_MAX_RETRIES = "lift-ldap.maxRetries"

  final val DEFAULT_URL = "ldap://localhost"
  final val DEFAULT_BASE_DN = ""
  final val DEFAULT_USER = ""
  final val DEFAULT_PASSWORD = ""
  final val DEFAULT_AUTHTYPE = "simple"
  final val DEFAULT_FACTORY = "com.sun.jndi.ldap.LdapCtxFactory"
  final val DEFAULT_LOOKUP = Empty
  final val DEFAULT_RETRY_INTERVAL = 5000
  final val DEFAULT_MAX_RETRIES = 6

  // =========== Configuration ===========
  @deprecated("Use the configure(...) methods")
  def parameters : () => Map[String,String] =
    if (internal_config.isEmpty) {
      () => null
    } else {
      () => internal_config
    }

  @deprecated("Use the configure(...) methods")
  def parameters_= (newParams : () => Map[String,String]) {
    internal_config = processConfig(newParams())
  }

  /**
   * Configure straight from the Props object. This allows
   * you to use Lift's run modes for different LDAP configuration.
   */
  def configure() {
    configure(Props.props)
  }

  /**
   * Configure from the given file. The file is expected
   * to be in a format parseable by java.util.Properties
   */
  def configure(filename : String) {
    val stream = new FileInputStream(filename)
    configure(stream)
    stream.close()
  }

  /**
   * Configure from the given input stream. The stream is expected
   * to be in a format parseable by java.util.Properties
   */
  def configure(stream : InputStream) {
    val p = new Properties()
    p.load(stream)
    
    configure(propertiesToMap(p))
  }    

  /**
   * Configure from the given Map[String,String]
   */
  def configure(props : Map[String,String]) {
    internal_config = processConfig(props)
  }

  /**
   * This controls the URL used to connect to the LDAP
   * server
   */
  val ldapUrl = new Inject[String](DEFAULT_URL){}

  /**
   * This controls the base DN used for searcheds
   */
  val ldapBaseDn = new Inject[String](DEFAULT_BASE_DN){}

  /**
   * This controls the username used to bind for
   * searches (not authentication)
   */
  val ldapUser = new Inject[String](DEFAULT_USER){}

  /**
   * This controls the password used to bind for
   * searches (not authentication)
   */
  val ldapPassword = new Inject[String](DEFAULT_PASSWORD){}

  /**
   * This controls the type of authentication to
   * use.
   */
  val ldapAuthType = new Inject[String](DEFAULT_AUTHTYPE){}

  /**
   * This controls the factory used to obtain an
   * InitialContext
   */
  val ldapFactory = new Inject[String](DEFAULT_FACTORY){}

  /**
   * This can be set to test the InitialContext on each LDAP
   * operation. It should be set to a search DN.
   */
  val testLookup = new Inject[Box[String]](Empty){}

  /**
   * This sets the interval between connection attempts
   * on the InitialContext. The default is 5 seconds
   */
  val retryInterval = new Inject[Long](5000){}

  /**
   * This sets the maximum number of connection
   * attempts before giving up. The default is 6
   */
  val retryMaxCount = new Inject[Int](6){}

  /**
   * This sets the Directory SearchControls instance
   * that is used to refine searches on the provider.
   */
  val searchControls = new Inject[SearchControls](defaultSearchControls){}
  
  /**
   * The default SearchControls to use: search the
   * base DN with a sub-tree scope, and return the
   * "cn" attribute.
   */
  def defaultSearchControls() : SearchControls = {
    val constraints = new SearchControls()
    constraints.setSearchScope(SearchControls.SUBTREE_SCOPE)
    constraints.setReturningAttributes(Array("cn"))
    return constraints
  }

  /**
   * The configuration to use for connecting to the
   * provider. It should be set via the configure methods
   */
  private var internal_config : Map[String,String] = Map.empty

  /**
   * The configuration to use for connecting to the
   * provider. It should be set via the configure methods
   */
  def configuration = internal_config

  /**
   * This method checks the configuration and sets defaults for any
   * properties that are required. It also processes any of the
   * optional configuration propertes related to context testing
   * and retries.
   *
   * This method is intended to be called during update of the default
   * configuration, not during granular override of the config.
   */
  def processConfig(input : Map[String,String]) : Map[String,String] = {
    var currentConfig = input

    def setIfEmpty(name : String, newVal : String) = 
      if (currentConfig.get(name).isEmpty) {
        currentConfig += (name -> newVal)
      }

    // Verify the minimum config
    setIfEmpty(KEY_URL, DEFAULT_URL)
    setIfEmpty(KEY_BASE_DN, DEFAULT_BASE_DN)
    setIfEmpty(KEY_USER, DEFAULT_USER)
    setIfEmpty(KEY_PASSWORD, DEFAULT_PASSWORD)
    setIfEmpty(KEY_AUTHTYPE, DEFAULT_AUTHTYPE)
    setIfEmpty(KEY_FACTORY, DEFAULT_FACTORY)

    // Set individual properties
    ldapUrl.default.set(currentConfig(KEY_URL))
    ldapBaseDn.default.set(currentConfig(KEY_BASE_DN))
    ldapUser.default.set(currentConfig(KEY_USER))
    ldapPassword.default.set(currentConfig(KEY_PASSWORD))
    ldapAuthType.default.set(currentConfig(KEY_AUTHTYPE))
    ldapFactory.default.set(currentConfig(KEY_FACTORY))

    // Process the optional configuration properties
    currentConfig.get(KEY_LOOKUP).foreach{ 
      prop => testLookup.default.set(Full(prop))
    }

    ControlHelpers.tryo {
      currentConfig.get(KEY_RETRY_INTERVAL).foreach{ 
        prop => retryInterval.default.set(prop.toLong)
      }
    }

    ControlHelpers.tryo {
      currentConfig.get(KEY_MAX_RETRIES).foreach{ 
        prop => retryMaxCount.default.set(prop.toInt)
      }
    }

    currentConfig
  }

  protected def propertiesToMap(props: Properties) : Map[String,String] = {
    Map.empty ++ props
  }

  // =========== Code ====================

  /**
   * Obtains a (possibly cached) InitialContext
   * instance based on the currently set parameters.
   */
  def initialContext = getInitialContext()

  def attributesFromDn(dn: String): Attributes =
    initialContext.getAttributes(dn)

  /**
   * Searches the base DN for entities matching the given filter.
   */
  def search(filter: String): List[String] = {
    logger.debug("Searching for '%s'".format(filter))

    val resultList = new ListBuffer[String]()

    val searchResults = initialContext.search(ldapBaseDn.vend,
                                              filter,
                                              searchControls.vend)

    while(searchResults.hasMore()) {
      resultList += searchResults.next().getName
    }
  
    return resultList.reverse.toList
  }

  /**
   * Attempts to authenticate the given DN against the configured
   * LDAP provider.
   */
  def bindUser(dn: String, password: String) : Boolean = {
    logger.debug("Attempting to bind user '%s'".format(dn))

    try {
      val username = dn + "," + ldapBaseDn.vend
      var ctx = 
        ldapUser.doWith(username) {
          ldapPassword.doWith(password) {
            openInitialContext()
          }
        }

      ctx.close

      logger.info("Successfully authenticated " + dn)
      true
    } catch {
      case ae : AuthenticationException => {
        logger.warn("Authentication failed for '%s' : %s".format(dn, ae.getMessage))
        false
      }
    }
  }

  // This caches the context for the current thread
  private[this] final val currentInitialContext = new ThreadGlobal[InitialLdapContext]()

  /**
   * This method attempts to fetch the cached InitialLdapContext for the
   * current thread. If there isn't a current context, open a new one. If a
   * test DN is configured, the connection (cached or new) will be validated
   * by performing a lookup on the test DN.
   */
  protected def getInitialContext() : InitialLdapContext = {
    val maxAttempts = retryMaxCount.vend
    var attempts = 0

    var context : Box[InitialLdapContext] = Empty

    while (context.isEmpty && attempts < maxAttempts) {
      try {
        context = (currentInitialContext.box, testLookup.vend) match {
          // If we don't want to test an existing context, just return it
          case (Full(ctxt), Empty) => Full(ctxt)
          case (Full(ctxt), Full(test)) => {
            logger.debug("Testing InitialContext prior to returning")
            ctxt.lookup(test)
            Full(ctxt)
          }
          case (Empty,_) => {
            // We'll just allocate a new InitialContext to the thread
            currentInitialContext(openInitialContext())

            // Setting context to Empty here forces one more iteration in case a test
            // DN has been configured
            Empty
          }
        }
      } catch {
        case commE : CommunicationException => {
          logger.error(("Communications failure on attempt %d while " +
                       "verifying InitialContext: %s").format(attempts + 1, commE.getMessage))

          // The current context failed, so clear it
          currentInitialContext(null)

          // We sleep before retrying
          Thread.sleep(retryInterval.vend)
          attempts += 1
        }
      }
    }

    // We have a final check on the context before returning
    context match {
      case Full(ctxt) => ctxt
      case Empty => throw new CommunicationException("Failed to connect to '%s' after %d attempts".
                                                     format(ldapUrl.vend, attempts))
    }
  }

  /**
   * This method does the actual work of setting up the environment and constructing
   * the InitialLdapContext.
   */
  protected def openInitialContext () : InitialLdapContext = {
    logger.debug("Obtaining an initial context from '%s'".format(ldapUrl.vend))
            
    var env = new Hashtable[String, String]()
    env.put(Context.PROVIDER_URL, ldapUrl.vend)
    env.put(Context.SECURITY_AUTHENTICATION, ldapAuthType.vend)
    env.put(Context.SECURITY_PRINCIPAL, ldapUser.vend)
    env.put(Context.SECURITY_CREDENTIALS, ldapPassword.vend)
    env.put(Context.INITIAL_CONTEXT_FACTORY, ldapFactory.vend)
    new InitialLdapContext(env, null)
  }
}

}} // Close nested packages

