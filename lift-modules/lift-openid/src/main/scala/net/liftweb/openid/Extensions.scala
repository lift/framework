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

import net.liftweb.common._

import _root_.org.openid4java.discovery.Identifier;
import _root_.org.openid4java.discovery.DiscoveryInformation;
import _root_.org.openid4java.message.ax.FetchRequest;
import org.openid4java.message.sreg.SRegRequest
import org.openid4java.message.sreg.{SRegResponse, SRegMessage}

import _root_.org.openid4java.message.ax.FetchResponse;
import _root_.org.openid4java.message.ax.AxMessage;
import _root_.org.openid4java.message._
import _root_.org.openid4java.OpenIDException;
import _root_.org.openid4java.consumer._

import scala.util.matching.Regex

import Implicits._

/**
 * Convert from raw java.util.List to List[T]. Assumes list content is uniform T
 */
private object RawHelper {
  implicit def rawJUL2List[T](l:java.util.List[_]):List[T] = {
    //import scala.collection.jcl.Conversions._
    //val lTyped: java.util.List[T] = l.asInstanceOf[java.util.List[T]]
    //val s = convertList(lTyped)
    //s.toList
    val arr: Array[Object] = l.toArray()
    arr.toList.map(_.asInstanceOf[T])
    // (for {i <- 0 until l.size} yield l(i).asInstanceOf[T]).toList
  }
}

/**
 * Wrapper for org.openid4java.message.Message
 */
class RichMessage(underlying: Message) {
  /**
   * Return extension with the specified URI if available on the message
   */
  def extension(typeURI: String): Box[MessageExtension] = {
    if (underlying.hasExtension(typeURI))
        Box !! underlying.getExtension(typeURI)
    else
        Empty
    }

  /**
   * Return the AxMessage.OPENID_NS_AX FetchResponse extension if available
   */
  def fetchResponse: Box[FetchResponse] = extension(AxMessage.OPENID_NS_AX).asA[FetchResponse]

  /**
   * Return the SRegMessage.OPENID_NS_SREG SRegResponse extentions if available
   */
  def sRegResponse: Box[SRegResponse] = extension(SRegMessage.OPENID_NS_SREG).asA[SRegResponse]
}

/**
 * Wrapper for org.openid4java.message.ax.FetchResponse
 */
class RichFetchResponse(underlying: FetchResponse) {
  import RawHelper._

  /**
   * Return the list of attribute aliases available in the reponse
   */
  def aliases: List[String] = underlying.getAttributeAliases

  /**
   * Get the first value with the specified alias
   */
  def value(alias:String) = Box !! underlying.getAttributeValue(alias)

  /**
   * Get all values with the specified alias
   */
  def values(alias:String): List[String] = underlying.getAttributeValues(alias) flatMap {v:String => Box !! v}
}

/**
 * Wrapper for org.openid4java.message.sreg.SRegResponse
 */
class RichSRegResponse(underlying: SRegResponse) {
  import RawHelper._

  /**
   * Return the list of attribute names available in the response
   */
  def names: List[String] =  underlying.getAttributeNames

  /**
   * Get the value with the specified name
   */
  def value(name:String) = Box !! underlying.getAttributeValue(name)
}

object Implicits {
  implicit def Message2Rich(msg: Message) = new RichMessage(msg)
  implicit def FetchResponse2Rich(fr: FetchResponse) = new RichFetchResponse(fr)
  implicit def SRegResponse2Rich(sr: SRegResponse) = new RichSRegResponse(sr)
}

/**
 * Attribute defines an attribute retrieved using either Sreg or Ax
 *
 * name: SReg name of attribute
 * uri: Ax uri of attribute
 */
case class Attribute(val name:String, val uri: String)

/**
 * Attributes that can retrieved using either Simple Registration or Attribute Exchange
 * extensions
 */
object WellKnownAttributes {
  val Nickname = Attribute("nickname", "http://axschema.org/namePerson/friendly")
  val Email = Attribute("email", "http://axschema.org/contact/email")
  val FullName = Attribute("fullname", "http://axschema.org/namePerson")
  val Language = Attribute("language", "http://axschema.org/pref/language")
  val TimeZone  = Attribute("timezone", "http://axschema.org/pref/timezone")

  // Ax supported only
  val FirstName = Attribute("first", "http://axschema.org/namePerson/first")
  val LastName = Attribute("last", "http://axschema.org/namePerson/last")

  // All WellKnownAttributes
  val attributes = List(Nickname, Email, FullName, Language, TimeZone, FirstName, LastName, Language)

  // Locate attribute with the specified name
  def withName(name: String) = attributes find {_.name == name}

  /**
   * Extract all WellKnownAttributes & their values from message
   */
  def attributeValues(msg: Message): Map[Attribute, String] = {
    Map() ++
      // Try Ax response
    (for {response <-  msg.fetchResponse.toList
         alias <- response.aliases
         attr <- withName(alias)
         value <- response.value(alias)} yield (attr, value)) ++
      // Try SReg response
    (for {response <-  msg.sRegResponse.toList
         name <- response.names
         attr <- withName(name)
         value <- response.value(name)} yield (attr, value))
  }
}

/**
 * Endpoint as identifed from DiscoveredInformation
 */
case class DiscoveredEndpoint(val name:String, val uriRegex: String) {

  /**
   * Create a MessageExtension for the endpoint that fetches the requested attributes
   */
  def fetchRequestExtension(attributes: List[(Attribute, Boolean)]) = {
     val fetch = FetchRequest.createFetchRequest()
     attributes foreach {case (attr,required) => fetch.addAttribute(attr.name, attr.uri, required)}
     fetch
  }

  /**
   * Create a SReg Extension for the endpoint that fetches the requested attributes
   */
  def sRegRequestExtension(attributes: List[(Attribute, Boolean)]) = {
     val sreg = SRegRequest.createFetchRequest()
     attributes foreach {case (attr,required) => sreg.addAttribute(attr.name, required)}
     sreg
  }

 /**
  * Create a provider specific MessageExtension for retrieving
  * the specified attributes using either Ax or SReg
  */
  def makeAttributeExtension(attributes: List[Attribute]): Box[MessageExtension] = Empty
}

/**
 * WellKnownEndpoints know how to create an endpoint specific MessageExtension for retrieving
 * the WellKnownAttributes
 *
 * Usefull for use in combination with the beforeAuth callback on OpenIDConsumer. The following example
 * shows a method that can be passed to beforeAuth to add an extension that fetches the Email, FullName,
 * FirstName & LastName attributes from the selected endpoint.
 *
 * <pre>
 *  def ext(di:DiscoveryInformation, authReq: AuthRequest): Unit = {
 *   import WellKnownAttributes._
 *   WellKnownEndpoints.findEndpoint(di) map {ep
 *     => ep.makeAttributeExtension(List(Email, FullName, FirstName, LastName)) foreach {ex => authReq.addExtension(ex)}}
 * }
 * </pre>
 *
 * See MetaOpenIDProtoUser for an example of how to extract the returned attribute values
 */
object WellKnownEndpoints {

  val Google = new DiscoveredEndpoint("Google","https://www\\.google\\.com/accounts/o8/.+") {
    override def makeAttributeExtension(attrs: List[Attribute]): Box[MessageExtension] =
      Full(fetchRequestExtension(attrs.zipAll(Nil, null, true)))
  }

  val Yahoo = new DiscoveredEndpoint("Yahoo","https://open\\.login\\.yahooapis\\.com/openid/op/auth") {
    override def makeAttributeExtension(attrs: List[Attribute]): Box[MessageExtension] =
      Full(fetchRequestExtension(attrs.zipAll(Nil, null, true)))
  }

  val MyOpenId = new DiscoveredEndpoint("MyOpenId","http://www\\.myopenid\\.com/server") {
    override def makeAttributeExtension(attrs: List[Attribute]): Box[MessageExtension] =
      Full(sRegRequestExtension(attrs.zipAll(Nil, null, true)))
  }

  /**
   * List of WellKnownEndpoints
   */
  val endpoints = List(Google, MyOpenId, Yahoo)

  /**
   * Try to identify a WellKnownEndpoint from DiscoveryInformation
   */
  def findEndpoint(di:DiscoveryInformation): Box[DiscoveredEndpoint] = {
    endpoints find {v => v.uriRegex.r.findFirstIn(di.getOPEndpoint().toString).isDefined}
  }
}

}
}
