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
package util

import javax.mail._
import javax.mail.internet._
import javax.naming.{Context, InitialContext}
import java.util.Properties

import scala.language.implicitConversions
import scala.xml.{Text, Elem, Node, NodeSeq}

import common._
import actor._

import Mailer._

/**
 * Utilities for sending email.
 */
object Mailer extends Mailer {

  sealed abstract class MailTypes
  /**
   * Add message headers to outgoing messages
   */
  final case class MessageHeader(name: String, value: String) extends MailTypes
  abstract class MailBodyType extends MailTypes
  final case class PlusImageHolder(name: String, mimeType: String, bytes: Array[Byte], attachment: Boolean = false)

  /**
   * Represents a text/plain mail body. The given text will
   * be encoded as UTF-8 when sent.
   */
  final case class PlainMailBodyType(text: String) extends MailBodyType

  /**
   * Represents a text/plain mail body that is encoded with the
   * specified charset
   */
  final case class PlainPlusBodyType(text: String, charset: String) extends MailBodyType

  final case class XHTMLMailBodyType(text: NodeSeq) extends MailBodyType
  final case class XHTMLPlusImages(text: NodeSeq, items: PlusImageHolder*) extends MailBodyType


  sealed abstract class RoutingType extends MailTypes
  sealed abstract class AddressType extends RoutingType {
    def address: String
    def name: Box[String]
  }
  final case class From(address: String, name: Box[String] = Empty) extends AddressType
  final case class To(address: String, name: Box[String] = Empty) extends AddressType
  final case class CC(address: String, name: Box[String] = Empty) extends AddressType
  final case class Subject(subject: String) extends RoutingType
  final case class BCC(address: String, name: Box[String] = Empty) extends AddressType
  final case class ReplyTo(address: String, name: Box[String] = Empty) extends AddressType

  final case class MessageInfo(from: From, subject: Subject, info: List[MailTypes])
}

/**
 * This trait implmenets the mail sending.  You can create subclasses of this class/trait and
 * implement your own mailer functionality
 */
trait Mailer extends SimpleInjector {
  private val logger = Logger(classOf[Mailer])

  implicit def xmlToMailBodyType(html: NodeSeq): MailBodyType = XHTMLMailBodyType(html)

  implicit def addressToAddress(in: AddressType): Address = {
    val ret = new InternetAddress(in.address)
    in.name.foreach{n => ret.setPersonal(n)}
    ret
  }

  implicit def adListToAdArray(in: List[AddressType]): Array[Address] = in.map(addressToAddress).toArray

  /**
   * Passwords cannot be accessed via System.getProperty.  Instead, we
   * provide a means of explicitlysetting the authenticator.
   */
  //def authenticator = authenticatorFunc
  var authenticator: Box[Authenticator] = Empty

  /**
   * Use the mailer resource in your container by specifying the JNDI name
   */
  var jndiName: Box[String] = Empty

  /**
   * Custom properties for the JNDI session
   */
  var customProperties: Map[String, String] = Map()

  lazy val jndiSession: Box[Session] =
  for{
    name <- jndiName
    contextObj <- Helpers.tryo(new InitialContext().lookup("java:comp/env"))
    context <- Box.asA[Context](contextObj)
    sessionObj <- Helpers.tryo(context.lookup(name))
    session <- Box.asA[Session](sessionObj)
  } yield session

  lazy val properties: Properties = {
    val p = System.getProperties.clone.asInstanceOf[Properties]
    customProperties.foreach {case (name, value) => p.put(name, value)}
    // allow the properties file to set/override system properties

    Props.props.foreach {
      case (name, value) =>
        p.setProperty(name, value)
    }
    p
  }

  /**
   * The host that should be used to send mail.
   */
  def host = hostFunc()

  /**
   * To change the way the host is calculated, set this to the function that calcualtes the host name.
   * By default: System.getProperty("mail.smtp.host")
   */
  var hostFunc: () => String = _host _

  private def _host = properties.getProperty("mail.smtp.host") match {
    case null => "localhost"
    case s => s
  }

  def buildProps: Properties = {
    val p = properties.clone.asInstanceOf[Properties]
    p.getProperty("mail.smtp.host") match {
      case null => p.put("mail.smtp.host", host)
      case _ =>
    }

    p
  }

  /**
   * Set the mail.charset property to something other than UTF-8 for non-UTF-8
   * mail.
   */
  lazy val charSet = properties.getProperty("mail.charset") match {
    case null => "UTF-8"
    case x => x
  }

  // def host_=(hostname: String) = System.setProperty("mail.smtp.host", hostname)

  protected class MsgSender extends SpecializedLiftActor[MessageInfo] {
    protected def messageHandler = {
      case MessageInfo(from, subject, info) =>
        try {
          msgSendImpl(from, subject, info)
        } catch {
          case e: Exception => logger.error("Couldn't send mail", e)
        }
    }
  }

  protected def performTransportSend(msg: MimeMessage) = {
    import Props.RunModes._
    (Props.mode match {
        case Development => devModeSend.vend
        case Test => testModeSend.vend
        case Staging => stagingModeSend.vend
        case Production => productionModeSend.vend
        case Pilot => pilotModeSend.vend
        case Profile => profileModeSend.vend
      }).apply(msg)
  }

  /**
   * How to send a message in dev mode.  By default, use Transport.send(msg)
   */
  lazy val devModeSend: Inject[MimeMessage => Unit] = new Inject[MimeMessage => Unit]((m: MimeMessage) => Transport.send(m)) {}

  /**
   * How to send a message in test mode.  By default, log the message
   */
  lazy val testModeSend: Inject[MimeMessage => Unit] = new Inject[MimeMessage => Unit]((m: MimeMessage) => logger.info("Sending Mime Message: "+m)) {}

  /**
   * How to send a message in staging mode.  By default, use Transport.send(msg)
   */
  lazy val stagingModeSend: Inject[MimeMessage => Unit] = new Inject[MimeMessage => Unit]((m: MimeMessage) => Transport.send(m)) {}

  /**
   * How to send a message in production mode.  By default, use Transport.send(msg)
   */
  lazy val productionModeSend: Inject[MimeMessage => Unit] = new Inject[MimeMessage => Unit]((m: MimeMessage) => Transport.send(m)) {}

  /**
   * How to send a message in pilot mode.  By default, use Transport.send(msg)
   */
  lazy val pilotModeSend: Inject[MimeMessage => Unit] = new Inject[MimeMessage => Unit]((m: MimeMessage) => Transport.send(m)) {}

  /**
   * How to send a message in profile mode.  By default, use Transport.send(msg)
   */
  lazy val profileModeSend: Inject[MimeMessage => Unit] = new Inject[MimeMessage => Unit]((m: MimeMessage) => Transport.send(m)) {}

  /**
   * Synchronously send an email.
   */
  def blockingSendMail(from: From, subject: Subject, rest: MailTypes*) {
    msgSendImpl(from, subject, rest.toList)
  }
  
  def msgSendImpl(from: From, subject: Subject, info: List[MailTypes]) {
    val session = authenticator match {
      case Full(a) => jndiSession openOr Session.getInstance(buildProps, a)
      case _ => jndiSession openOr Session.getInstance(buildProps)
    }
    val subj = MimeUtility.encodeText(subject.subject, "utf-8", "Q")
    val message = new MimeMessage(session)
    message.setFrom(from)
    message.setRecipients(Message.RecipientType.TO, info.flatMap {case x: To => Some[To](x) case _ => None})
    message.setRecipients(Message.RecipientType.CC, info.flatMap {case x: CC => Some[CC](x) case _ => None})
    message.setRecipients(Message.RecipientType.BCC, info.flatMap {case x: BCC => Some[BCC](x) case _ => None})
    message.setSentDate(new java.util.Date())
    // message.setReplyTo(filter[MailTypes, ReplyTo](info, {case x @ ReplyTo(_) => Some(x); case _ => None}))
    message.setReplyTo(info.flatMap {case x: ReplyTo => Some[ReplyTo](x) case _ => None})
    message.setSubject(subj)
    info.foreach {
      case MessageHeader(name, value) => message.addHeader(name, value)
      case _ => 
    }

    val bodyTypes = info.flatMap {case x: MailBodyType => Some[MailBodyType](x); case _ => None}
    bodyTypes match {
      case PlainMailBodyType(txt) :: Nil =>
        message.setText(txt)

      case _ =>
        val multiPart = new MimeMultipart("alternative")
        bodyTypes.foreach {
          tab =>
          val bp = buildMailBody(tab)
          multiPart.addBodyPart(bp)
        }
        message.setContent(multiPart);
    }

    Mailer.this.performTransportSend(message)
  }

  protected lazy val msgSender = new MsgSender

  /**
   * The default mechanism for encoding a NodeSeq to a String representing HTML.  By default, use Html5.toString(node)
   */
  protected def encodeHtmlBodyPart(in: NodeSeq): String = Html5.toString(firstNode(in))

  protected def firstNode(in: NodeSeq): Node = in match {
    case n: Node => n
    case ns => ns.toList.collect {
      case e: Elem => e
    } match {
      case Nil => if (ns.length == 0) Text("") else ns(0)
      case x :: xs => x
    }
  }

  /**
   * Given a MailBodyType, convert it to a javax.mail.BodyPart.  You can override this method if you
   * add custom MailBodyTypes
   */
  protected def buildMailBody(tab: MailBodyType): BodyPart = {
    val bp = new MimeBodyPart

    tab match {
      case PlainMailBodyType(txt) =>
        bp.setText(txt, "UTF-8")

      case PlainPlusBodyType(txt, charset) =>
        bp.setText(txt, charset)

      case XHTMLMailBodyType(html) =>
        bp.setContent(encodeHtmlBodyPart(html), "text/html; charset=" + charSet)

      case XHTMLPlusImages(html, img@_*) =>
        val (attachments, images) = img.partition(_.attachment)
        val relatedMultipart = new MimeMultipart("related")

        val htmlBodyPart = new MimeBodyPart
        htmlBodyPart.setContent(encodeHtmlBodyPart(html), "text/html; charset=" + charSet)
        relatedMultipart.addBodyPart(htmlBodyPart)

        images.foreach { image =>
          relatedMultipart.addBodyPart(buildAttachment(image))
        }

        if (attachments.isEmpty) {
          bp.setContent(relatedMultipart)
        } else {
          // Some old versions of Exchange server will not behave correclty without
          // a mixed multipart wrapping file attachments. This appears to be linked to
          // specific versions of Exchange and Outlook. See the discussion at
          // https://github.com/lift/framework/pull/1569 for more details.
          val mixedMultipart = new MimeMultipart("mixed")

          val relatedMultipartBodypart = new MimeBodyPart
          relatedMultipartBodypart.setContent(relatedMultipart)
          mixedMultipart.addBodyPart(relatedMultipartBodypart)

          attachments.foreach { attachment =>
            mixedMultipart.addBodyPart(buildAttachment(attachment))
          }

          bp.setContent(mixedMultipart)
        }
    }

    bp
  }

  private def buildAttachment(holder: PlusImageHolder) = {
    val part = new MimeBodyPart

    part.setFileName(holder.name)
    part.setContentID(holder.name)
    part.setDisposition(if (holder.attachment) Part.ATTACHMENT else Part.INLINE)
    part.setDataHandler(new javax.activation.DataHandler(new javax.activation.DataSource {
      def getContentType = holder.mimeType
      def getInputStream = new java.io.ByteArrayInputStream(holder.bytes)
      def getName = holder.name
      def getOutputStream = throw new java.io.IOException("Unable to write to item")
    }))

    part
  }


  /**
   * Asynchronously send an email.
   */
  def sendMail(from: From, subject: Subject, rest: MailTypes*) {
    // forward it to an actor so there's no time on this thread spent sending the message
    msgSender ! MessageInfo(from, subject, rest.toList)
  }
}

