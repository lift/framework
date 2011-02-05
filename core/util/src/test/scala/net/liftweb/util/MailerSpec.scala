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
package util {

import _root_.org.specs.runner._
import _root_.org.specs._
import common._
import javax.mail.internet.{MimeMultipart, MimeMessage}
import javax.mail.Transport


class MailerSpecTest extends Runner(MailerSpec) with JUnit
object MailerSpec extends Specification {
  
  object MyMailer extends MailerImpl {
      @volatile var lastMessage: Box[MimeMessage] = Empty

     testModeSend.default.set((msg: MimeMessage) => {
       lastMessage = Full(msg)
       MailerSpec.this.notifyAll()})

    def touch() {
      Props.testMode
      Thread.sleep(10)
    } // do nothing, but force initialization of this class
  }

  MyMailer.touch()

  import MyMailer._
  
  private def doNewMessage(f: => Unit): MimeMessage = {
    lastMessage = Empty

    val ignore = f

    MailerSpec.this.synchronized {
      while (lastMessage.isEmpty) {
        MailerSpec.this.wait(100)
      }
      lastMessage.open_!
    }
  }

  "Mailer" should {
    "deliver simple messages as simple messages" in {
      val msg = doNewMessage {
        sendMail(
          From("sender@nowhere.com"),
          Subject("This is a simple email"),
          To("recipient@nowhere.com"),
          PlainMailBodyType("Here is some plain text.")
          )
      }

      msg.getContent match {
        case s: String =>
        case x => fail("The simple message has content type of " + x.getClass.getName)
      }
    }

    "deliver multipart messages as multipart" in {
      val msg = doNewMessage {
        sendMail(
          From("sender@nowhere.com"),
          Subject("This is a multipart email"),
          To("recipient@nowhere.com"),
          PlainMailBodyType("Here is some plain text."),
          PlainMailBodyType("Here is some more plain text.")
          )
      }

      msg.getContent match {
        case mp: MimeMultipart =>
        case x => fail("The complex message has content type of " + x.getClass.getName)
      }
    }

    "deliver rich messages as multipart" in {
      val msg = doNewMessage {
        sendMail(
          From("sender@nowhere.com"),
          Subject("This is a rich email"),
          To("recipient@nowhere.com"),
          XHTMLMailBodyType(<html> <body>Here is some rich text</body> </html>)
          )
      }

      msg.getContent match {
        case mp: MimeMultipart =>
        case x => fail("The complex message has content type of " + x.getClass.getName)
      }
    }
  }
}

}
}
