/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
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
package xmpp {

import _root_.java.util.Collection
import _root_.java.io.IOException
import _root_.org.jivesoftware.smack.Chat
import _root_.org.jivesoftware.smack.ChatManager
import _root_.org.jivesoftware.smack.ConnectionConfiguration
import _root_.org.jivesoftware.smack.MessageListener
import _root_.org.jivesoftware.smack.ChatManagerListener
import _root_.org.jivesoftware.smack.Roster
import _root_.org.jivesoftware.smack.RosterEntry
import _root_.org.jivesoftware.smack.RosterListener
import _root_.org.jivesoftware.smack.XMPPConnection
import _root_.org.jivesoftware.smack.XMPPException
import _root_.org.jivesoftware.smack.packet.Message
import _root_.org.jivesoftware.smack.packet.Presence
import _root_.org.jivesoftware.smack.util.StringUtils
import _root_.net.liftweb.actor._
import _root_.scala.collection.mutable.HashMap
import _root_.scala.collection.mutable.Map

/** These messages are sent to the XMPPDispatcher Actor. */
// Send the Presence to the XMPP server
case class SetPresence(presence: Presence)
case class CreateChat(to: String)
case class SendMsg(to: String, msg: String)
case class CloseChat(to: String)
case class GetPendingMsg(to: String)

/** These messages are sent to the client Actor */
case class NewRoster(r: Roster)
// TODO(stevej): make these type-safe when Java generics are in.
case class RosterEntriesDeleted[T](entries: Collection[T])
case class RosterEntriesUpdated[T](entries: Collection[T])
case class RosterEntriesAdded[T](entries: Collection[T])
case class RosterPresenceChanged(p: Presence)
case class NewChat(chat: Chat)
case class RecvMsg(chat: Chat, msg: Message)
case class BulkMsg(chat: Chat, msg: List[Message])

// A RosterListener that sends events to the Actor given.
abstract class DispatchRosterListener(val dispatch: LiftActor) extends RosterListener

/**
* An XMPP Dispatcher connects to an XMPP server on behalf of a User.
*
*
* @param connf A function that returns the proper ConnectionConfiguration
* @param login A function that takes an XMPPConnection and initializes the connection
*              by logging in.
* @author Steve Jenson (stevej@pobox.com)
*/
class XMPPDispatcher(val connf: () => ConnectionConfiguration, val login: XMPPConnection => Unit) extends LiftActor {
  val conn = new XMPPConnection(connf())
  conn.connect
  login(conn)
  val roster: Roster = conn.getRoster();
  // Some XMPP server configs do not give you a Roster.
  if (roster != null) {
    roster.addRosterListener(new DispatchRosterListener(this) {
      def entriesDeleted(a: Collection[String]) {
	dispatch ! RosterEntriesDeleted(a)
      }
      def entriesUpdated(a: Collection[String]) {
	dispatch ! RosterEntriesUpdated(a)
      }
      def entriesAdded(a: Collection[String]) {
	dispatch ! RosterEntriesAdded(a)
      }
      def presenceChanged(p: Presence) {
	dispatch ! RosterPresenceChanged(p)
      }
    })
  }

  // This is a Map of to: address to Chat object.
  val chats: HashMap[String, Chat] = new HashMap[String, Chat]
  val pendingMsg: HashMap[String, List[String]] = new HashMap[String, List[String]]
  val md = new MessageDispatcher(this)

  // Manage the remotely created chats, so we don't miss incomming messages
  // The only thing we need to do is add our message listener, the rest
  // will be managed by the dispatching actor.
  conn.getChatManager().addChatListener(new ChatManagerListener {
    def chatCreated(chat: Chat, createdLocally: Boolean) {
      if (!createdLocally) {
        chat.addMessageListener(md)
      }
    }
  })

  private var clients: List[LiftActor] = Nil

  protected def messageHandler = {
      /* These are all messages we process from the client Actors. */
      case AddListener(actor: LiftActor) => 
        actor ! NewRoster(roster)
	clients ::= actor

      case RemoveListener(actor: LiftActor) => 
	clients -= actor

      case SetPresence(presence) => conn.sendPacket(presence)
    
      case GetPendingMsg(to) => pendingMsg.getOrElse(to, Nil) match {
        case Nil => pendingMsg -= to
        case xs: List[Message] => {
          pendingMsg -= to;
          clients.foreach(_ ! BulkMsg(chats.getOrElse(to, null), xs))
        }
        case _ => 
      }

      case CreateChat(to) => {
        val chat: Chat = conn.getChatManager().createChat(to, md)
        chats += (to -> chat)
        clients.foreach(_ ! NewChat(chat))
      }

      // Send a Message to the XMPP Server
      case SendMsg(to, message) => 
        val msg = new Message(to, Message.Type.chat)
        msg.setBody(message)
        // If there isn't an existing chat in chats, make one and put it there.
        chats.getOrElse(to, Nil) match {
          case chat: Chat => chat.sendMessage(msg)
          case Nil => {
            val chat = conn.getChatManager().createChat(to, new MessageDispatcher(this))
            chats += (to -> chat)
            chat.sendMessage(msg)
          }
        }

      case CloseChat(to) => chats -= to

      /* From here on are Messages we process from the XMPP server */
      case r@RosterEntriesDeleted(_) => clients.foreach(_ ! r)
      case r@RosterEntriesUpdated(_) => clients.foreach(_ ! r)
      case r@RosterEntriesAdded(_) => clients.foreach(_ ! r)
      case r@RosterPresenceChanged(_) => clients.foreach(_ ! r)
      case c@NewChat(chat) => clients.foreach(_ ! c)
      // A new Chat has come in from the XMPP server
      case m@RecvMsg(chat, msg) => {
        // If this is starting a new chat, then it won't be in the
        // chats Map. So add it and send the clients a NewChat message.
        chats.getOrElse(msg.getFrom(), Nil) match {
          case Nil => {
            chats += (msg.getFrom() -> chat)
            clients.foreach(_ ! NewChat(chat))
          }
          case _ => {}
        }
        clients.foreach(_ ! RecvMsg(chat, msg))

      }
      case a => 
  }
  
  // Accepts messages from XMPP and sends them to the local actor for dispatching.
  class MessageDispatcher(dispatch: LiftActor) extends MessageListener {
    def processMessage(chat: Chat, msg: Message) {
      dispatch ! RecvMsg(chat, msg)
    }
  }
}

case class AddListener(actor: LiftActor)
case class RemoveListener(actor: LiftActor)
case object Start


/**
* An example Chat application that prints to stdout.
*
* @param username is the username to login to at Google Talk: format: something@gmail.com
* @param password is the password for the user account at Google Talk.
*/
class ConsoleChatActor(val username: String, val password: String) extends LiftActor {
  def connf() = new ConnectionConfiguration("talk.google.com", 5222, "gmail.com")
  def login(conn: XMPPConnection) = conn.login(username, password)
  val xmpp = new XMPPDispatcher(connf, login)

  val chats: Map[String, List[Message]] = new HashMap[String, List[Message]]
  val rosterMap: HashMap[String, Presence] = new HashMap[String, Presence]
  var roster: Roster = null

  protected def messageHandler = {
      case Start => {
        xmpp ! AddListener(this)
        xmpp ! SetPresence(new Presence(Presence.Type.available))
      }
      case NewChat(c) => {
        chats += (c.getParticipant -> Nil)
      }
      case RecvMsg(chat, msg) => {
        println("RecvMsg from: " + msg.getFrom + ": " + msg.getBody);
      }
      case NewRoster(r) => {
        println("getting a new roster: " + r)
        this.roster = r
        val e: Array[Object] = r.getEntries.toArray.asInstanceOf[Array[Object]]
        for (entry <- e) {
          val user: String = entry.asInstanceOf[RosterEntry].getUser
          rosterMap += (user -> r.getPresence(user))
        }
      }
      case RosterPresenceChanged(p) => {
        val user = StringUtils.parseBareAddress(p.getFrom)
        println("Roster Update: " + user + " " + p)
        // It's best practice to ask the roster for the presence. This is because
        // multiple presences can exist for one user and the roster knows which one
        // has priority.
        rosterMap += (user -> roster.getPresence(user))
      }
    case RosterEntriesDeleted(e) => {
        println(e)
      }
    case RosterEntriesUpdated(e) => {
        println(e)
      }
    case RosterEntriesAdded(e) => {
        println(e)
      }
    case a => println(a)

  }

  def createChat(to: String) {
    xmpp ! CreateChat(to)
  }

  def sendMessage(to: String, msg: String) {
    xmpp ! SendMsg(to, msg)
  }

  /**
  * @returns an Iterable of all users who aren't unavailable along with their Presence
  */
  def availableUsers: Iterable[(String, Presence)] = {
    rosterMap.filter((e) => e._2.getType() != Presence.Type.unavailable)
  }
}

object ConsoleChatHelper {
  /**
  * @param u is the username
  * @param p is the password
  */
  def run(u: String, p: String) = {
    val ex = new ConsoleChatActor(u, p)

    ex ! Start
    ex
  }
}

}
}
