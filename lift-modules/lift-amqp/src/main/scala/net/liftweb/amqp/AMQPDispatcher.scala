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
package amqp {

import _root_.com.rabbitmq.client._
import _root_.net.liftweb.actor._
import _root_.java.io.ObjectInputStream
import _root_.java.io.ByteArrayInputStream
import _root_.java.util.Timer
import _root_.java.util.TimerTask

/**
 * @param a The actor to add as a Listener to this Dispatcher.
 */
case class AMQPAddListener(a: LiftActor)

/**
 * @param message A deserialized value received via AMQP.
 *
 * Messages received from AMQP are wrapped in this case class. When you
 * register a listener, this is the case class that you will be matching on.
 */
case class AMQPMessage[T](message: T)


/**
 * Reconnect to the AMQP Server after a delay of {@code delay} milliseconds.
 */
case class AMQPReconnect(delay: Long)

/**
 * An actor that serves as an endpoint for AMQP messages of serialized type T
 * coming into a specific queue/exchange.
 *
 * To listen for messages coming into that queue/exchange, send
 * this actor an AMQPAddListener message.
 *
 * For each message containing a value of type T, all listeners will be send
 * an AMQPMessage contaning that value.
 *
 * See also Enterprise Integration Patterns pp. 508-514
 *
 * @author Steve Jenson (stevej@pobox.com)
 */
abstract class AMQPDispatcher[T](cf: ConnectionFactory, host: String, port: Int) extends LiftActor {
  var (conn, channel) = connect()
  private var as: List[LiftActor] = Nil

  private def connect(): (Connection, Channel) = {
    val conn = cf.newConnection(host, port)
    val channel = conn.createChannel()
    configure(channel)
    (conn, channel)
  }

  /**
   * Override this to configure the Channel and Consumer.
   */
  def configure(channel: Channel)
  
  private val reconnectTimer = new Timer("AMQPReconnectTimer")
  
  protected def messageHandler = {
    case AMQPAddListener(a) => as ::= a
    case msg@AMQPMessage(t) => as.foreach(_ ! msg)
    case AMQPReconnect(delay: Long) => 
      try {
        val details = connect()
        conn = details._1
        channel = details._2
        println("AMQPDispatcher: Successfully reconnected to AMQP Server")
      } catch {
        // Attempts to reconnect again using geometric back-off.
        case e: Exception => {
          val amqp = this
          println("AMQPDispatcher: Will attempt reconnect again in " + (delay * 2) + "ms.")
          reconnectTimer.schedule(new TimerTask(){
            override def run = {
              amqp ! AMQPReconnect(delay * 2)
            }
          }, delay)
        }
      }
      case _ => 
  }
}

/**
 * Example consumer on an AMQP channel.
 */
class SerializedConsumer[T](channel: Channel, a: LiftActor) extends DefaultConsumer(channel) {
  override def handleDelivery(tag: String, env: Envelope, props: AMQP.BasicProperties, body: Array[Byte]) {
    val routingKey = env.getRoutingKey
    val contentType = props.getContentType
    val deliveryTag = env.getDeliveryTag
    val in = new ObjectInputStream(new ByteArrayInputStream(body))
    val t = in.readObject.asInstanceOf[T];
    // Send t to all registered listeners.
    a ! AMQPMessage(t)
    channel.basicAck(deliveryTag, false);
  }
}

/**
 * Example Dispatcher that listens on an example queue and exchange. Use this
 * as your guiding example for creating your own Dispatcher.
 *
 */
class ExampleSerializedAMQPDispatcher[T](factory: ConnectionFactory, host: String, port: Int)
    extends AMQPDispatcher[T](factory, host, port) {
  override def configure(channel: Channel) {
    // Set up the exchange and queue
    channel.exchangeDeclare("mult", "direct")
    channel.queueDeclare("mult_queue")
    channel.queueBind("mult_queue", "mult", "routeroute")
    // Use the short version of the basicConsume method for convenience.
    channel.basicConsume("mult_queue", false, new SerializedConsumer(channel, this))
  }
}

/**
 * Example class that accepts Strings coming in from the
 * ExampleSerializedAMQPDispatcher.
 */
class ExampleStringAMQPListener {
  val params = new ConnectionParameters
  params.setUsername("guest")
  params.setPassword("guest")
  params.setVirtualHost("/")
  params.setRequestedHeartbeat(0)

  val factory = new ConnectionFactory(params)
  // thor.local is a machine on your network with rabbitmq listening on port 5672
  val amqp = new ExampleSerializedAMQPDispatcher[String](factory, "thor.local", 5672)

  // Example Listener that just prints the String it receives.

  val stringListener = new LiftActor {
    protected def messageHandler = {
      case msg@AMQPMessage(contents: String) => println("received: " + msg)
    }
  }
  
  amqp ! AMQPAddListener(stringListener)
}

}
}
