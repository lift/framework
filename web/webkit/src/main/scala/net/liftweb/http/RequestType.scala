/*
 * Copyright 2007-2011 WorldWide Conferencing, LLC
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
package http 

import provider._

abstract class RequestType extends Serializable{
  def post_? : Boolean = false

  def get_? : Boolean = false

  def head_? : Boolean = false

  def put_? : Boolean = false

  def delete_? : Boolean = false

  def options_? : Boolean = false

  def method: String
}

case object GetRequest extends RequestType {
  override def get_? = true
  val method = "GET"
}
case object PostRequest extends RequestType {
  override def post_? = true
  val method = "POST"
}
case object HeadRequest extends RequestType {
  override def head_? = true
  val method = "HEAD"
}
case object PutRequest extends RequestType {
  override def put_? = true
  val method = "PUT"
}
case object DeleteRequest extends RequestType {
  override def delete_? = true
  val method = "DELETE"
}
case object OptionsRequest extends RequestType {
  override def options_? = true
  val method = "OPTIONS"
}
case class UnknownRequest(method: String) extends RequestType

object RequestType {
  def apply(req: HTTPRequest): RequestType = {
    req.method.toUpperCase match {
      case "GET" => GetRequest
      case "POST" => PostRequest
      case "HEAD" => HeadRequest
      case "PUT" => PutRequest
      case "DELETE" => DeleteRequest
      case "OPTIONS" => OptionsRequest
      case meth => UnknownRequest(meth)
    }
  }
}

