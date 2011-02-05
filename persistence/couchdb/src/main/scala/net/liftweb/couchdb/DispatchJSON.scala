/*
 * Copyright 2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb {
package couchdb {

import _root_.dispatch.{Handler, Request}
import _root_.net.liftweb.json.JsonAST.{JValue, render}
import _root_.net.liftweb.json.{JsonParser, Printer}
import _root_.org.apache.http.client.methods.{HttpPost, HttpPut}
import _root_.org.apache.http.entity.StringEntity
import _root_.org.apache.http.params.HttpProtocolParams

object DispatchJSON {
  /** Implicitly convert a string (representing a URL) to a JSONRequest, which has operators for sending and receiving Lift JSON JValues */
  implicit def stringToJSONRequest(url: String): JSONRequest = JSONRequest(new Request(url))

  /** Implicitly convert a request to a JSONRequest, which has operators for sending and receiving Lift JSON JValues */
  implicit def requestToJSONRequest(req: Request): JSONRequest = JSONRequest(req)
}

/** Wrapper for a Dispatch Request that has operators for sending and receiving Lift JSON JValues */
case class JSONRequest(req: Request) {
  /** Handle the response by converting it into a Lift JSON JValue */
  def handleJSON[T](f: JValue => T): Handler[T] = req >- { s => f(JsonParser.parse(s)) }

  /** Alias for handleJSON */
  def ># [T](f: JValue => T): Handler[T] = handleJSON(f)

  /** PUT a JValue rendered as compact JSON to the resource referenced by the request */
  def put(jvalue: JValue): Request = req.next {
    val m = new HttpPut
    m.setEntity(jvalueToStringEntity(jvalue))
    HttpProtocolParams.setUseExpectContinue(m.getParams, false)
    Request.mimic(m) _
  } 

  /** Alias for put */
  def <<<# (jvalue: JValue): Request = put(jvalue)

  /** POST a JValue rendered as compact JSON to the resource referenced by the request */
  def post(jvalue: JValue): Request = req.next {
    val m = new HttpPost
    m.setEntity(jvalueToStringEntity(jvalue))
    HttpProtocolParams.setUseExpectContinue(m.getParams, false)
    Request.mimic(m) _
  }

  /** Alias for post */
  def <<# (jvalue: JValue): Request = post(jvalue)

  /** Convert a JValue into a StringEntity with the application/json content type */
  private def jvalueToStringEntity(in: JValue): StringEntity = {
    val entity = new StringEntity(Printer.compact(render(in)), Request.factoryCharset)
    entity.setContentType("application/json")
    entity
  }
}

}
}
