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

import _root_.dispatch.Request
import _root_.net.liftweb.json.JsonAST.{JValue, render}
import _root_.net.liftweb.json.Printer
import _root_.org.apache.http.client.methods.{HttpPost, HttpPut}
import _root_.org.apache.http.entity.StringEntity
import _root_.org.apache.http.params.HttpProtocolParams

object SendJSON {
  /** Implicitly convert a request to a JSONSendingRequest, which has operators for sending Lift JSON JValues via PUT or POST requests */
  implicit def requestToJSONSendingRequest(req: Request): JSONSendingRequest = JSONSendingRequest(req)
}

/** Wrapper for a Dispatch Request that has operators for sending Lift JSON JValues via PUT or POST requests */
case class JSONSendingRequest(req: Request) {
  /** PUT a JValue rendered as compact JSON to the resource referenced by the request */
  def put(jvalue: JValue): Request = req.next {
    val m = new HttpPut
    m.setEntity(new StringEntity(Printer.compact(render(jvalue)), Request.factoryCharset))
    HttpProtocolParams.setUseExpectContinue(m.getParams, false)
    Request.mimic(m) _
  } 

  /** Alias for put */
  def <<<# (jvalue: JValue): Request = put(jvalue)

  /** POST a JValue rendered as compact JSON to the resource referenced by the request */
  def post(jvalue: JValue): Request = req.next {
    val m = new HttpPost
    m.setEntity(new StringEntity(Printer.compact(render(jvalue)), Request.factoryCharset))
    HttpProtocolParams.setUseExpectContinue(m.getParams, false)
    Request.mimic(m) _
  }

  /** Alias for post */
  def <<# (jvalue: JValue): Request = post(jvalue)
}

}
}
