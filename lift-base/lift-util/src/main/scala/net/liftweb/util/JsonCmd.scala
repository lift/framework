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

import common._
import xml.NodeSeq

trait HasParams {
  def param(name: String): Box[String]
}


/**
 * Impersonates a JSON command
 */
case class JsonCmd(command: String, target: String, params: Any,
                   all: _root_.scala.collection.Map[String, Any])

/**
 * Holds information about a response
 */
class ResponseInfoHolder {
  var headers: Map[String, String] = Map.empty
  private var _docType: Box[String] = Empty
  private var _setDocType = false

  def docType = _docType

  def docType_=(in: Box[String]) {
    _docType = in
    _setDocType = true
  }

  def overrodeDocType = _setDocType
}

}
}
