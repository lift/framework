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
package proto {

import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.scala.reflect.Manifest

import _root_.java.util.regex.Pattern

/**
 * This singleton contains the rules for persistence
 */
object ProtoRules extends Factory with LazyLoggable {
  /**
   * The regular expression pattern for matching email addresses.  This
   * assumes that the email address has been converted to lower case.
   */
  var emailRegexPattern = new FactoryMaker(Pattern.compile("^[a-z0-9._%\\-+]+@(?:[a-z0-9\\-]+\\.)+[a-z]{2,4}$")) {}
  
}


}
}
