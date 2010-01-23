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
package widgets {
package flot {

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

/**
 * Serie of data
 */

trait FlotSerie
{
  def data: List[(Double, Double)] = Nil
  def label: Box[String] = Empty
  def lines: Box[FlotLinesOptions] = Empty
  def points: Box[FlotPointsOptions] = Empty
  def bars: Box[FlotBarsOptions] = Empty
  def color: Box[Either[String, Int]] = Empty
  def shadowSize: Box[Int] = Empty
}

}
}
}
