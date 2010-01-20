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
package util {

import Helpers._
import common._

/**
 * A simple fixed-point currency representation
 */
class Currency(val amount: Long, val symbol: String, val decimals: Int) {
  override def toString = {
    if (decimals == 0) symbol+amount
    else {
      val d = amount.toDouble
      val pow = Math.pow(10, decimals)
      symbol+(d / pow)
    }
  }

  /**
   * Return a string formatted as the URL-encoded symbol followed
   * by the amount and decimals delimited by the "&amp;" symbol.
   */
  def forDB: String = Helpers.urlEncode(symbol)+"&"+amount+"&"+decimals

  /**
   * Determines whether two currencies are equal with respect to
   * symbol, amount, and decimal value.
   */
  override def equals(other: Any) = other match {
    case c: Currency => c.amount == amount && c.symbol == symbol && c.decimals == decimals
    case _ => false
  }

  /**
   * Addition on Currency objects. This compares currency symbols to prevent
   * addition of different types of currency to one another.
   * @throws CurrencyMismatchException for mismatched currency types.
   */
  def +(other: Currency): Currency =
  if (symbol != other.symbol || decimals != other.decimals) throw new CurrencyMismatchException
  else new Currency(amount + other.amount, symbol, decimals)

  /**
   * Subtraction on Currency objects. This compares currency symbols to prevent
   * subtraction of different types of currency from one another.
   * @throws CurrencyMismatchException for mismatched currency types.
   */
  def -(other: Currency): Currency =
  if (symbol != other.symbol || decimals != other.decimals) throw new CurrencyMismatchException
  else new Currency(amount - other.amount, symbol, decimals)
}

/**
 * This exception is thrown if an operation is attempted on two currency values
 * where currency symbols do not match.
 */
class CurrencyMismatchException extends Exception

object Currency {
  /**
   * Parse a currency from the format specified by Currency.forDB
   */
  def apply(s: String): Box[Currency] = s.roboSplit("&") match {
    case List(cur, a, d) => for (av <- asLong(a); dv <- asInt(d)) yield new Currency(av, urlDecode(cur), dv)
    case _ => Empty
  }
}

}
}
