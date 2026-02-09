/*
 * Copyright 2007-2026 Lift Committers and Contributors
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
package util

import java.util.Locale
import java.text.{DecimalFormat, NumberFormat}
import scala.annotation.nowarn

trait TwoFractionDigits {
    def numberOfFractionDigits = 2
    def scale = 10
}

trait DollarCurrency extends TwoFractionDigits {
    def currencySymbol: String = "$"
}

/* Various Currencies */
@nowarn("msg=constructor Locale in class Locale is deprecated \\(since 19\\)") // Use Locale.of when only JDK 19+ is supported
object AU extends CurrencyZone {
    type Currency = AUD
    var locale: Locale = new Locale("en", "AU")
    def make(x: BigDecimal): AUD = new Currency{def amount: BigDecimal = x}
    abstract class AUD extends AbstractCurrency("AUD") with DollarCurrency {}
}

object US extends CurrencyZone {
    type Currency = USD
    var locale: Locale = Locale.US
    def make(x: BigDecimal): USD = new Currency{def amount: BigDecimal = x}
    abstract class USD extends AbstractCurrency("USD") with DollarCurrency {}
}

object GB extends CurrencyZone {
    type Currency = GBP
    var locale: Locale = Locale.UK
    def make(x: BigDecimal): GBP = new Currency{def amount: BigDecimal = x}
    abstract class GBP extends AbstractCurrency("GBP") with TwoFractionDigits {def currencySymbol = "£"}
}

object EU extends CurrencyZone {
    type Currency = EUR
    var locale: Locale = Locale.GERMANY // guess this is why its a var
    def make(x: BigDecimal): EUR = new Currency{def amount: BigDecimal = x; override val _locale: Locale = locale}
    abstract class EUR extends AbstractCurrency("EUR") with TwoFractionDigits {def currencySymbol = "€"}
}

abstract class CurrencyZone {
    type Currency <: AbstractCurrency

    var locale: Locale
    def make(x: BigDecimal): Currency

    def apply(x: String): Currency = {
        try {
            make(BigDecimal(x)) // try normal number
        } catch { case e: java.lang.NumberFormatException => {
                    try {
                        make(BigDecimal(""+NumberFormat.getNumberInstance(locale).parse(x))) // try with grouping separator
                    } catch { case e: java.text.ParseException => {
                                make(BigDecimal(""+NumberFormat.getCurrencyInstance(locale).parse(x))) } // try with currency symbol and grouping separator
                    }
                }
        }

    }

    def apply(x: BigDecimal): Currency = make(x)

    /* currency factory*/
    abstract class AbstractCurrency(val designation: String) extends Ordered[Currency] {

        val _locale: Locale = locale
        def amount: BigDecimal
        def floatValue: Float = amount.floatValue
        def doubleValue: Double = amount.doubleValue
        def currencySymbol: String
        def numberOfFractionDigits: Int
        def scale: Int

        def +(that: Currency): Currency = make(this.amount + that.amount)
        def +(that: Int): Currency = this + make(that)

        def *(that: Currency): Currency = make(this.amount * that.amount)
        def *(that: Int): Currency = this * make(that)

        def -(that: Currency): Currency = make(this.amount - that.amount)
        def -(that: Int): Currency = this - make(that)

        def /(that: Currency): Currency =
        make(new BigDecimal(this.amount.bigDecimal.divide(that.amount.bigDecimal, scale, java.math.RoundingMode.HALF_UP)) )
        def /(that: Int): Currency = this / make(that)

        def compare(that: Currency): Int = this.amount compare that.amount

        override def equals(that: Any): Boolean = that match {
            case that: AbstractCurrency => this.designation+this.format("", scale) == that.designation+that.format("", scale)
            case _ => false
        }

        override def hashCode: Int = (this.designation+format("", scale)).hashCode

        def round(precision: Int) = make(BigDecimal(get(precision)))

        override def toString: String = format("", numberOfFractionDigits)

        def format(fd: Int): String = format(currencySymbol, fd)

        def format: String = format(currencySymbol, numberOfFractionDigits)

        def format(currencySymbol: String, numberOfFractionDigits: Int): String = {
            val moneyValue = amount match {
                case null => 0
                case _ => amount.setScale(numberOfFractionDigits, BigDecimal.RoundingMode.HALF_UP).doubleValue;
            }

            val numberFormat = NumberFormat.getCurrencyInstance(_locale)
            numberFormat.setMinimumFractionDigits(numberOfFractionDigits)
            numberFormat.setMaximumFractionDigits(numberOfFractionDigits)
            val symbol=numberFormat.getCurrency.getSymbol(_locale)
            numberFormat.format(moneyValue).replace(symbol, currencySymbol)

        }

        def get: String = get(numberOfFractionDigits)

        def get(numberOfFractionDigits: Int): String = {
            val nf = NumberFormat.getNumberInstance(_locale)
            val df = nf.asInstanceOf[DecimalFormat]
            val groupingSeparator = df.getDecimalFormatSymbols.getGroupingSeparator

            format("", numberOfFractionDigits).replaceAll(groupingSeparator.toString+"", "")
        }

    }

}
