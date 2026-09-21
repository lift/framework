/*
 * Copyright 2006-2026 Lift Committers and Contributors
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

import org.specs2.mutable.Specification

/**
 * Each example below is expected to FAIL while the corresponding latent bug is
 * present, and to pass once the bug is fixed.
 */
class LatentBugsSpec extends Specification {
  "Latent bug validation".title

  "Bug 1: SecurityHelpers.randomLong / randomInt" should {
    // randomLong / randomInt reduce the raw RNG output into [0, mod) via
    // Helpers.nonNegativeMod, the exact reduction those methods apply.
    // SecureRandom.nextLong can return Long.MinValue, and the old
    // `math.abs(value) % mod` overflowed (math.abs(Long.MinValue) is still
    // negative), yielding a negative result and violating the documented
    // "modulo a number" contract. Exercised deterministically with the
    // worst-case RNG output.
    "never yield a negative value, even when the RNG returns Long.MinValue" in {
      Helpers.nonNegativeMod(Long.MinValue, 7L) must be_>=(0L)
    }
    "never yield a negative value, even when the RNG returns Int.MinValue" in {
      Helpers.nonNegativeMod(Int.MinValue, 7) must be_>=(0)
    }
  }

  "Bug 2: CurrencyZone.get / round (EU / German locale)" should {
    // CurrencyZone.scala:147 uses replaceAll (which takes a *regex*) with the
    // locale grouping separator. For EU (Locale.GERMANY) the grouping separator
    // is '.', so the regex matches every character and wipes out the number.
    "keep the digits of a formatted EU amount instead of erasing them" in {
      EU("1234.56").get must beMatching(".*1234.*")
    }
    "not throw when rounding a EU amount" in {
      // round(p) = make(BigDecimal(get(p))); with the bug get(p) == "" and
      // BigDecimal("") throws NumberFormatException.
      EU("1234.56").round(2) must not(throwA[NumberFormatException])
    }
  }

  "Bug 4: TimeSpan.format" should {
    import Helpers._
    // TimeHelpers.scala:264 caps the final "week" unit with a divisor of 10000,
    // so weeks are reported modulo 10000 and whole 10000-week blocks disappear.
    "report all weeks for a duration of exactly 10000 weeks" in {
      TimeSpan.format(weeks(10000L)) must beEqualTo("10000 weeks")
    }
  }

  "Bug 5: StringHelpers.splitNameValuePairs" should {
    import StringHelpers._
    // StringHelpers.scala:54-62 reads pair(1) without checking that a value was
    // actually present, so a malformed entry throws IndexOutOfBoundsException
    // instead of being handled gracefully.
    "not throw on an entry that has no '=' value" in {
      splitNameValuePairs("foo") must not(throwAn[IndexOutOfBoundsException])
    }
  }
}
