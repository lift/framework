/*
 * Copyright 2016 WorldWide Conferencing, LLC
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

import java.net.URI

import scala.concurrent.duration.{Duration,SECONDS,DAYS}

import org.specs2.mutable.Specification

class HttpsRulesSpec extends Specification {
  "HttpsRules" should {
    "default to no required HTTPS" in {
      HttpsRules().headers must beEmpty
    }

    "provide a secure variant with 1-year max age and including sub-domains" in {
      HttpsRules.secure.requiredTime === Some(Duration(365, DAYS))
      HttpsRules.secure.includeSubDomains === true
    }

    "generate a correct Strict-Transport-Security header without sub-domains" in {
      HttpsRules(Some(Duration(1440, SECONDS)), false).headers === List(
        "Strict-Transport-Security" -> "max-age=1440"
      )
    }

    "generate a correct Strict-Transport-Security header including sub-domains" in {
      HttpsRules(Some(Duration(1440, SECONDS)), true).headers === List(
        "Strict-Transport-Security" -> "max-age=1440 ; includeSubDomains"
      )
    }
  }
}

class ContentSecurityPolicySpec extends Specification {
  "ContentSecurityPolicy" should {
    "default to accepting images from everywhere" in {
      ContentSecurityPolicy().imageSources === List(ContentSourceRestriction.All)
    }

    "default to allowing script eval and script sources only from self" in {
      ContentSecurityPolicy().scriptSources === List(
        ContentSourceRestriction.UnsafeEval,
        ContentSourceRestriction.Self
      )
    }

    "default to allowing everything else only from self" in {
      ContentSecurityPolicy().defaultSources === List(ContentSourceRestriction.Self)
      ContentSecurityPolicy().connectSources === Nil
      ContentSecurityPolicy().fontSources === Nil
      ContentSecurityPolicy().frameSources === Nil
      ContentSecurityPolicy().mediaSources === Nil
      ContentSecurityPolicy().objectSources === Nil
      ContentSecurityPolicy().styleSources === Nil
    }

    "provide a secure setting that drops image sources to the default restrictions" in {
      ContentSecurityPolicy.secure.defaultSources === List(ContentSourceRestriction.Self)
      ContentSecurityPolicy.secure.imageSources === Nil
      ContentSecurityPolicy.secure.connectSources === Nil
      ContentSecurityPolicy.secure.fontSources === Nil
      ContentSecurityPolicy.secure.frameSources === Nil
      ContentSecurityPolicy.secure.mediaSources === Nil
      ContentSecurityPolicy.secure.objectSources === Nil
      ContentSecurityPolicy.secure.styleSources === Nil
    }

    "default to reporting to the CSP default report URI" in {
      ContentSecurityPolicy().reportUri === Some(ContentSecurityPolicy.defaultReportUri)
    }

    "provide [X-]Content-Security-Policy if enforcement is enabled" in {
      ContentSecurityPolicy()
        .headers(enforce = true, logViolations = true)
        .collect {
          case (headerName, _) if headerName.contains("Content-Security-Policy") =>
            headerName
        } === List("Content-Security-Policy", "X-Content-Security-Policy")

      ContentSecurityPolicy()
        .headers(enforce = true, logViolations = false)
        .collect {
          case (headerName, _) if headerName.contains("Content-Security-Policy") =>
            headerName
        } === List("Content-Security-Policy", "X-Content-Security-Policy")
    }

    "provide [X-]Content-Security-Policy-Report-Only if enforcement is disabled and logging enabled" in {
      ContentSecurityPolicy()
        .headers(enforce = false, logViolations = true)
        .collect {
          case (headerName, _) if headerName.contains("Content-Security-Policy") =>
            headerName
        } === List("Content-Security-Policy-Report-Only", "X-Content-Security-Policy-Report-Only")
    }

    "provide no headers with enforcement and logging disabled" in {
      ContentSecurityPolicy()
        .headers(enforce = false, logViolations = false) must beEmpty
    }

    "correctly generate restriction strings for the various restriction types" in {
      ContentSecurityPolicy(
        defaultSources = Nil,
        imageSources = Nil,
        styleSources = Nil,
        reportUri = None,
        scriptSources = List(
          ContentSourceRestriction.All,
          ContentSourceRestriction.Host("https://base.*.example.com"),
          ContentSourceRestriction.Scheme("data"),
          ContentSourceRestriction.None,
          ContentSourceRestriction.Self,
          ContentSourceRestriction.UnsafeInline,
          ContentSourceRestriction.UnsafeEval
        )
      ).headers(enforce = true).head._2 ===
        "script-src * https://base.*.example.com data: 'none' 'self' 'unsafe-inline' 'unsafe-eval'"
    }

    "not generate a restriction string for empty restrictions" in {
      ContentSecurityPolicy(
        Nil,
        Nil,
        Nil,
        Nil,
        Nil,
        Nil,
        Nil,
        Nil,
        Nil,
        reportUri = None
      ).headers(enforce = true).head._2 === ""
    }

    "combine restrictions for multiple content types correctly" in {
      ContentSecurityPolicy(
        defaultSources = List(ContentSourceRestriction.Self),
        fontSources = List(ContentSourceRestriction.Host("https://base.*.example.com")),
        frameSources = List(ContentSourceRestriction.Scheme("data")),
        imageSources = List(ContentSourceRestriction.All),
        mediaSources = List(ContentSourceRestriction.None),
        scriptSources = List(ContentSourceRestriction.Self),
        styleSources = List(ContentSourceRestriction.UnsafeInline),
        reportUri = None
      ).headers(enforce = true).head._2 ===
        "default-src 'self'; font-src https://base.*.example.com; frame-src data:; img-src *; media-src 'none'; script-src 'self'; style-src 'unsafe-inline'"
    }

    "include the report URI" in {
      ContentSecurityPolicy(
        defaultSources = List(ContentSourceRestriction.Self),
        fontSources = Nil,
        frameSources = Nil,
        imageSources = Nil,
        mediaSources = Nil,
        scriptSources = Nil,
        styleSources = Nil,
        reportUri = Some(new URI("/example/uri"))
      ).headers(enforce = true, logViolations = true).head._2 ===
        "default-src 'self'; report-uri /example/uri"
    }

    "include the report URI even if logging is disabled provided enforcement is enabled" in {
      ContentSecurityPolicy(
        defaultSources = List(ContentSourceRestriction.Self),
        fontSources = Nil,
        frameSources = Nil,
        imageSources = Nil,
        mediaSources = Nil,
        scriptSources = Nil,
        styleSources = Nil,
        reportUri = Some(new java.net.URI("/example/uri"))
      ).headers(enforce = true, logViolations = false).head._2 ===
        "default-src 'self'; report-uri /example/uri"
    }
  }
}

class FrameRestrictionsSpec extends Specification {
  "FrameRestrictions" should {
    "provide the correct X-Frame-Options setting for SameOrigin restrictions" in {
      FrameRestrictions.SameOrigin.headers === List("X-Frame-Options" -> "SAMEORIGIN")
    }

    "provide the correct X-Frame-Options setting for Deny restrictions" in {
      FrameRestrictions.Deny.headers === List("X-Frame-Options" -> "DENY")
    }
  }
}

class SecurityRulesSpec extends Specification {
  "SecurityRules" should {
    "default to no HTTPS requirement" in {
      SecurityRules().https === None
    }

    "default to default Content-Security-Policy settings" in {
      SecurityRules().content === Some(ContentSecurityPolicy())
    }

    "default to same-origin frame restrictions" in {
      SecurityRules().frameRestrictions === Some(FrameRestrictions.SameOrigin)
    }

    "default to enforcing in no modes and logging in all modes" in {
      SecurityRules().enforceInOtherModes === false
      SecurityRules().enforceInDevMode === false
      SecurityRules().logInOtherModes === true
      SecurityRules().logInDevMode === true
    }

    "provide a secure default with secure HTTPS, CSP, and non-dev enforcement" in {
      SecurityRules.secure.https === Some(HttpsRules.secure)
      SecurityRules.secure.content === Some(ContentSecurityPolicy.secure)
      SecurityRules.secure.enforceInOtherModes === true
      SecurityRules.secure.enforceInDevMode === false
      SecurityRules.secure.logInOtherModes === true
      SecurityRules.secure.logInDevMode === true
    }
  }
}
