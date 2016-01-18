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
      HttpsRules().headers must be empty
    }

    "provide a secure variant with 1-year max age and including sub-domains" in {
      HttpsRules.secure.requiredTime must_== Some(Duration(365, DAYS))
      HttpsRules.secure.includeSubDomains must_== true
    }

    "generate a correct Strict-Transport-Security header without sub-domains" in {
      HttpsRules(Some(Duration(1440, SECONDS)), false).headers must_== List(
        "Strict-Transport-Security" -> "max-age=1440"
      )
    }

    "generate a correct Strict-Transport-Security header including sub-domains" in {
      HttpsRules(Some(Duration(1440, SECONDS)), true).headers must_== List(
        "Strict-Transport-Security" -> "max-age=1440 ; includeSubDomains"
      )
    }
  }
}

class ContentSecurityPolicySpec extends Specification {
  "ContentSecurityPolicy" should {
    "default to accepting images from everywhere" in {
      ContentSecurityPolicy().imageSources must_== List(ContentSourceRestriction.All)
    }

    "default to allowing script eval and script sources only from self" in {
      ContentSecurityPolicy().scriptSources must_== List(
        ContentSourceRestriction.UnsafeEval,
        ContentSourceRestriction.Self
      )
    }

    "default to allowing everything else only from self" in {
      ContentSecurityPolicy().defaultSources must_== List(ContentSourceRestriction.Self)
      ContentSecurityPolicy().connectSources must_== Nil
      ContentSecurityPolicy().fontSources must_== Nil
      ContentSecurityPolicy().frameSources must_== Nil
      ContentSecurityPolicy().mediaSources must_== Nil
      ContentSecurityPolicy().objectSources must_== Nil
      ContentSecurityPolicy().styleSources must_== Nil
    }

    "provide a secure setting that drops image sources to the default restrictions" in {
      ContentSecurityPolicy.secure.defaultSources must_== List(ContentSourceRestriction.Self)
      ContentSecurityPolicy.secure.imageSources must_== Nil
      ContentSecurityPolicy.secure.connectSources must_== Nil
      ContentSecurityPolicy.secure.fontSources must_== Nil
      ContentSecurityPolicy.secure.frameSources must_== Nil
      ContentSecurityPolicy.secure.mediaSources must_== Nil
      ContentSecurityPolicy.secure.objectSources must_== Nil
      ContentSecurityPolicy.secure.styleSources must_== Nil
    }

    "default to reporting to the CSP default report URI" in {
      ContentSecurityPolicy().reportUri must_== Some(ContentSecurityPolicy.defaultReportUri)
    }

    "provide [X-]Content-Security-Policy if enforcement is enabled" in {
      ContentSecurityPolicy()
        .headers(enforce = true, logViolations = true)
        .collect {
          case (headerName, _) if headerName.contains("Content-Security-Policy") =>
            headerName
        } must_== List("Content-Security-Policy", "X-Content-Security-Policy")

      ContentSecurityPolicy()
        .headers(enforce = true, logViolations = false)
        .collect {
          case (headerName, _) if headerName.contains("Content-Security-Policy") =>
            headerName
        } must_== List("Content-Security-Policy", "X-Content-Security-Policy")
    }

    "provide [X-]Content-Security-Policy-Report-Only if enforcement is disabled and logging enabled" in {
      ContentSecurityPolicy()
        .headers(enforce = false, logViolations = true)
        .collect {
          case (headerName, _) if headerName.contains("Content-Security-Policy") =>
            headerName
        } must_== List("Content-Security-Policy-Report-Only", "X-Content-Security-Policy-Report-Only")
    }

    "provide no headers with enforcement and logging disabled" in {
      ContentSecurityPolicy()
        .headers(enforce = false, logViolations = false) must be empty
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
      ).headers(enforce = true).head._2 must_==
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
      ).headers(enforce = true).head._2 must_== ""
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
      ).headers(enforce = true).head._2 must_==
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
      ).headers(enforce = true, logViolations = true).head._2 must_==
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
      ).headers(enforce = true, logViolations = false).head._2 must_==
        "default-src 'self'; report-uri /example/uri"
    }
  }
}

class FrameRestrictionsSpec extends Specification {
  "FrameRestrictions" should {
    "provide the correct X-Frame-Options setting for SameOrigin restrictions" in {
      FrameRestrictions.SameOrigin.headers must_== List("X-Frame-Options" -> "SAMEORIGIN")
    }

    "provide the correct X-Frame-Options setting for Deny restrictions" in {
      FrameRestrictions.Deny.headers must_== List("X-Frame-Options" -> "DENY")
    }
  }
}

class SecurityRulesSpec extends Specification {
  "SecurityRules" should {
    "default to no HTTPS requirement" in {
      SecurityRules().https must_== None
    }

    "default to default Content-Security-Policy settings" in {
      SecurityRules().content must_== Some(ContentSecurityPolicy())
    }

    "default to same-origin frame restrictions" in {
      SecurityRules().frameRestrictions must_== Some(FrameRestrictions.SameOrigin)
    }

    "default to enforcing in no modes and logging in all modes" in {
      SecurityRules().enforceInOtherModes must_== false
      SecurityRules().enforceInDevMode must_== false
      SecurityRules().logInOtherModes must_== true
      SecurityRules().logInDevMode must_== true
    }

    "provide a secure default with secure HTTPS, CSP, and non-dev enforcement" in {
      SecurityRules.secure.https must_== Some(HttpsRules.secure)
      SecurityRules.secure.content must_== Some(ContentSecurityPolicy.secure)
      SecurityRules.secure.enforceInOtherModes must_== true
      SecurityRules.secure.enforceInDevMode must_== false
      SecurityRules.secure.logInOtherModes must_== true
      SecurityRules.secure.logInDevMode must_== true
    }
  }
}
