/*
 * Copyright 2007-2015 WorldWide Conferencing, LLC
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

import scala.concurrent.duration._

import util.Props

// TODO Set it up so we can redirect HTTP->HTTPS but not send Strict-Transport-Security.
/**
 * Rules for HTTPS usage by a Lift application.
 *
 * Currently corresponds directly to the [[HTTP `Strict-Transport-Security`
 * header http://tools.ietf.org/html/rfc6797]].
 */
final case class HttpsRules(
  /**
   * When set, the duration of the requirement that HTTPS be used for this
   * site. It's usually a good idea for this to be a high number. If unset,
   * HTTPS is not required when interacting with this site.
   */
  requiredTime: Option[Duration] = None,
  /**
   * When set to true, the required time above includes subdomains.
   */
  includeSubDomains: Boolean = false
) {
  lazy val headers: List[(String, String)] = {
    requiredTime.toList.map { duration =>
      val age = s"max-age=${duration.toSeconds}"

      val header =
        if (includeSubDomains) {
          s"$age ; includeSubDomains"
        } else {
          age
        }

      ("Strict-Transport-Security" -> header)
    }
  }

  /**
   * Returns the headers implied by this set of HTTPS rules. If
   * `enforceInDevMode` is false and we are in dev mode, returns nothing.
   */
  def headers(enforceInDevMode: Boolean = false): List[(String, String)] = {
    if (! enforceInDevMode && Props.devMode) {
      Nil
    } else  {
      headers
    }
  }
}
object HttpsRules {
  /**
   * Creates a restrictive set of HTTPS rules requiring HTTPS for 365 days and
   * including subdomains in the requirement.
   */
  def secure = apply(Some(365.days), true)
}

/**
 * Base trait for content source restrictions. These are different ways of
 * restricting where contents of various types are allowed to come from, in
 * conjunction with `ContentSecurityPolicy` categories.
 */
sealed trait ContentSourceRestriction {
  /**
   * The `Content-Security-Policy` string that represents this restriction.
   */
  def sourceRestrictionString: String
}
/**
 * Marker trait for restrictions that only apply to JavaScript.
 */
sealed trait JavaScriptSourceRestriction extends ContentSourceRestriction
/**
 * Marker trait for restrictions that only apply to stylesheets.
 */
sealed trait StylesheetSourceRestriction extends ContentSourceRestriction

object ContentSourceRestriction {
  /**
   * Indicates content from all sources is allowed.
   */
  case object All extends ContentSourceRestriction {
    val sourceRestrictionString = "*"
  }
  /**
   * Indicates content from the given host path is allowed. See the
   * `Content-Security-Policy` spec's [[matching rules for `host-source`
   * http://www.w3.org/TR/CSP/#matching]] for more about what this can look
   * like.
   *
   * Example:
   * {{{
   * Host("https://base.*.example.com")
   * }}}
   */
  case class Host(hostAndPath: String) extends ContentSourceRestriction {
    val sourceRestrictionString = hostAndPath
  }
  /**
   * Indicates content from the given scheme is allowed. The scheme should not
   * include the trailing `:`.
   *
   * Example:
   * {{{
   * Scheme("data")
   * }}}
   */
  case class Scheme(scheme: String) extends ContentSourceRestriction {
    val sourceRestrictionString = scheme + ":"
  }
  /**
   * Indicates content from no sources is allowed.
   */
  case object None extends ContentSourceRestriction {
    val sourceRestrictionString = "'none'"
  }
  /**
   * Indicates content from the same origin as the content is allowed.
   */
  case object Self extends ContentSourceRestriction {
    val sourceRestrictionString = "'self'"
  }
  /**
   * Indicates inline content on the page is allowed to be interpreted.  It is
   * highly recommended that this not be used, as it exposes your application to
   * cross-site scripting and other vulnerabilities.
   *
   * If not specified for JavaScript, JavaScript `on*` event handler attributes,
   * `<script>` elements, and `javascript:` URIs will not be executed by a
   * browser that supports content security policies.
   *
   * If not specified for stylesheets, `<style>` elements and inline `style`
   * attributes will not be read by a browser that supports content security
   * policies.
   */
  case object UnsafeInline extends JavaScriptSourceRestriction with StylesheetSourceRestriction {
    val sourceRestrictionString = "'unsafe-inline'"
  }
  /**
   * Indicates `eval` and related functionality can be used. It is highly
   * recommended that this not be used, as it exposes your application to code
   * injection attacks.
   *
   * If not specified for JavaScript, invoking `eval`, the `Function`
   * constructor, or `setTimeout`/`setInterval` with a string parameter will
   * all throw security exceptions in a browser that supports content security
   * policies.
   */
  case object UnsafeEval extends JavaScriptSourceRestriction {
    val sourceRestrictionString = "'unsafe-eval'"
  }
}

/**
 * Specifies a `[[https://developer.mozilla.org/en-US/docs/Web/Security/CSP Content-Security-Policy]] `
 * for this site. This will be sent to the client in a `Content-Security-Policy`
 * header when responses are returned from Lift.
 *
 * In development mode, content security policy violations are only reported if
 * the browser supports them, not enforced. In all other modes, content security
 * policy violations are enforced if the browser supports them.
 *
 * Note that the `X-Webkit-CSP` header is NOT specified, due to
 * potentially-broken behavior in iOS 5 and 5.1. This means iOS 6/6.1 will not
 * receive a content security policy that it can
 * understand. See the [[caniuse page on content security policy
 * http://caniuse.com/#feat=contentsecuritypolicy]] for more.
 */
final case class ContentSecurityPolicy(
  /**
   * A list of default source restrictions; if one of the other sources
   * parameters is empty, the default sources will apply instead.
   */
  defaultSources: List[ContentSourceRestriction] = List(ContentSourceRestriction.Self),
  /**
   * A list of source restrictions for `XmlHttpRequest` (AJAX) connections.
   */
  connectSources: List[ContentSourceRestriction] = Nil,
  /**
   * A list of source restrictions for loading fonts (e.g., from CSS `font-face`
   * declarations).
   */
  fontSources: List[ContentSourceRestriction] = Nil,
  /**
   * A list of source restrictions for loading frames and iframes.
   */
  frameSources: List[ContentSourceRestriction] = Nil,
  /**
   * A list of source restrictions for loading images.
   */
  imageSources: List[ContentSourceRestriction] = List(ContentSourceRestriction.All),
  /**
   * A list of source restrictions for loading media (audio and video).
   */
  mediaSources: List[ContentSourceRestriction] = Nil,
  /**
   * A list of source restrictions for loading `object`, `embed`, `applet`, and
   * related elements.
   */
  objectSources: List[ContentSourceRestriction] = Nil,
  /**
   * A list of source restrictions for loading scripts. Also accepts the
   * `UnsafeInline` and `UnsafeEval` sources, though these are strongly
   * discouraged.
   */
  scriptSources: List[JavaScriptSourceRestriction] = Nil,
  /**
   * A list of source restrictions for loading styles. Also accepts the
   * `UnsafeInline` source, though it is strongly discouraged.
   */
  styleSources: List[StylesheetSourceRestriction] = Nil,
  /**
   * The URI where any violation of the security policy will be reported. You
   * can set the function that handles these violations in
   * `LiftRules.contentSecurityPolicyViolationReport`. By default, reported to
   * `[[ContentSecurityPolicy.defaultReportUri]]`.
   *
   * If this is `None`, violations will not be reported.
   */
  reportUri: Option[URI] = Some(ContentSecurityPolicy.defaultReportUri)
) {
  /**
   * The string that describes this content security policy in the syntax
   * expected by the `Content-Security-Policy` header.
   */
  def contentSecurityPolicyString = {
    val allRestrictions =
      Map(
        "default-src" -> defaultSources,
        "connect-src" -> connectSources,
        "font-src" -> fontSources,
        "frame-src" -> frameSources,
        "img-src" -> imageSources,
        "media-src" -> mediaSources,
        "object-src" -> objectSources,
        "script-src" -> scriptSources,
        "style-src" -> styleSources
      )

    val restrictionString =
      allRestrictions
        .collect {
          case (category, restrictions) if restrictions.nonEmpty =>
            category +
              " " +
              restrictions.map(_.sourceRestrictionString).mkString(" ")
        }
        .mkString("; ")

    reportUri.map { uri =>
      s"$restrictionString; report-uri $uri"
    } getOrElse {
      restrictionString
    }
  }

  private lazy val reportOnlyHeaders = {
    List(
      "Content-Security-Policy-Report-Only" -> contentSecurityPolicyString,
      "X-Content-Security-Policy-Report-Only" -> contentSecurityPolicyString
    )
  }
  private lazy val enforcedHeaders = {
    List(
      "Content-Security-Policy" -> contentSecurityPolicyString,
      "X-Content-Security-Policy" -> contentSecurityPolicyString
    )
  }
  /**
   * Returns the headers implied by this content security policy.
   */
  def headers(reportOnlyInDev: Boolean = true): List[(String, String)] = {
    if (reportOnlyInDev && Props.devMode) {
      reportOnlyHeaders
    } else {
      enforcedHeaders
    }
  }
}
object ContentSecurityPolicy {
  /**
   * The default URI where security policy violations will be reported. This
   * URI is under Lift's URI namespace, at `[[LiftRules.liftPath]]`.
   */
  def defaultReportUri = {
    new URI(LiftRules.liftPath + "/content-security-policy-report")
  }

  /**
   * Creates a restrictive content security policy that disallows images from
   * all sources except the page's origin.
   *
   * Note that the default content security policy restricts all other resources
   * to the same origin, but allows images from any source; the secure one only
   * differs because it adds restrictions to the image sources.
   */
  def secure(reportUri: Option[URI]): ContentSecurityPolicy = {
    ContentSecurityPolicy(imageSources = Nil, reportUri = reportUri)
  }
  def secure: ContentSecurityPolicy = secure(Some(defaultReportUri))
}

/**
 * Specifies security rules for a Lift application. By default, HTTPS is not
 * required and `Content-Security-Policy` is restricted to the current domain
 * for everything except images, which are accepted from any domain.
 *
 * You can use `[[SecurityRules$.secure]]` to enable more restrictive, but
 * also more secure, defaults.
 */
final case class SecurityRules(
  https: Option[HttpsRules] = None,
  content: Option[ContentSecurityPolicy] = Some(ContentSecurityPolicy()),
  /**
   * If true, security policies and HTTPS rules are enforced in dev mode in
   * addition to staging/pilot/production/etc.
   */
  enforceInDevMode: Boolean = false,
  /**
   * If true, dev mode violations of security policies are logged by
   * default. Note that if you override
   * [[`LiftRules.contentSecurityPolicyViolationReport`]] or otherwise change
   * the default Lift policy violation handling behavior, it will be up to you
   * to handle this property as desired.
   */
  logInDevMode: Boolean = true
) {
  /**
   * Returns the headers implied by this set of security rules.
   */
  lazy val headers: List[(String, String)] = {
    https.toList.flatMap(_.headers(enforceInDevMode)) :::
      content.toList.flatMap(_.headers(enforceInDevMode))
  }
}
object SecurityRules {
  /**
   * Creates a restrictive set of security rules, including required HTTPS,
   * [[HttpsRules$.secure secure HTTPS rules]], and
   * [[ContentSecurityPolicy$.secure secure `Content-Security-Policy` rules]].
   *
   * To tweak any of these settings, use the `SecurityRules` constructor
   * directly.
   */
  def secure = {
    apply(
      Some(HttpsRules.secure),
      Some(ContentSecurityPolicy.secure)
    )
  }
}