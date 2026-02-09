/*
 * Copyright 2007-2015 Lift Committers and Contributors
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

import common._
import util.Props
import util.Helpers.tryo

import LiftRules._

import org.json4s._

/**
 * Rules for HTTPS usage by a Lift application.
 *
 * Currently corresponds directly to the [[http://tools.ietf.org/html/rfc6797
 * HTTP `Strict-Transport-Security` header]].
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
   * Returns the headers implied by this set of HTTPS rules. If `enforce` is
   * false, returns nothing.
   */
  def headers(enforce: Boolean): List[(String, String)] = {
    if (enforce) {
      headers
    } else  {
      Nil
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
sealed trait GeneralSourceRestriction extends JavaScriptSourceRestriction with StylesheetSourceRestriction

object ContentSourceRestriction {
  /**
   * Indicates content from all sources is allowed.
   */
  case object All extends GeneralSourceRestriction {
    val sourceRestrictionString = "*"
  }
  /**
   * Indicates content from the given host path is allowed. See the
   * `Content-Security-Policy` spec's [[https://www.w3.org/TR/CSP/#source-list-path-patching
   * matching rules for `host-source`]] for more about what this can look
   * like.
   *
   * Example:
   * {{{
   * Host("https://base.*.example.com")
   * }}}
   */
  case class Host(hostAndPath: String) extends GeneralSourceRestriction {
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
  case class Scheme(scheme: String) extends GeneralSourceRestriction {
    val sourceRestrictionString = scheme + ":"
  }
  /**
   * Indicates content from no sources is allowed.
   */
  case object None extends GeneralSourceRestriction {
    val sourceRestrictionString = "'none'"
  }
  /**
   * Indicates content from the same origin as the content is allowed.
   */
  case object Self extends GeneralSourceRestriction {
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
   * Indicates `eval` and related functionality can be used. Some of Lift's
   * functionality, including `idMemoize` and comet handling, relies on eval,
   * so not including this in your script sources will mean you won't be able to
   * use those.
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
 * understand. See the [[http://caniuse.com/#feat=contentsecuritypolicy caniuse
 * page on content security policy]] for more.
 *
 * @param defaultSources A list of default source restrictions; if one of the
 *        other sources parameters is empty, the default sources will apply
 *        instead.
 * @param connectSources A list of source restrictions for `XmlHttpRequest`
 *        (AJAX) connections.
 * @param fontSources A list of source restrictions for loading fonts (e.g.,
 *        from CSS `font-face` declarations).
 * @param frameSources A list of source restrictions for loading frames and
 *        iframes.
 * @param imageSources A list of source restrictions for loading images.
 * @param mediaSources A list of source restrictions for loading media (audio
 *        and video).
 * @param objectSources A list of source restrictions for loading `object`,
 *        `embed`, `applet`, and related elements.
 * @param scriptSources A list of source restrictions for loading scripts. Also
 *        accepts the `[[ContentSourceRestriction.UnsafeInline UnsafeInline]]`
 *        and `[[ContentSourceRestriction.UnsafeEval UnsafeEval]]` source
 *        restrictions, though these are strongly discouraged.
 * @param styleSources A list of source restrictions for loading styles. Also
 *        accepts the `[[ContentSourceRestriction.UnsafeInline UnsafeInline]]`
 *        source, though it is strongly discouraged.
 * @param reportUri The URI where any violation of the security policy will be
 *        reported. You can set the function that handles these violations in
 *        `[[LiftRules.contentSecurityPolicyViolationReport]]`. By default,
 *        reported to `[[ContentSecurityPolicy.defaultReportUri]]`.
 *
 *        If this is `None`, violations will not be reported.
 */
final case class ContentSecurityPolicy(
  defaultSources: List[ContentSourceRestriction] = List(ContentSourceRestriction.Self),
  connectSources: List[ContentSourceRestriction] = Nil,
  fontSources: List[ContentSourceRestriction] = Nil,
  frameSources: List[ContentSourceRestriction] = Nil,
  imageSources: List[ContentSourceRestriction] = List(ContentSourceRestriction.All),
  mediaSources: List[ContentSourceRestriction] = Nil,
  objectSources: List[ContentSourceRestriction] = Nil,
  scriptSources: List[JavaScriptSourceRestriction] = List(
    ContentSourceRestriction.UnsafeEval,
    ContentSourceRestriction.Self
  ),
  styleSources: List[StylesheetSourceRestriction] = Nil,
  reportUri: Option[URI] = Some(ContentSecurityPolicy.defaultReportUri)
) {
  /**
   * The string that describes this content security policy in the syntax
   * expected by the `Content-Security-Policy` header.
   */
  def contentSecurityPolicyString = {
    val allRestrictions =
      List(
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

  private[this] lazy val reportOnlyHeaders = {
    List(
      "Content-Security-Policy-Report-Only" -> contentSecurityPolicyString,
      "X-Content-Security-Policy-Report-Only" -> contentSecurityPolicyString
    )
  }
  private[this] lazy val enforcedHeaders = {
    List(
      "Content-Security-Policy" -> contentSecurityPolicyString,
      "X-Content-Security-Policy" -> contentSecurityPolicyString
    )
  }
  /**
   * Returns the headers implied by this content security policy.
   */
  def headers(enforce: Boolean = true, logViolations: Boolean = true): List[(String, String)] = {
    if (enforce) {
      enforcedHeaders
    } else if (logViolations) {
      reportOnlyHeaders
    } else {
      Nil
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
  def secure: ContentSecurityPolicy = {
    ContentSecurityPolicy(imageSources = Nil)
  }
}

/**
 * The expected payload of a content security policy violation report.
 *
 * Parsable from the JSON POST that a browser should send when a violation
 * occurs.
 */
case class ContentSecurityPolicyViolation(
  documentUri: String,
  referrer: String,
  blockedUri: String,
  violatedDirective: String,
  originalPolicy: String
)
object ContentSecurityPolicyViolation extends LazyLoggable {
  private[this] implicit val formats: DefaultFormats.type = DefaultFormats

  def defaultViolationHandler: DispatchPF = {
    case request @ Req(start :+ "content-security-policy-report", _, _) if start == LiftRules.liftContextRelativePath() =>
      val violation =
        for {
          requestJson <- request.forcedBodyAsJson
          camelCasedJson = requestJson.transformField {
            case JField("document-uri", content) =>
              JField("documentUri", content)
            case JField("blocked-uri", content) =>
              JField("blockedUri", content)
            case JField("violated-directive", content) =>
              JField("violatedDirective", content)
            case JField("original-policy", content) =>
              JField("originalPolicy", content)
          }
          violationJson = camelCasedJson \ "csp-report"
          extractedViolation <- tryo(violationJson.extract[ContentSecurityPolicyViolation])
        } yield {
          extractedViolation
        }

      () => {
        violation match {
          case Full(violation) =>
            LiftRules.contentSecurityPolicyViolationReport(violation) or
              Full(OkResponse())

          case _ =>
            logger.warn(
              s"Got a content security violation report we couldn't interpret: '${request.body.map(new String(_, "UTF-8"))}'."
            )

            Full(BadRequestResponse("Unrecognized format for content security policy report."))
        }
      }
  }
}

/**
 * Defines restrictions on allowing served pages to be embedded in frames.
 */
sealed trait FrameRestrictions {
  def headers: List[(String,String)]

  /**
   * Returns the headers implied by these frame restrictions.
   *
   * Because of how frame restrictions are handled, if enforcement is turned
   * off, no headers are generated.
   */
  def headers(enforce: Boolean = false): List[(String,String)] = {
    if (enforce) {
      headers
    } else {
      Nil
    }
  }
}
object FrameRestrictions {
  /**
   * Allows other pages from the same origin as the one being served to embed
   * this page in a frame.
   */
  case object SameOrigin extends FrameRestrictions {
    val headers = List("X-Frame-Options" -> "SAMEORIGIN")
  }
  /**
   * Does not allow embedding the page being served in a frame at all.
   */
  case object Deny extends FrameRestrictions {
    val headers = List("X-Frame-Options" -> "DENY")
  }
}

/**
 * Specifies security rules for a Lift application. By default, HTTPS is not
 * required and `Content-Security-Policy` is restricted to the current domain
 * for everything except images, which are accepted from any domain.
 * Additionally, served pages can only be embedded in other frames from
 * the current domain.
 *
 * You can use `[[SecurityRules.secure]]` to enable more restrictive, but
 * also more secure, defaults.
 *
 * @param enforceInDevMode If true, security policies and HTTPS rules are
 *        enforced in dev mode in addition to staging/pilot/production/etc.
 * @param logInDevMode If true, dev mode violations of security policies are
 *        logged by default. Note that if you override
 *        `[[LiftRules.contentSecurityPolicyViolationReport]]` or otherwise
 *        change the default Lift policy violation handling behavior, it will
 *        be up to you to handle this property as desired.
 */
final case class SecurityRules(
  https: Option[HttpsRules] = None,
  content: Option[ContentSecurityPolicy] = Some(ContentSecurityPolicy()),
  frameRestrictions: Option[FrameRestrictions] = Some(FrameRestrictions.SameOrigin),
  enforceInOtherModes: Boolean = false,
  logInOtherModes: Boolean = true,
  enforceInDevMode: Boolean = false,
  logInDevMode: Boolean = true
) {
  private val enforce_? = {
    if (Props.devMode) {
      enforceInDevMode
    } else {
      enforceInOtherModes
    }
  }
  private val logViolations_? = {
    if (Props.devMode) {
      logInDevMode
    } else {
      logInOtherModes
    }
  }
  /**
   * Returns the headers implied by this set of security rules.
   */
  lazy val headers: List[(String, String)] = {
    https.toList.flatMap(_.headers(enforce_?)) :::
      content.toList.flatMap(_.headers(enforce_?, logViolations_?)) :::
      frameRestrictions.toList.flatMap(_.headers(enforce_?))
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
      Some(ContentSecurityPolicy.secure),
      enforceInOtherModes = true
    )
  }
}
