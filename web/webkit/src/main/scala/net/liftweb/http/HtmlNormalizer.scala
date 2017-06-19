package net.liftweb
package http

import scala.xml._
import scala.collection.immutable.Vector

import js.JsCmd
import js.JsCmds.Noop
import js.JE.{AnonFunc, Call, JsRaw}

import util.Helpers._

/**
 * Represents an HTML attribute for an event handler. Carries the event name and
 * the JS that should run when that event is triggered as a String.
 */
private case class EventAttribute(eventName: String, jsString: String)
private object EventAttribute {
  /**
   * Some elements allow a URL attribute to take a `javascript:(//)`-style
   * URL instead of setting an `on*` event in order to invoke JS. For example,
   * you can (and Lift does) set a form's `action` attribute to
   * `javascript://(some JS)` instead of setting `onsubmit` to `(someJS);
   * return false`.
   *
   * This is a map from those attribute names to the corresponding JS event
   * that would be used to execute that JS when it isn't run in line.
   */
  val eventsByAttributeName =
    Map(
      "action" -> "submit",
      "href" -> "click"
    )

  object EventForAttribute {
    def unapply(attributeName: String): Option[String] = {
      eventsByAttributeName.get(attributeName)
    }
  }
}

case class NodesAndEventJs(nodes: NodeSeq, js: JsCmd) {
  def append(newNodesAndEventJs: NodesAndEventJs): NodesAndEventJs = {
    this.copy(
      nodes = nodes ++ newNodesAndEventJs.nodes,
      js = js & newNodesAndEventJs.js
    )
  }
  def append(newNodeAndEventJs: NodeAndEventJs): NodesAndEventJs = {
    append(newNodeAndEventJs.node, newNodeAndEventJs.js)
  }
  def append(newNode: Node): NodesAndEventJs = {
    this.copy(nodes = nodes :+ newNode)
  }
  def append(newJs: JsCmd): NodesAndEventJs = {
    this.copy(js = js & newJs)
  }
  def append(newNode: Node, newJs: JsCmd): NodesAndEventJs = {
    this.copy(nodes = nodes :+ newNode, js = js & newJs)
  }
}
private[http] case class NodeAndEventJs(node: Node, js: JsCmd) {
  def append(newJs: JsCmd): NodeAndEventJs = {
    this.copy(js = js & newJs)
  }
}

/**
 * Helper class that performs certain Lift-specific normalizations of HTML
 * represented as `NodeSeq`s:
 *  - Fixes URLs for `link`, `a`, `form`, and `script` elements to properly
 *    prepend the container's context path in case these are absolute URLs.
 *  - Extracts event attributes (replacing them with
 *    `[[LiftRules.attributeForRemovedEventAttributes]]` if needed), returning
 *    instead JavaScript that will attach the corresponding event handler to
 *    that element.
 *  - Provides for caller-specific additional processing for each node.
 */
private[http] final object HtmlNormalizer {
  // Fix URLs using Req.normalizeHref and extract JS event attributes for
  // putting into page JS. Returns a triple of:
  //  - The optional id that was found in this set of attributes.
  //  - The normalized metadata.
  //  - A list of extracted `EventAttribute`s.
  private[this] def normalizeUrlAndExtractEvents(
    attributeToNormalize: String,
    attributes: MetaData,
    contextPath: String,
    shouldRewriteUrl: Boolean, // whether to apply URLRewrite.rewriteFunc
    extractInlineJavaScript: Boolean,
    eventAttributes: List[EventAttribute] = Nil
  ): (Option[String], MetaData, List[EventAttribute]) = {
    if (attributes == Null) {
      (None, Null, eventAttributes)
    } else {
      // Note: we don't do this tail-recursively because we have to preserve
      // attribute order!
      val (id, normalizedRemainingAttributes, remainingEventAttributes) =
        normalizeUrlAndExtractEvents(
          attributeToNormalize,
          attributes.next,
          contextPath,
          shouldRewriteUrl,
          extractInlineJavaScript,
          eventAttributes
        )

      attributes match {
        case attribute @ UnprefixedAttribute(
               EventAttribute.EventForAttribute(eventName),
               attributeValue,
               remainingAttributes
             ) if attributeValue.text.startsWith("javascript:") && extractInlineJavaScript =>
          val attributeJavaScript = {
            // Could be javascript: or javascript://.
            val base = attributeValue.text.substring(11)
            val strippedJs =
              if (base.startsWith("//"))
                base.substring(2)
              else
                base

            if (strippedJs.trim.isEmpty) {
              Nil
            } else {
              // When using javascript:-style URIs, event.preventDefault is implied.
              List(strippedJs + "; event.preventDefault()")
            }
          }

          val updatedEventAttributes =
            attributeJavaScript.map(EventAttribute(eventName, _)) :::
            remainingEventAttributes

          (id, normalizedRemainingAttributes, updatedEventAttributes)

        case UnprefixedAttribute(name, _, _) if name == attributeToNormalize =>
          val normalizedUrl =
            Req.normalizeHref(
              contextPath,
              attributes.value,
              shouldRewriteUrl,
              URLRewriter.rewriteFunc
            )

          val newMetaData =
            new UnprefixedAttribute(
              attributeToNormalize,
              normalizedUrl,
              normalizedRemainingAttributes
            )

          (id, newMetaData, remainingEventAttributes)

        case UnprefixedAttribute(name, attributeValue, _) if name.startsWith("on") && extractInlineJavaScript =>
          val updatedEventAttributes =
            EventAttribute(name.substring(2), attributeValue.text) ::
            remainingEventAttributes

          (id, normalizedRemainingAttributes, updatedEventAttributes)

        case UnprefixedAttribute("id", attributeValue, _) =>
          val idValue = Option(attributeValue.text).filter(_.nonEmpty)

          (idValue, attributes.copy(normalizedRemainingAttributes), remainingEventAttributes)

        case _ =>
          (id, attributes.copy(normalizedRemainingAttributes), remainingEventAttributes)
      }
    }
  }

  // Given an element id and the `EventAttribute`s to apply to elements with
  // that id, return a JsCmd that binds all those event handlers to that id.
  private[this] def jsForEventAttributes(elementId: String, eventAttributes: List[EventAttribute]): JsCmd = {
    eventAttributes.map {
      case EventAttribute(name, handlerJs) =>
        Call(
          "lift.onEvent",
          elementId,
          name,
          AnonFunc("event", JsRaw(handlerJs).cmd)
        ).cmd
    }.foldLeft(Noop)(_ & _)
  }

  private[http] def normalizeElementAndAttributes(
    element: Elem,
    attributeToNormalize: String,
    contextPath: String,
    shouldRewriteUrl: Boolean,
    extractEventJavaScript: Boolean
  ): NodeAndEventJs = {
    val (id, normalizedAttributes, eventAttributes) =
      normalizeUrlAndExtractEvents(
        attributeToNormalize,
        element.attributes,
        contextPath,
        shouldRewriteUrl,
        extractEventJavaScript
      )

    val attributesIncludingEventsAsData =
      LiftRules.attributeForRemovedEventAttributes match {
        case Some(attribute) if eventAttributes.nonEmpty =>
          val removedAttributes = eventAttributes.map {
            case EventAttribute(event, _) =>
              s"on$event"
          }
          new UnprefixedAttribute(attribute, removedAttributes.mkString(" "), normalizedAttributes)

        case _ =>
          normalizedAttributes
      }

    id.map { foundId =>
      NodeAndEventJs(
        element.copy(attributes = attributesIncludingEventsAsData),
        jsForEventAttributes(foundId, eventAttributes)
      )
    } getOrElse {
      if (eventAttributes.nonEmpty) {
        val generatedId = s"lift-event-js-${nextFuncName}"

        NodeAndEventJs(
          element.copy(attributes = new UnprefixedAttribute("id", generatedId, attributesIncludingEventsAsData)),
          jsForEventAttributes(generatedId, eventAttributes)
        )
      } else {
        NodeAndEventJs(
          element.copy(attributes = attributesIncludingEventsAsData),
          Noop
        )
      }
    }
  }

  private[http] def normalizeNode(
    node: Node,
    contextPath: String,
    stripComments: Boolean,
    extractEventJavaScript: Boolean
  ): Option[NodeAndEventJs] = {
    node match {
      case element: Elem =>
        val (attributeToFix, shouldRewriteUrl) =
          element.label match {
            case "form" =>
              ("action", true)

            case "a" =>
              ("href", true)
            case "link" =>
              ("href", false)

            case "script" =>
              ("src", false)
            case _ =>
              ("src", true)
          }

        Some(
          normalizeElementAndAttributes(
            element,
            attributeToFix,
            contextPath,
            shouldRewriteUrl,
            extractEventJavaScript
          )
        )

      case _: Comment if stripComments =>
        None

      case otherNode =>
        Some(NodeAndEventJs(otherNode, Noop))
    }
  }

  /**
   * Normalizes `nodes` to adjust URLs with the given `contextPath`, stripping
   * comments if `stripComments` is `true`, and extracting event JavaScript
   * into the `js` part of the returned `NodesAndEventJs` if
   * `extractEventJavaScript` is `true`.
   *
   * See `[[LiftMerge.merge]]` for sample usage.
   */
  def normalizeHtmlAndEventHandlers(
    nodes: NodeSeq,
    contextPath: String,
    stripComments: Boolean,
    extractEventJavaScript: Boolean
  ): NodesAndEventJs = {
    nodes.foldLeft(NodesAndEventJs(Vector[Node](), Noop)) { (soFar, nodeToNormalize) =>
      normalizeNode(nodeToNormalize, contextPath, stripComments, extractEventJavaScript).map {
        case NodeAndEventJs(normalizedElement: Elem, js: JsCmd) =>
          val NodesAndEventJs(normalizedChildren, childJs) =
            normalizeHtmlAndEventHandlers(
              normalizedElement.child,
              contextPath,
              stripComments,
              extractEventJavaScript
            )

          soFar
            .append(js)
            .append(normalizedElement.copy(child = normalizedChildren), childJs)

        case node =>
          soFar.append(node)
      } getOrElse {
        soFar
      }
    }
  }
}
