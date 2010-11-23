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
package http {

import _root_.net.liftweb.common._
import _root_.scala.xml.{Node, Group, NodeSeq}
import _root_.net.liftweb.util._
import _root_.net.liftweb.util.Helpers._
import java.io.{Writer, InputStream}

/**
 * This trait encapsulates the various choices related to
 * parsing and emitting HTML/XHTML
 */
trait HtmlProperties {
  /**
   * When we emit the HTML, what DocType will be emitted
   */
  def docType: Box[String]

  /**
   * Creates a new instance of HtmlProperties with the
   * docType property changed
   */
  def setDocType(newDocType: Box[String]) = {
    val old = this
    new HtmlProperties {
      def docType = newDocType
      def encoding = old.encoding
      def htmlOutputHeader = old.htmlOutputHeader
      def htmlParser = old.htmlParser
      def htmlWriter = old.htmlWriter
      def html5FormsSupport = old.html5FormsSupport
      def maxOpenRequests = old.maxOpenRequests
      def userAgent = old.userAgent
    }
  }

  /**
   * When we output the HTML, what encoding will be emitted
   */
  def encoding: Box[String]

  /**
   * Creates a new instance of HtmlProperties with the
   * encoding property changed
   */
  def setEncoding(newEncoding: Box[String]) = {
    val old = this
    new HtmlProperties {
      def docType = old.docType
      def encoding = newEncoding
      def htmlOutputHeader = old.htmlOutputHeader
      def htmlParser = old.htmlParser
      def htmlWriter = old.htmlWriter
      def html5FormsSupport = old.html5FormsSupport
      def maxOpenRequests = old.maxOpenRequests
      def userAgent = old.userAgent
    }
  }


  /**
   * For XHTML, the Encoding appears before the
   * DocType, except if you're writing to IE6,
   * so, rather than having a hard-coded calculation
   * we allow the calculation to be done here.
   */
  def htmlOutputHeader: Box[String]

  /**
   * Creates a new instance of HtmlProperties with the
   * htmlOutputHeader property changed
   */
  def setHtmlOutputHeader(newHeader: Box[String]) = {
    val old = this
    new HtmlProperties {
      def docType = old.docType
      def encoding = old.encoding
      def htmlOutputHeader = newHeader
      def htmlParser = old.htmlParser
      def htmlWriter = old.htmlWriter
      def html5FormsSupport = old.html5FormsSupport
      def maxOpenRequests = old.maxOpenRequests
      def userAgent = old.userAgent
    }
  }

  /*
  /**
   * Creates a new instance of HtmlProperties with the
   * htmlOutputHeader property changed
   */
  def setHtmlOutputHeader(newHeader: Box[String]) = {
    val old = this
    new HtmlProperties {
      def docType = old.docType
      def encoding = old.encoding
      def htmlOutputHeader = old.htmlOutputHeader
      def htmlParser = old.htmlParser
      def htmlWriter = old.htmlWriter
      def html5FormsSupport = old.html5FormsSupport
      def maxOpenRequests = old.maxOpenRequests
      def userAgent = old.userAgent
    }
  }
  */

  /**
   * How are we parsing incoming files into a NodeSeq?
   * This will likely point to either PCDataXmlParser.apply or
   * Html5.parse
   */
  def htmlParser: InputStream => Box[NodeSeq]

  /**
   * Given a NodeSeq and a Writer, convert the output
   * to the writer.
   */
  def htmlWriter: (NodeSeq, Writer) => Unit

  /**
   * Are there HTML5 forms support?
   */
  def html5FormsSupport: Boolean

  /**
   * What is the maximum number of open HTTP
   * requests.
   */
  def maxOpenRequests: Int

  /**
   * What's the UserAgent that was used to create
   * this HtmlChoice
   */
  def userAgent: String
}

}
}
