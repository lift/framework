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
package rssfeed {

import _root_.scala.xml._
import _root_.java.net.{URLConnection, URL}
import _root_.scala.collection.mutable._
// import net.liftweb.util.IoHelpers._
import net.liftweb.util.Helpers._

object RSSFeed {
  /**
   * Renders an RSS feed using a list
   */
  def apply(feedUrl: String) = new RSSFeed().render(feedUrl)
}

class RSSFeed {
  /**
   * Renders an RSS feed using a list
   */
  def render(feedUrl: String): NodeSeq = {
    val feed = getFeed(feedUrl)

    var src = new Queue[Node]()

    src += <li class="rsswidgettitle"><b><a href={ (feed \ "channel" \ "link").text }>{ ( feed \ "channel" \ "title" ).text }</a></b></li>

    for (c <- findElems(feed){_.label == "item"}) {
      src += <li class="rsswidgetitem"><a href={(c \\ "link").text}>{(c \\ "title").text}</a></li>
    }

    <div class="rsswidget"><ul>{src}</ul></div>
  }

  /**
   * Returns the feed as a plain XML
   */
  def getFeed(feedUrl: String): Elem = {
    val u = new URL(feedUrl)
    val con = u.openConnection
    val is = con.getInputStream
    doClose(is) {
      XML.load(is)
    }
  }

}

}
}
}
