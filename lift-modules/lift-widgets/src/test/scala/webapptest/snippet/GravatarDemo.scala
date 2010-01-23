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

package webapptest {
package snippet {

import _root_.scala.xml.NodeSeq
import _root_.net.liftweb.widgets.gravatar.Gravatar

class GravatarDemo {

  def render(xhtml: NodeSeq) :NodeSeq = {
    // Gravatar("tyler.weir@gmail.com", 50)
    // Gravatar("tyler.weir@gmail.com", 48, "R")
    Gravatar("tyler.weir@gmail.com")
  }
}

}
}
