/*
 * Copyright 2009-2010 WorldWide Conferencing, LLC
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
package actor {

import common._

class LAFuture[T] /*extends Future[T]*/ {
  @volatile private[this] var item: T = _
  @volatile private[this] var satisfied = false

  def satisfy(value: T): Unit = synchronized {
    if (!satisfied) {
      item = value
      satisfied = true
    }
    notifyAll()
  }

  def get: T = synchronized {
    if (satisfied) item
    else {
      this.wait()
      if (satisfied) item
      else get
    }
  }

  def get(timeout: Long): Box[T] = synchronized {
    if (satisfied) Full(item)
    else {
      try {
        wait(timeout)
        if (satisfied) Full(item)
        else Empty
      } catch {
        case _: InterruptedException => Empty
      }
    }
  }
}

}
}
