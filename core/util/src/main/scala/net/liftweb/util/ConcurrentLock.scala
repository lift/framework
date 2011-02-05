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
package util {

import _root_.java.util.concurrent.locks._

class ConcurrentLock extends ReentrantReadWriteLock {

  def read[T](f: => T): T = {
    readLock().lock()
    try {
      f
    } finally {
      readLock().unlock()
    }
  }

  def write[T](f: => T): T = {
    writeLock().lock()
    try {
      f
    } finally {
      writeLock().unlock()
    }
  }

  def upgrade[T](f: => T): T = {
    readLock().unlock
    writeLock().lock
    try {
      f
    } finally {
      writeLock().unlock
      readLock().lock
    }
  }
}

}
}
