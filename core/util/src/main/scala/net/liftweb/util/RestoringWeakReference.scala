/*
 * Copyright 2006-2012 WorldWide Conferencing, LLC
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
package util

import scala.ref.WeakReference

/**
 * `RestoringWeakReference` contains a `scala.ref.WeakReference` that,
 * after it has been nulled out, uses a restorer function to restore the
 * value.  This can be used for data that can afford to be evicted by
 * the garbage collector, but will be needed later. One good example is
 * Lift form callbacks, which may need the value of an object, but where
 * you don't necessarily want to be retaining the object indefinitely
 * while someone is on a page in the face of GC contention.
 *
 * You can use `RestoringWeakReference` in a couple of basic ways:
 *
 * {{{
 *   val ref = RestoringWeakReference(() => User.find(id))
 * }}}
 * In this situation, the `RestoringWeakReference` will immediately call
 * the passed function to provide the starting value for the reference,
 * and then the same function will be used if the reference is evicted.
 *
 * {{{
 *   val ref = RestoringWeakReference(starter, () => User.find(id))
 * }}}
 * Here, starter is an `Option[User]` and `User.find` returns an
 * `Option[User]`. The first parameter will be used to initialize the
 * weak reference, while the function will be used to restore the value
 * if the reference is evicted.
 *
 * {{{
 *   val baseRef = new WeakReference(starter)
 *   val ref = new RestoringWeakReference(baseRef, () => User.find(id))
 * }}}
 * If you already have a `WeakReference` instance, you can instantiate
 * `RestoringWeakReference` directly and pass that reference as the
 * starting value, as well.
 *
 * In all these cases, you use `ref.value` to get a hold of the value.
 * That function will return the value if it is available, and, if not,
 * it will restore the WeakReference and then return the value.
 */
class RestoringWeakReference[T <: AnyRef](private var reference:WeakReference[T], restorer:()=>T) {
  def apply() : T = {
    val existing = reference.get
    if (! existing.isDefined) {
      restoreReference
      apply()
    } else {
      existing.get
    }
  }

  private def restoreReference = {
    reference = new WeakReference(restorer())
  }
}
object RestoringWeakReference {
  def apply[T <: AnyRef](restorer:()=>T) = {
	new RestoringWeakReference(new WeakReference(restorer()), restorer)
  }
  def apply[T <: AnyRef](starter:T, restorer:()=>T) = {
	new RestoringWeakReference(new WeakReference(starter), restorer)
  }
}

