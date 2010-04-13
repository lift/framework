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
package http {
package rest {

import _root_.scala.collection.mutable.{HashMap, ListBuffer}
import _root_.net.liftweb.common._
import _root_.net.liftweb.actor._

/**
 * Provides a generic way of sending asynchronous response to HTTP clients. If
 * the underlying web container does not support continuations the asynchronous
 * nature is achieved using locks.
 */
object RestContinuation {

  /**
   * Use this in DispatchPF for processing REST requests asynchronously. Note that
   * this must be called in a stateful context, therefore the S state must be a valid one.
   *
   * @param f - the user function that does the actual computation. This function
   *            takes one parameter which is the functino that must be invoked
   *            for returning the actual response to the client. Note that f function
   *            is invoked asynchronously in the context of a different thread.
   * 
   */
  def respondAsync(req: Req)(f: => Box[LiftResponse]): () => Box[LiftResponse] = {
    val store = ContinuationsStore.is
    val key = ContinuationKey(req.path, req.requestType)

    def handleNonContinuation: Continuation = {
      store.get(key) match {
        case None => 
          val cont = new Continuation {
          @volatile var timedOut = false
          val future = new LAFuture[Box[LiftResponse]]

          lazy val cometTimeout: Long = (LiftRules.cometRequestTimeout openOr 120) * 1000L
          
          var cachedResp: Box[LiftResponse] = null
          val resumeFunc: Box[LiftResponse] => Unit = { response => 
            if (timedOut){
              cachedResp = response
            } else {
              store -= key
            }
            future.satisfy(response)
          }
          
          LAScheduler.execute(() => resumeFunc(f))

          def tryRespond: Box[LiftResponse] = {
            if (cachedResp != null){
               val res = cachedResp
               cachedResp = null
               store -= key
               res
            } else {
              future.get(cometTimeout) match {
                case Full(resp) => 
                  cachedResp = null
                  store -= key; 
                  resp
                case _ => timedOut = true;
                  Full(EmptyResponse)
              }
            }
           }
        }
      
        store += (key -> cont)
        cont
        case Some(cont) => cont
      }
    }


    def handleContinuation: Continuation = {
      store.get(key) match {
        case None => 
          val cont = new Continuation {

            lazy val cometTimeout: Long = (LiftRules.cometRequestTimeout openOr 120) * 1000L
          
            var cachedResp: Box[LiftResponse] = null
            val resumeFunc: Box[LiftResponse] => Unit = { response => 
              val ok = req.request.resume(response.map((req, _)) openOr (req, EmptyResponse))
              if (!ok) {
                cachedResp = response
              } else {
                store -= key
              }
            }
       
            def tryRespond: Box[LiftResponse] = {
              if (cachedResp != null){
                val res = cachedResp
                cachedResp = null
                store -= key
                res
              } else {
                try {
                  req.request.suspend(cometTimeout)
                  Full(EmptyResponse)
                } finally {
                  LAScheduler.execute(() => resumeFunc(f))
                }
              }
            }
        }
      
        store += (key -> cont)
        cont
        case Some(cont) => cont
      }
   }

   val continuation = if (req.request.suspendResumeSupport_?) 
     handleContinuation
   else
     handleNonContinuation

   () => continuation tryRespond

   }
}


object ContinuationsStore extends 
  SessionVar[HashMap[ContinuationKey, Continuation]](new HashMap[ContinuationKey, Continuation])

trait Continuation {
  def tryRespond: Box[LiftResponse]
}
case class ContinuationKey(path: ParsePath, reqType: RequestType)

}
}
}
