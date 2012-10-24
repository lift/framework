/*
 * Copyright 2011-2012 WorldWide Conferencing, LLC
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

import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.JsCmd
import net.liftweb.common._
import util._

import xml._

trait CssBoundLiftScreen extends LiftScreen with CssBoundScreen {
  protected object SavedDefaultXml extends ScreenVar[NodeSeq](defaultXml) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  protected object LocalAction extends TransientRequestVar[String]("") {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  protected object LocalActionRef extends ScreenVar[String](S.fmapFunc(setLocalAction _)(s => s)) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  protected object PrevId extends TransientRequestVar[Box[String]](Empty) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  protected object CancelId extends TransientRequestVar[String]("") {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  protected object LocalActions extends ScreenVar[Map[String, () => JsCmd]](Map[String, () => JsCmd]()) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  override def localSetup() {
    SavedDefaultXml.get
    LocalActionRef.get
  }

  override def allTemplate = SavedDefaultXml.get

  protected def defaultAllTemplate = super.allTemplate

  override protected def doFinish(): JsCmd= {
    val fMap: Map[String, () => JsCmd] = LocalActions.get
    if (! LocalAction.get.isEmpty)
      fMap.get(LocalAction.get) map (_()) getOrElse (
        throw new IllegalArgumentException("No local action available with that binding"))
    else {
      validate match {
        case Nil =>
          val snapshot = createSnapshot
          PrevSnapshot.set(Full(snapshot))
          finish()
          redirectBack()
        case xs => {
          S.error(xs)
          if (ajaxForms_?) {
            replayForm
          } else {
            Noop
          }
        }
      }
    }
  }

  protected def renderWithErrors(errors: List[FieldError]) {
    S.error(errors)
    AjaxOnDone.set(replayForm)
  }

  protected def renderFormCmd: JsCmd = SetHtml(FormGUID, renderHtml())

  protected def replayForm: JsCmd = renderFormCmd
}
