/*
 * Copyright 2009-2014 WorldWide Conferencing, LLC
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

import net.liftweb._
import http._
import js._
import JsCmds._

import common._
import util._
import Helpers._
import scala.xml._
import scala.reflect.Manifest

object WizardRules extends Factory with FormVendor {
  val dbConnectionsForTransaction: FactoryMaker[List[LoanWrapper]] =
    new FactoryMaker[List[LoanWrapper]](() => Nil) {}

  private def m[T](implicit man: Manifest[T]): Manifest[T] = man

  val allTemplatePath: FactoryMaker[List[String]] = new FactoryMaker[List[String]](() => List("templates-hidden", "wizard-all")) {}

  private object currentWizards extends SessionVar[Set[String]](Set())

  private[http] def registerWizardSession(): String = {
    S.synchronizeForSession {
      val ret = Helpers.nextFuncName
      currentWizards.set(currentWizards.is + ret)
      ret
    }
  }

  private[http] def isValidWizardSession(id: String): Boolean =
    S.synchronizeForSession {
      currentWizards.is.contains(id)
    }

  private[http] def deregisterWizardSession(id: String) {
    S.synchronizeForSession {
      currentWizards.set(currentWizards.is - id)
    }
  }
}

/**
 * A wizard allows you to create a multi-screen set of input forms
 * with back-button support and state support
 */
trait Wizard extends StatefulSnippet with Factory with ScreenWizardRendered {
  def dispatch = {
    case _ => template => {
      _defaultXml.set(template)
      this.toForm
    }
  }

  /**
   * Holds the template passed via the snippet for the duration
   * of the request
   */
  protected object _defaultXml extends TransientRequestVar[NodeSeq](NodeSeq.Empty) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  /**
   * the NodeSeq passed as a parameter when the snippet was invoked
   */
  protected def defaultXml: NodeSeq = _defaultXml.get

  implicit def elemInABox(in: Elem): Box[Elem] = Full(in)

  @volatile private[this] var _screenList: List[Screen] = Nil

  private object ScreenVars extends TransientRequestVar[Map[String, (NonCleanAnyVar[_], Any)]](Map()) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  protected object CurrentScreen extends WizardVar[Box[Screen]]({
    val screen = calcFirstScreen
    screen.foreach(_.transitionIntoFrom(Empty))
    screen.foreach(_.enterScreen())
    screen
  }) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  private object PrevSnapshot extends TransientRequestVar[Box[WizardSnapshot]](Empty) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  protected object Referer extends WizardVar[String](calcReferer) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  protected object Ajax_? extends WizardVar[Boolean](calcAjax) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  /**
   * A unique GUID for the form... this allows us to do an Ajax SetHtml
   * to replace the form
   */
  protected object FormGUID extends WizardVar[String](Helpers.nextFuncName) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  /**
   * What to do when the Screen is done.  By default, will
   * do a redirect back to Whence, but you can change this behavior,
   * for example, put up some other Ajax thing or alternatively,
   * remove the form from the screen.
   */
  protected object AjaxOnDone extends WizardVar[JsCmd](calcAjaxOnDone) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  protected object OnFirstScreen extends TransientRequestVar[Boolean](true) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  private object FirstTime extends WizardVar[Boolean](true) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  private object CurrentSession extends WizardVar[String](WizardRules.registerWizardSession()) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  protected object VisitedScreens extends WizardVar[Vector[Screen]](Vector()) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }


  def noticeTypeToAttr(screen: AbstractScreen): Box[NoticeType.Value => MetaData] = {
    screen.inject[NoticeType.Value => MetaData] or
      inject[NoticeType.Value => MetaData] or
      WizardRules.inject[NoticeType.Value => MetaData] or
      screen.noticeTypeToAttr(screen)
  }

  /**
   * Override this method to do setup the first time the
   * screen is entered
   */
  protected def localSetup() {

  }

  override def formName: String = "wizard"

  def toForm: NodeSeq = {
    ScreenVars.is // initialize
    Referer.is // touch to capture the referer
    Ajax_?.is // capture the Ajax state
    CurrentSession.is
    FormGUID.is

    if (FirstTime) {
      FirstTime.set(false)

      localSetup()

      if (!ajaxForms_?) {
        val localSnapshot = createSnapshot
        // val notices = S.getAllNotices
        S.seeOther(S.uri, () => {
          // S.appendNotices(notices)
          localSnapshot.restore
        })
      }
    }

    val form = renderHtml()
    if (ajaxForms_?) wrapInDiv(form) else form
  }

  protected def submitOrAjax(id: String): String =
    (if (ajaxForms_?) {
      SHtml.makeAjaxCall(LiftRules.jsArtifacts.serialize(id)).toJsCmd
    } else {
      "document.getElementById(" + id.encJs + ").submit()"
    })

  protected def renderHtml(): NodeSeq = {
    val nextId = Helpers.nextFuncName
    val prevId = Helpers.nextFuncName
    val cancelId = Helpers.nextFuncName

    val theScreen: Screen = currentScreen openOr {
      WizardRules.deregisterWizardSession(CurrentSession.is)
      S.seeOther(Referer.is) // this should never happen
    }

    val (nextButton, finishButton) =
      if (!theScreen.isLastScreen)
        (Full(theScreen.nextButton %
          ("onclick" -> submitOrAjax(nextId))), Empty)
      else
        (Empty, Full(theScreen.finishButton %
          ("onclick" -> submitOrAjax(nextId))))

    val prevButton: Box[Elem] = if (OnFirstScreen) Empty
    else
      Full(theScreen.prevButton % ("onclick" -> submitOrAjax(prevId)))

    val cancelButton: Elem = theScreen.cancelButton %
      ("onclick" -> submitOrAjax(cancelId))


    val url = S.uri

    val extraFields: List[ScreenFieldInfo] =
      if (theScreen.confirmScreen_?) {

        for {
          screen <- VisitedScreens.is.toList
          field <- screen.screenFields.collect {
            case c: ConfirmField => c
          } if field.show_? && field.onConfirm_?
        } yield ScreenFieldInfo(field, field.displayHtml, Empty,
          Full(field.asHtml))
      } else Nil

    renderAll(
      CurrentScreen.is.map(s => Text((s.myScreenNum + 1).toString)), //currentScreenNumber: Box[NodeSeq],
      Full(Text(screenCount.toString)), //screenCount: Box[NodeSeq],
      wizardTop, // wizardTop: Box[Elem],
      theScreen.screenTop, //screenTop: Box[Elem],
      extraFields :::
        theScreen.screenFields.flatMap(f =>
          if (f.show_?) List(ScreenFieldInfo(f, f.displayHtml, f.helpAsHtml, f.toForm)) else Nil), //fields: List[ScreenFieldInfo],
      prevButton, // prev: Box[Elem],
      Full(cancelButton), // cancel: Box[Elem],
      nextButton, // next: Box[Elem],
      finishButton, //finish: Box[Elem],
      theScreen.screenBottom, // screenBottom: Box[Elem],
      wizardBottom, //wizardBottom: Box[Elem],
      nextId -> (() => {
        this.nextScreen()
        // if (currentScreen.isEmpty) S.seeOther(Referer.is)
      }), // nextId: (String, () => Unit),
      Full(prevId -> (() => {
        this.prevScreen
      })), // prevId: Box[(String, () => Unit)],
      cancelId -> (() => {
        WizardRules.deregisterWizardSession(CurrentSession.is); redirectBack()
      }), //cancelId: (String, () => Unit),
      theScreen, ajaxForms_?)
  }


  protected def allTemplatePath: List[String] = WizardRules.allTemplatePath.vend

  protected def allTemplate: NodeSeq = Templates(allTemplatePath) openOr allTemplateNodeSeq

  /**
   * What additional attributes should be put on the
   */
  protected def formAttrs: MetaData = scala.xml.Null

  protected def wizardTop: Box[Elem] = None

  protected def wizardBottom: Box[Elem] = None

  private def doTransition(from: Box[Screen], to: Box[Screen]) {
    to.foreach(_.enterScreen())

    (from, to) match {
      case (Full(old), Full(cur)) if old eq cur => {
        /* do nothing */
      }
      case (Full(old), Full(cur)) => {
        old.transitionOutOfTo(Full(cur))
        cur.transitionIntoFrom(Full(old))
      }

      case (Full(old), _) => old.transitionOutOfTo(Empty)
      case (_, Full(cur)) => cur.transitionIntoFrom(Empty)
      case _ =>
    }
  }

  class WizardSnapshot(private[http] val screenVars: Map[String, (NonCleanAnyVar[_], Any)],
                       val currentScreen: Box[Screen],
                       private[http] val snapshot: Box[WizardSnapshot],
                       private val firstScreen: Boolean) extends Snapshot {
    def restore() {
      registerThisSnippet();
      ScreenVars.set(screenVars)
      if (CurrentScreen.set_?) {
        doTransition(CurrentScreen.get, currentScreen)
      } else {
        currentScreen.foreach(_.transitionIntoFrom(Empty))
        currentScreen.foreach(_.enterScreen())
      }

      CurrentScreen.set(currentScreen)
      PrevSnapshot.set(snapshot)
      OnFirstScreen.set(firstScreen)
      if (!WizardRules.isValidWizardSession(CurrentSession.is)) {
        S.seeOther(Referer.is) // FIXME Wizard
      }
    }
  }

  private def _register(screen: Screen) {
    _screenList = _screenList ::: List(screen)
  }

  def dbConnections: List[LoanWrapper] = WizardRules.dbConnectionsForTransaction.vend

  /**
   * The ordered list of Screens
   */
  def screens: List[Screen] = _screenList

  /**
   * Total number of screens in the wizard
   */
  lazy val screenCount = screens.size

  /**
   * Given the current screen, what's the next screen?
   */
  def calcScreenAfter(which: Screen): Box[Screen] =
    screens.dropWhile(_ ne which).drop(1).headOption


  /**
   * What's the first screen in this wizard
   */
  def calcFirstScreen: Box[Screen] = screens.headOption

  def nextButton: Elem = <button>
    {S.?("Next")}
  </button>

  def prevButton: Elem = <button>
    {S.?("Previous")}
  </button>

  def cancelButton: Elem = <button>
    {S.?("Cancel")}
  </button>

  def finishButton: Elem = <button>
    {S.?("Finish")}
  </button>

  def currentScreen: Box[Screen] = CurrentScreen.is

  def createSnapshot = {
    val cs = CurrentScreen.is
    val prev = PrevSnapshot.is
    val onFirst = OnFirstScreen.is
    new WizardSnapshot(ScreenVars.is, cs, prev, onFirst)
  }

  /**
   * This method will be called within a transactional block when the last screen is completed
   */
  protected def finish(): Unit

  def nextScreen(): JsCmd = {
    (for {
      screen <- CurrentScreen.is
    } yield {
      screen.validate match {
        case Nil => {
          val snapshot = createSnapshot
          PrevSnapshot.set(Full(snapshot))
          val nextScreen = screen.nextScreen
          CurrentScreen.is.foreach {
            s => VisitedScreens.set(VisitedScreens :+ s)
          }
          doTransition(CurrentScreen.get, nextScreen)
          CurrentScreen.set(nextScreen)
          OnFirstScreen.set(false)

          nextScreen match {
            case Empty =>
              def useAndFinish(in: List[LoanWrapper]) {
                in match {
                  case Nil => {
                    WizardRules.deregisterWizardSession(CurrentSession.is)
                    VisitedScreens.foreach {
                      s => s.finish()
                    }
                    finish()
                    VisitedScreens.foreach {
                      s => s.postFinish()
                    }
                  }

                  case x :: xs => x.apply {
                      useAndFinish(xs)
                  }
                }
              }
              useAndFinish(dbConnections)
              if (ajaxForms_?) {
                AjaxOnDone.is
              } else {
                Noop
              }

            case _ => SetHtml(FormGUID, renderHtml())
          }
        }
        case xs => {
          S.error(xs)
          SetHtml(FormGUID, renderHtml())
        }
      }
    }) openOr AjaxOnDone.is
  }

  def prevScreen(): JsCmd = {
    (for {
      snapshot <- PrevSnapshot.is
    } yield {
      val cur = if (CurrentScreen.set_?) CurrentScreen.get else Empty
      snapshot.restore()

      if (CurrentScreen.set_?) {
        doTransition(cur, CurrentScreen.get)
      } else {
        cur.foreach(_.transitionIntoFrom(Empty))
        cur.foreach(_.enterScreen())
      }

      SetHtml(FormGUID, renderHtml())
    }) openOr AjaxOnDone.is
  }

  protected def vendForm[T](implicit man: Manifest[T]): Box[(T, T => Any) => NodeSeq] = Empty

  /**
   * By default, are all the fields on all the screen in this wizard on the confirm screen?
   */
  def onConfirm_? = true

  /**
   * Define a screen within this wizard
   */
  trait Screen extends AbstractScreen {
    val myScreenNum = screens.length

    /**
     * The name of the screen.  Override this to change the screen name
     */
    override def screenName: String = "Screen " + (myScreenNum + 1)

    def nextButton: Elem = Wizard.this.nextButton

    def prevButton: Elem = Wizard.this.prevButton

    override def cancelButton: Elem = Wizard.this.cancelButton

    override def finishButton: Elem = Wizard.this.finishButton

    def nextScreen: Box[Screen] = calcScreenAfter(this)

    def isLastScreen = nextScreen.isEmpty

    /**
     * A notification that we are transitioning into
     * this screen.  Override this method to perform
     * some screen-specific actions
     *
     * @param from the screen we're coming from
     */
    def transitionIntoFrom(from: Box[Screen]) {
    }

    /**
     * A notification that we are transitioning out of
     * this screen.  Override this method to perform
     * some screen-specific actions
     *
     * @param to the screen we're transitioning to
     */
    def transitionOutOfTo(to: Box[Screen]) {
    }

    /**
     * By default, are all the fields on this screen on the confirm screen?
     */
    def onConfirm_? = Wizard.this.onConfirm_?


    /**
     * Is this screen a confirm screen?
     */
    def confirmScreen_? = false

    /**
     * Define a field within the screen
     */
    trait Field extends super.Field with ConfirmField {
      /**
       * Is this field on the confirm screen
       */
      override def onConfirm_? = Screen.this.onConfirm_?

      override protected def otherFuncVendors(what: Manifest[ValueType]):
      Box[(ValueType, ValueType => Any) => NodeSeq] =
        Wizard.this.vendForm(manifest) or WizardRules.vendForm(manifest)
    }

    Wizard.this._register(this)

    private object _touched extends WizardVar(false) {
      override lazy val __nameSalt = Helpers.nextFuncName
    }

    /**
     * override this method if there's a screen-specific thing
     * to do on finish.  This method is called before the main Wizard's
     * finish method
     */
    def finish() {
    }

    /**
     * override this method if there's a screen-specific thing
     * to do on finish.  This method is executed after the main Wizards
     * finish() method.
     */
    def postFinish() {
    }

    private[http] def enterScreen() {
      if (!_touched) {
        _touched.set(true)
        localSetup()
      }
    }

    protected def vendAVar[T](dflt: => T): NonCleanAnyVar[T] = Wizard.this.vendAVar[T](dflt)
  }

  protected def vendAVar[T](dflt: => T): NonCleanAnyVar[T] = new WizardVar[T](dflt) {
    override protected def __nameSalt = randomString(20)
  }

  /**
   * Keep request-local information around without the nastiness of naming session variables
   * or the type-unsafety of casting the results.
   * RequestVars share their value through the scope of the current HTTP
   * request. They have no value at the beginning of request servicing
   * and their value is discarded at the end of request processing. They
   * are helpful to share values across many snippets.
   *
   * @param dflt - the default value of the session variable
   */
  abstract class WizardVar[T](dflt: => T) extends NonCleanAnyVar[T](dflt) {
    override protected def findFunc(name: String): Box[T] = WizardVarHandler.get(name)

    override protected def setFunc(name: String, value: T): Unit = WizardVarHandler.set(name, this, value)

    override protected def clearFunc(name: String): Unit = WizardVarHandler.clear(name)

    override protected def wasInitialized(name: String, bn: String): Boolean = {
      val old: Boolean = WizardVarHandler.get(bn) openOr false
      WizardVarHandler.set(bn, this, true)
      old
    }

    override protected def testWasSet(name: String, bn: String): Boolean = {
      WizardVarHandler.get(name).isDefined || (WizardVarHandler.get(bn) openOr false)
    }

    /**
     * Different Vars require different mechanisms for synchronization. This method implements
     * the Var specific synchronization mechanism
     */
    def doSync[F](f: => F): F = f

    // no sync necessary for RequestVars... always on the same thread
  }


  private[http] object WizardVarHandler {
    def get[T](name: String): Box[T] =
      ScreenVars.is.get(name).map(_._2.asInstanceOf[T])


    def set[T](name: String, from: WizardVar[_], value: T): Unit =
      ScreenVars.set(ScreenVars.is + (name -> (from, value)))

    def clear(name: String): Unit =
      ScreenVars.set(ScreenVars.is - name)
  }

}
