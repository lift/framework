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
package wizard {

import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.net.liftweb.mapper._
import Helpers._
import _root_.scala.xml._
import _root_.scala.reflect.Manifest

object WizardRules extends Factory with FormVendor {
  val dbConnectionsForTransaction: FactoryMaker[List[ConnectionIdentifier]] =
  new FactoryMaker[List[ConnectionIdentifier]](() => Nil) {}

  private def m[T](implicit man: Manifest[T]): Manifest[T] = man

  val allTemplatePath: FactoryMaker[List[String]] = new FactoryMaker[List[String]](() => List("templates-hidden", "wizard-all")) {}
  private object currentWizards extends SessionVar[Set[String]](Set())

  private[wizard] def registerWizardSession(): String = {
    S.synchronizeForSession {
      val ret = Helpers.nextFuncName
      currentWizards.set(currentWizards.is + ret)
      ret
    }
  }

  private[wizard] def isValidWizardSession(id: String): Boolean =
    S.synchronizeForSession {
      currentWizards.is.contains(id)
    }

  private[wizard] def deregisterWizardSession(id: String) {
    S.synchronizeForSession {
      currentWizards.set(currentWizards.is - id)
    }
  }
}

//case class WizardFieldInfo(field: FieldIdentifier, text: NodeSeq, help: Box[NodeSeq], input: Box[NodeSeq])

trait Wizard extends StatefulSnippet with Factory with ScreenWizardRendered {
  def dispatch = {
    case _ => ignore => this.toForm
  }

  implicit def elemInABox(in: Elem): Box[Elem] = Full(in)

  @volatile private[this] var _screenList: List[Screen] = Nil
  private object ScreenVars extends RequestVar[Map[String, (NonCleanAnyVar[_], Any)]](Map())
  protected object CurrentScreen extends RequestVar[Box[Screen]](calcFirstScreen)
  private object PrevSnapshot extends RequestVar[Box[WizardSnapshot]](Empty)
  protected object Referer extends WizardVar[String](S.referer openOr "/")
  protected object OnFirstScreen extends RequestVar[Boolean](true)
  private object FirstTime extends WizardVar[Boolean](true)
  private object CurrentSession extends WizardVar[String](WizardRules.registerWizardSession())


  def noticeTypeToAttr(screen: AbstractScreen): Box[NoticeType.Value => MetaData] = {
    screen.inject[NoticeType.Value => MetaData] or
    inject[NoticeType.Value => MetaData] or
    WizardRules.inject[NoticeType.Value => MetaData] or
    screen.noticeTypeToAttr(screen)
  }


  def toForm = {
    Referer.is // touch to capture the referer
    CurrentSession.is

    if (FirstTime) {
      FirstTime.set(false)
      val localSnapshot = createSnapshot
      S.seeOther(S.uri, () => localSnapshot.restore)
    }

    val nextId = Helpers.nextFuncName
    val prevId = Helpers.nextFuncName
    val cancelId = Helpers.nextFuncName

    val theScreen = currentScreen openOr {
      WizardRules.deregisterWizardSession(CurrentSession.is)
      S.seeOther(Referer.is)
    }

    val (nextButton, finishButton) =
    if (!theScreen.isLastScreen)
      (Full(theScreen.nextButton % ("onclick" -> ("document.getElementById(" + nextId.encJs + ").submit()"))), Empty)
    else
      (Empty, Full(theScreen.finishButton % ("onclick" -> ("document.getElementById(" + nextId.encJs + ").submit()"))))

    val prevButton: Box[Elem] = if (OnFirstScreen) Empty else
      Full(theScreen.prevButton % ("onclick" -> ("document.getElementById(" + prevId.encJs + ").submit()")))

    val cancelButton: Elem = theScreen.cancelButton % ("onclick" -> ("document.getElementById(" + cancelId.encJs + ").submit()"))


    val url = S.uri

  renderAll(
    CurrentScreen.is.map(s => Text((s.myScreenNum + 1).toString)), //currentScreenNumber: Box[NodeSeq],
          Full(Text(screenCount.toString)), //screenCount: Box[NodeSeq],
          wizardTop, // wizardTop: Box[Elem],
          theScreen.screenTop, //screenTop: Box[Elem],
          theScreen.screenFields.flatMap(f =>
        if (f.show_?) List(ScreenFieldInfo(f, f.displayHtml, f.helpAsHtml, f.toForm)) else Nil), //fields: List[ScreenFieldInfo],
            prevButton, // prev: Box[Elem],
            Full(cancelButton), // cancel: Box[Elem],
            nextButton, // next: Box[Elem],
            finishButton, //finish: Box[Elem],
            theScreen.screenBottom, // screenBottom: Box[Elem],
            wizardBottom, //wizardBottom: Box[Elem],
            nextId -> (() => {
      this.nextScreen
      if (currentScreen.isEmpty) S.seeOther(Referer.is)
    }), // nextId: (String, () => Unit),
            Full(prevId -> (() => {this.prevScreen})), // prevId: Box[(String, () => Unit)],
            cancelId -> (() => { WizardRules.deregisterWizardSession(CurrentSession.is)}), //cancelId: (String, () => Unit),
            theScreen)
  }


  protected def allTemplatePath: List[String] = WizardRules.allTemplatePath.vend

  protected def allTemplate: NodeSeq = TemplateFinder.findAnyTemplate(allTemplatePath) openOr allTemplateNodeSeq

  /**
   * What additional attributes should be put on the
   */
  protected def formAttrs: MetaData = scala.xml.Null

  protected def wizardTop: Box[Elem] = None

  protected def wizardBottom: Box[Elem] = None

  class WizardSnapshot(private[wizard] val screenVars: Map[String, (NonCleanAnyVar[_], Any)],
                       val currentScreen: Box[Screen],
                       private[wizard] val snapshot: Box[WizardSnapshot],
                       private val firstScreen: Boolean) extends Snapshot {
    def restore() {
      registerThisSnippet();      
      ScreenVars.set(screenVars)
      CurrentScreen.set(currentScreen)
      PrevSnapshot.set(snapshot)
      OnFirstScreen.set(firstScreen)
      if (!WizardRules.isValidWizardSession(CurrentSession.is)) {
        S.seeOther(Referer.is)
      }
    }
  }

  private def _register(screen: Screen) {
    _screenList = _screenList ::: List(screen)
  }

  def dbConnections: List[ConnectionIdentifier] = WizardRules.dbConnectionsForTransaction.vend

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

  def nextButton: Elem = <button>{S.??("Next")}</button>

  def prevButton: Elem = <button>{S.??("Previous")}</button>

  def cancelButton: Elem = <button>{S.??("Cancel")}</button>

  def finishButton: Elem = <button>{S.??("Finish")}</button>

  def currentScreen: Box[Screen] = CurrentScreen.is

  def createSnapshot = new WizardSnapshot(ScreenVars.is, CurrentScreen.is, PrevSnapshot.is, OnFirstScreen.is)

  /**
   * This method will be called within a transactional block when the last screen is completed
   */
  protected def finish(): Unit

  def nextScreen {
    for{
      screen <- CurrentScreen.is
    } {
      screen.validate match {
        case Nil =>
          val snapshot = createSnapshot
          PrevSnapshot.set(Full(snapshot))
          val nextScreen = screen.nextScreen
          CurrentScreen.set(screen.nextScreen)
          OnFirstScreen.set(false)

          nextScreen match {
            case Empty =>
              def useAndFinish(in: List[ConnectionIdentifier]) {
                in match {
                  case Nil =>
                    WizardRules.deregisterWizardSession(CurrentSession.is)
                    finish()

                  case x :: xs => DB.use(x) {
                    conn =>
                        useAndFinish(xs)
                  }
                }
              }
              useAndFinish(dbConnections)

            case _ =>
          }
        case xs => S.error(xs)
      }
    }
  }

  def prevScreen {
    for{
      snapshot <- PrevSnapshot.is
    } {
      snapshot.restore()
    }
  }

  protected def vendForm[T](implicit man: Manifest[T]): Box[(T, T => Unit) => NodeSeq] = Empty

  /**
   * By default, are all the fields on all the screen in this wizardn on the confirm screen?
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
    trait Field extends super.Field {
      /**
       * Is this field on the confirm screen
       */
      def onConfirm_? = Screen.this.onConfirm_?

      override protected def otherFuncVendors(what: Manifest[ValueType]):
      Box[(ValueType, ValueType => Unit) => NodeSeq] =
        Wizard.this.vendForm(manifest) or WizardRules.vendForm(manifest)
    }

    Wizard.this._register(this)

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

    override protected def wasInitialized(name: String): Boolean = {
      val bn = name + "_inited_?"
      val old: Boolean = WizardVarHandler.get(bn) openOr false
      WizardVarHandler.set(bn, this, true)
      old
    }

    override protected def testWasSet(name: String): Boolean = {
      val bn = name + "_inited_?"
      WizardVarHandler.get(name).isDefined || (WizardVarHandler.get(bn) openOr false)
    }

    /**
     * Different Vars require different mechanisms for synchronization. This method implements
     * the Var specific synchronization mechanism
     */
    def doSync[F](f: => F): F = f // no sync necessary for RequestVars... always on the same thread
  }


  private[wizard] object WizardVarHandler {
    def get[T](name: String): Box[T] =
      ScreenVars.is.get(name).map(_._2.asInstanceOf[T])


    def set[T](name: String, from: WizardVar[_], value: T): Unit =
      ScreenVars.set(ScreenVars.is + (name -> (from, value)))

    def clear(name: String): Unit =
      ScreenVars.set(ScreenVars.is - name)
  }
}

}
}
