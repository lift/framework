package net.liftweb.http

import net.liftweb.util.{HasCalcDefaultValue, LiftValue}

object LiftRulesGuardedSetting {
  type StackTrace = Array[StackTraceElement]
}

import LiftRulesGuardedSetting._

/**
  * TODO
  * @param name
  * @param default
  * @tparam T
  */
class LiftRulesGuardedSetting[T](val name: String, val default: T) extends LiftValue[T] with HasCalcDefaultValue[T] {
  private[this] var v: T = default
  private[this] var lastSet: Option[StackTrace] = None
  private[this] var lastRead: Option[StackTrace] = None

  private[this] def writeAfterReadMessage =
    s"LiftRules.$name was set AFTER already being read! " +
    s"Review the stacktrace below to see where the value was last read. "

  private[this] def writeAfterBootMessage =
    s"LiftRules.$name set after Lift finished booting. " +
    s"Review the stacktrace below to see where settings are being changed after boot time. "

  private[this] def writtenTwiceMessage1(newVal: T) =
    s"LiftRules.$name was set to $v then later set to $newVal. " +
    s"This could potentially cause your Lift application to run in an inconsistent state. " +
    s"Review the stacktrace below to see where LiftRules.$name was first set to $v. "

  private[this] def writtenTwiceMessage2(newVal: T) =
    s"Review the stacktrace below to see where LiftRules.$name was later set to $newVal. "

  private[this] def trimmedStackTrace(t: Throwable): StackTrace = {
    val toIgnore = Set("LiftRulesGuardedSetting", "LiftValue")
    t.getStackTrace.dropWhile(e => toIgnore.find(e.getClassName contains _).isDefined)
  }

  private[this] def currentStackTrace: StackTrace = trimmedStackTrace(new Exception)

  override def set(value: T): T = {
    lastSet.foreach { case stackTrace if v != value =>
      val e1 = LiftRulesSettingWrittenTwiceException(name, stackTrace, writtenTwiceMessage1(value))
      LiftRules.settingsExceptionFunc.get.apply(e1)

      val e2 = LiftRulesSettingWrittenTwiceException(name, stackTrace, writtenTwiceMessage2(value))
      LiftRules.settingsExceptionFunc.get.apply(e2)
    }

    if(LiftRules.doneBoot) {
      val e = LiftRulesSettingWrittenAfterBootException(name, currentStackTrace, writeAfterBootMessage)
      LiftRules.settingsExceptionFunc.get.apply(e)
    }

    // TODO: Skip if the value is the same?
    lastRead.foreach { stackTrace =>
      val e2 = LiftRulesSettingWrittenAfterReadException(name, stackTrace, writeAfterReadMessage)
      LiftRules.settingsExceptionFunc.get.apply(e2)
    }

    lastSet = Some(currentStackTrace)
    v = value
    v
  }

  override def get: T = {
    if(!LiftRules.doneBoot) lastRead = Some(currentStackTrace)
    v
  }

  override protected def calcDefaultValue: T = default
}



abstract class LiftRulesSettingException(settingName: String, stackTrace: StackTrace, message: String) extends Exception(message) {
  setStackTrace(stackTrace)
}

case class LiftRulesSettingWrittenAfterReadException(settingName: String, stackTrace: StackTrace, message: String)
  extends LiftRulesSettingException(settingName, stackTrace, message)

case class LiftRulesSettingWrittenAfterBootException(settingName: String, stackTrace: StackTrace, message: String)
  extends LiftRulesSettingException(settingName, stackTrace, message)

case class LiftRulesSettingWrittenTwiceException(settingName: String, stackTrace: StackTrace, message: String)
  extends LiftRulesSettingException(settingName, stackTrace, message)



