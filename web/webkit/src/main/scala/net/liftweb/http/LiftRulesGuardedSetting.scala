package net.liftweb.http

import net.liftweb.util.{HasCalcDefaultValue, LiftValue}

object LiftRulesGuardedSetting {
  type StackTrace = Array[StackTraceElement]

  /**
    * Base class for all possible violations which LiftRulesGuardedSetting warns you about
    * @param settingName
    * @param stackTrace
    * @param message
    */
  abstract class SettingViolation(settingName: String, stackTrace: StackTrace, message: String) extends Serializable {
    /**
      * Converts this violation into an Exception which can be handed to a logger for clean message printing
      * @return
      */
    def toException: Exception = {
      val e = new Exception(message)
      e.setStackTrace(stackTrace)
      e
    }
  }

  /**
    * Indicates that a LiftRulesGuardedSetting was written after it had already been read.
    * @param settingName
    * @param stackTrace
    * @param message
    */
  case class SettingWrittenAfterRead(settingName: String, stackTrace: StackTrace, message: String)
    extends SettingViolation(settingName, stackTrace, message)

  /**
    * Indicates that a LiftRulesGuardedSetting was written after Lift finished booting.
    * @param settingName
    * @param stackTrace
    * @param message
    */
  case class SettingWrittenAfterBoot(settingName: String, stackTrace: StackTrace, message: String)
    extends SettingViolation(settingName, stackTrace, message)

  /**
    * Indicates that a LiftRulesGuardedSetting was set to two different values.
    * @param settingName
    * @param stackTrace
    * @param message
    */
  case class SettingWrittenTwice(settingName: String, stackTrace: StackTrace, message: String)
    extends SettingViolation(settingName, stackTrace, message)
}

import LiftRulesGuardedSetting._

/**
  * This class encapsulates a mutable LiftRules setting which guards its value against changes which can produce
  * unexpected results in a Lift application.
  *
  * @param name
  * @param default
  * @tparam T
  */
class LiftRulesGuardedSetting[T](val name: String, val default: T) extends LiftValue[T] with HasCalcDefaultValue[T] {
  private[this] var v: T = default
  private[this] var lastSet: Option[StackTrace] = None
  private[this] var lastRead: Option[StackTrace] = None

  private[this] def writeAfterReadMessage =
    s"LiftRules.$name was set after already being read! " +
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
      val e1 = SettingWrittenTwice(name, stackTrace, writtenTwiceMessage1(value))
      LiftRules.guardedSettingViolationFunc.get.apply(e1)

      val e2 = SettingWrittenTwice(name, currentStackTrace, writtenTwiceMessage2(value))
      LiftRules.guardedSettingViolationFunc.get.apply(e2)
    }

    if(LiftRules.doneBoot) {
      val e = SettingWrittenAfterBoot(name, currentStackTrace, writeAfterBootMessage)
      LiftRules.guardedSettingViolationFunc.get.apply(e)
    }

    // TODO: Skip if the value is the same?
    lastRead.foreach { stackTrace =>
      val e2 = SettingWrittenAfterRead(name, stackTrace, writeAfterReadMessage)
      LiftRules.guardedSettingViolationFunc.get.apply(e2)
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



