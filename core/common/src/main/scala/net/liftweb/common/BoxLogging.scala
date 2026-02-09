/*
 * Copyright 2007-2026 Lift Committers and Contributors
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
package common

/**
 * Mix this trait in to get some low-cost implicits for logging boxes
 * easily. The consumer will need to implement a `[[logBoxError]]`
 * method to log messages with an optional `Throwable`, as well as its related
 * friends for trace, debug, info, and warn levels. This allows abstracting out
 * where and what the actual logger is.
 *
 * With this mixed in, boxes will have `[[logFailure]]` and
 * `[[logFailure]]` methods. The first logs all `[[Failure]]`s as well
 * as `[[Empty]]`. The second logs only `Failure`s and
 * `[[ParamFailure]]`s, treating `Empty` as a valid value. These both log their
 * respective items at ERROR level. You can also use `traceLog*`, `debugLog*`,
 * `infoLog*`, and `warnLog*` if you want to log at other levels (e.g., you can
 * use `infoLogFailure` to log an `Empty` or `Failure` at `INFO` level).
 *
 * All of these return the box unchanged, so you can continue to use it
 * in `for` comprehensions, call `openOr` on it, etc.
 *
 * There is an implementation for anyone who wants to use Lift's
 * `[[Loggable]]` trait called `[[LoggableBoxLogging]]`. Another implementaiton
 * is available for use with a plain SLF4J logger, `SLF4JBoxLogging`. You can
 * also implement a version for any other logging adapter. Lastly, you can
 * simply import `BoxLogging._` to get the methods available at a top level;
 * however, note that using them this way will lose information about where the
 * log message came from.
 *
 * Here is an example of how you might use this in system that executes a third-
 * party service and notifies another system of failures.
 *
 * {{{
 * val systemResult: Box[ServiceReturn] =
 *   system
 *     .executeService(true, requester)
 *     .map(...)
 *     .logFailure("Failed to execute service") match {
 *       case Full(content) =>
 *         content
 *       case failure: Failure =>
 *         otherSystem.notifyFailure(failure)
 *       case Empty =>
 *         otherSystem.notifyFailure(Failure("No idea what happened."))
 *     }
 * }}}
 */
trait BoxLogging {
  private[this] def logBoxError(message: String): Unit = {
    logBoxError(message, None)
  }
  private[this] def logBoxWarn(message: String): Unit = {
    logBoxWarn(message, None)
  }
  private[this] def logBoxInfo(message: String): Unit = {
    logBoxInfo(message, None)
  }
  private[this] def logBoxDebug(message: String): Unit = {
    logBoxDebug(message, None)
  }
  private[this] def logBoxTrace(message: String): Unit = {
    logBoxTrace(message, None)
  }

  /**
   * Called with an error message and possibly a throwable that caused
   * the error in question. Should ERROR log the message and the throwable.
   *
   * Exists in order to abstract away logger abstractions. Abstractception,
   * as it were.
   */
  protected def logBoxError(message: String, throwable: Option[Throwable]): Unit
  /**
   * Called with a warn message and possibly a throwable that caused the issue
   * in question. Should WARN log the message and the throwable.
   *
   * Exists in order to abstract away logger abstractions. Abstractception,
   * as it were.
   */
  protected def logBoxWarn(message: String, throwable: Option[Throwable]): Unit
  /**
   * Called with an info message and possibly a throwable that caused the issue
   * in question. Should INFO log the message and the throwable.
   *
   * Exists in order to abstract away logger abstractions. Abstractception,
   * as it were.
   */
  protected def logBoxInfo(message: String, throwable: Option[Throwable]): Unit
  /**
   * Called with a debug message and possibly a throwable that caused the issue
   * in question. Should DEBUG log the message and the throwable.
   *
   * Exists in order to abstract away logger abstractions. Abstractception,
   * as it were.
   */
  protected def logBoxDebug(message: String, throwable: Option[Throwable]): Unit
  /**
   * Called with a trace message and possibly a throwable that caused the issue
   * in question. Should TRACE log the message and the throwable.
   *
   * Exists in order to abstract away logger abstractions. Abstractception,
   * as it were.
   */
  protected def logBoxTrace(message: String, throwable: Option[Throwable]): Unit

  implicit class LogEmptyOrFailure[T](val box: Box[T]) extends AnyRef {
    private def doLog(message: String, logFn: (String, Option[Throwable])=>Unit, onEmpty: ()=>Unit) = {
      box match {
        case ParamFailure(failureMessage, exception, Full(chain), param) =>
          logFn(s"$message: $failureMessage with param $param", exception)
          chain.logFailure(s"$failureMessage with param $param caused by", logFn)

        case ParamFailure(failureMessage, exception, _, param) =>
          logFn(s"$message: $failureMessage with param $param", exception)

        case Failure(failureMessage, exception, Full(chain)) =>
          logFn(s"$message: $failureMessage", exception)
          chain.logFailure(s"$failureMessage caused by", logFn)

        case Failure(failureMessage, exception, _) =>
          logFn(s"$message: $failureMessage", exception)

        case Empty =>
          onEmpty()

        case _: Full[_] => // nothing to log
      }
    }

    def logFailure(message: String, logFn: (String, Option[Throwable])=>Unit): Unit = {
      doLog(message, logFn, ()=>logFn(s"$message: Box was Empty.", None))
    }

    def logEmptyBox(message: String): Box[T] = {
      doLog(message, logBoxError, ()=>logBoxError(s"$message: Box was Empty."))
      box
    }

    def warnLogEmptyBox(message: String): Box[T] = {
      doLog(message, logBoxWarn, ()=>logBoxWarn(s"$message: Box was Empty."))
      box
    }

    def infoLogEmptyBox(message: String): Box[T] = {
      doLog(message, logBoxInfo, ()=>logBoxInfo(s"$message: Box was Empty."))
      box
    }

    def debugLogEmptyBox(message: String): Box[T] = {
      doLog(message, logBoxDebug, ()=>logBoxDebug(s"$message: Box was Empty."))
      box
    }

    def traceLogEmptyBox(message: String): Box[T] = {
      doLog(message, logBoxTrace, ()=>logBoxTrace(s"$message: Box was Empty."))
      box
    }

    def logFailure(message: String): Box[T] = {
      doLog(message, logBoxError, ()=>())
      box
    }

    def warnLogFailure(message: String): Box[T] = {
      doLog(message, logBoxWarn, ()=>())
      box
    }

    def infoLogFailure(message: String): Box[T] = {
      doLog(message, logBoxInfo, ()=>())
      box
    }

    def debugLogFailure(message: String): Box[T] = {
      doLog(message, logBoxDebug, ()=>())
      box
    }

    def traceLogFailure(message: String): Box[T] = {
      doLog(message, logBoxTrace, ()=>())
      box
    }
  }
}

/**
 * A version of `[[BoxLogging]]` with a default implementation of
 * `logBoxError` that logs to the logger provided by Lift's
 * `[[Loggable]]`.
 */
trait LoggableBoxLogging extends BoxLogging with Loggable {
  protected def logBoxError(message: String, throwable: Option[Throwable]) = {
    throwable match {
      case Some(throwable) =>
        logger.error(message, throwable)
      case _ =>
        logger.error(message)
    }
  }
  protected def logBoxWarn(message: String, throwable: Option[Throwable]) = {
    throwable match {
      case Some(throwable) =>
        logger.warn(message, throwable)
      case _ =>
        logger.warn(message)
    }
  }
  protected def logBoxInfo(message: String, throwable: Option[Throwable]) = {
    throwable match {
      case Some(throwable) =>
        logger.info(message, throwable)
      case _ =>
        logger.info(message)
    }
  }
  protected def logBoxDebug(message: String, throwable: Option[Throwable]) = {
    throwable match {
      case Some(throwable) =>
        logger.debug(message, throwable)
      case _ =>
        logger.debug(message)
    }
  }
  protected def logBoxTrace(message: String, throwable: Option[Throwable]) = {
    throwable match {
      case Some(throwable) =>
        // Below, disambiguate between the trace helper for tracing a value and
        // the one that takes a throwable.
        logger.trace(message: AnyRef, throwable)
      case _ =>
        logger.trace(message)
    }
  }
}

trait SLF4JBoxLogging extends BoxLogging {
  protected val logger: org.slf4j.Logger

  protected def logBoxError(message: String, throwable: Option[Throwable]) = {
    throwable match {
      case Some(throwable) =>
        logger.error(message, throwable)
      case _ =>
        logger.error(message)
    }
  }
  protected def logBoxWarn(message: String, throwable: Option[Throwable]) = {
    throwable match {
      case Some(throwable) =>
        logger.warn(message, throwable)
      case _ =>
        logger.warn(message)
    }
  }
  protected def logBoxInfo(message: String, throwable: Option[Throwable]) = {
    throwable match {
      case Some(throwable) =>
        logger.info(message, throwable)
      case _ =>
        logger.info(message)
    }
  }
  protected def logBoxDebug(message: String, throwable: Option[Throwable]) = {
    throwable match {
      case Some(throwable) =>
        logger.debug(message, throwable)
      case _ =>
        logger.debug(message)
    }
  }
  protected def logBoxTrace(message: String, throwable: Option[Throwable]) = {
    throwable match {
      case Some(throwable) =>
        logger.trace(message, throwable)
      case _ =>
        logger.trace(message)
    }
  }
}

/**
 * A convenience singleton for `[[BoxLogging]]` that makes `logFailure`
 * and `logFailure` available for all code after it's been imported
 * as `import com.elemica.common.BoxLogging._`. Logging done this way
 * will come from `BoxLogging`, not from the class where the box was
 * logged.
 */
object BoxLogging extends LoggableBoxLogging
