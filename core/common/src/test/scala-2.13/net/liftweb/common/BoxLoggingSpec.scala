package net.liftweb
package common

import org.slf4j.{Logger=>SLF4JLogger}

import org.scalamock.specs2.MockContext
import org.specs2.mutable.Specification

class BoxLoggingSpec extends Specification {
  class MockBoxLoggingClass extends BoxLogging {
    var loggedErrors = List[(String, Option[Throwable])]()
    var loggedWarns = List[(String, Option[Throwable])]()
    var loggedInfos = List[(String, Option[Throwable])]()
    var loggedDebugs = List[(String, Option[Throwable])]()
    var loggedTraces = List[(String, Option[Throwable])]()

    protected def logBoxError(message: String, throwable: Option[Throwable]): Unit = {
      loggedErrors ::= (message, throwable)
    }
    protected def logBoxWarn(message: String, throwable: Option[Throwable]): Unit = {
      loggedWarns ::= (message, throwable)
    }
    protected def logBoxInfo(message: String, throwable: Option[Throwable]): Unit = {
      loggedInfos ::= (message, throwable)
    }
    protected def logBoxDebug(message: String, throwable: Option[Throwable]): Unit = {
      loggedDebugs ::= (message, throwable)
    }
    protected def logBoxTrace(message: String, throwable: Option[Throwable]): Unit = {
      loggedTraces ::= (message, throwable)
    }
  }

  "BoxLogging" should {
    "when logging empty boxes" in {
      def verifyContentList(list: List[(String, Option[Throwable])]) = {
        list must beLike {
          case (paramFailure4, None) ::
               (paramFailure3, None) ::
               (paramFailure2, None) ::
               (paramFailure1, Some(paramExp)) ::
               (chained1, None) ::
               (chained2, None) ::
               (level1, None) ::
               (level2, Some(exp2)) ::
               (level3, None) ::
               (level4, Some(exp4)) ::
               (emptyMessage, None) ::
               (fullParamMessage, Some(paramException)) ::
               (paramMessage, None) ::
               (exceptedMessage, Some(failureException)) ::
               (failureMessage, None) ::
               Nil =>
            (failureMessage must startWith("Second")) and
              (failureMessage must contain("Failed")) and
              (exceptedMessage must startWith("Third")) and
              (exceptedMessage must contain("Excepted")) and
              (failureException.getMessage must_== "uh-oh") and
              (paramMessage must startWith("Fourth")) and
              (paramMessage must contain("ParamFailed")) and
              (paramMessage must contain("BoxLoggingSpec")) and
              (fullParamMessage must startWith("Fifth")) and
              (fullParamMessage must contain("ParamExcepted")) and
              (fullParamMessage must contain("BoxLoggingSpec")) and
              (paramException.getMessage must_== "param uh-oh") and
              (emptyMessage must startWith("Sixth")) and
              (emptyMessage must contain("Empty")) and
              (level1 must contain("Failure level 3 caused by: Failure level 4")) and
              (level2 must contain("Failure level 2 caused by: Failure level 3")) and
              (exp2 must beAnInstanceOf[IllegalArgumentException]) and
              (level3 must contain("Failure level 1 caused by: Failure level 2")) and
              (level4 must contain("Multilevel failure: Failure level 1")) and
              (exp4 must beAnInstanceOf[NullPointerException]) and
              (chained1 must contain("Chained failure caused by: Boom")) and
              (chained2 must contain("Chain all failures: Chained failure")) and
              (paramFailure4 must contain("Param Failure lvl 3 with param Param 3 caused by: Param Failure lvl 4 with param Param 4")) and
              (paramFailure3 must contain("Failure lvl 2 caused by: Param Failure lvl 3 with param Param 3")) and
              (paramFailure2 must contain("Param Failure lvl 1 with param Param 1 caused by: Failure lvl 2")) and
              (paramFailure1 must contain("Param failure: Param Failure lvl 1 with param Param 1")) and
              (paramExp must beAnInstanceOf[IllegalArgumentException])
        }
      }

      "log correctly on ERROR level" in {
        val results =
          new MockBoxLoggingClass {
            Full("Not empty").logEmptyBox("First")
            Failure("Failed").logEmptyBox("Second")
            Failure("Excepted", Full(new Exception("uh-oh")), Empty).logEmptyBox("Third")
            ParamFailure("ParamFailed", this).logEmptyBox("Fourth")
            ParamFailure(
              "ParamExcepted",
              Full(new Exception("param uh-oh")),
              Empty,
              this
            ).logEmptyBox("Fifth")
            (Empty).logEmptyBox("Sixth")
            Failure(
              "Failure level 1", Full(new NullPointerException), Full(Failure(
                "Failure level 2", Empty, Full(Failure(
                  "Failure level 3", Full(new IllegalArgumentException), Full(Failure(
                    "Failure level 4"
                  )))
                ))
              )
            ).logEmptyBox("Multilevel failure")
            (Failure("Boom") ?~! "Chained failure").logEmptyBox("Chain all failures")
            ParamFailure(
              "Param Failure lvl 1", Full(new IllegalArgumentException), Full(Failure(
                "Failure lvl 2", Empty, Full(ParamFailure(
                  "Param Failure lvl 3", Empty, Full(ParamFailure(
                    "Param Failure lvl 4",
                    "Param 4"
                  )),
                  "Param 3"
                ))
              )),
              "Param 1"
            ).logEmptyBox("Param failure")
          }

        verifyContentList(results.loggedErrors)
      }

      "log correctly on WARN level" in {
        val results =
          new MockBoxLoggingClass {
            Full("Not empty").warnLogEmptyBox("First")
            Failure("Failed").warnLogEmptyBox("Second")
            Failure("Excepted", Full(new Exception("uh-oh")), Empty).warnLogEmptyBox("Third")
            ParamFailure("ParamFailed", this).warnLogEmptyBox("Fourth")
            ParamFailure(
              "ParamExcepted",
              Full(new Exception("param uh-oh")),
              Empty,
              this
            ).warnLogEmptyBox("Fifth")
            (Empty).warnLogEmptyBox("Sixth")
            Failure(
              "Failure level 1", Full(new NullPointerException), Full(Failure(
                "Failure level 2", Empty, Full(Failure(
                  "Failure level 3", Full(new IllegalArgumentException), Full(Failure(
                    "Failure level 4"
                  )))
                ))
              )
            ).warnLogEmptyBox("Multilevel failure")
            (Failure("Boom") ?~! "Chained failure").warnLogEmptyBox("Chain all failures")
            ParamFailure(
              "Param Failure lvl 1", Full(new IllegalArgumentException), Full(Failure(
                "Failure lvl 2", Empty, Full(ParamFailure(
                  "Param Failure lvl 3", Empty, Full(ParamFailure(
                    "Param Failure lvl 4",
                    "Param 4"
                  )),
                  "Param 3"
                ))
              )),
              "Param 1"
            ).warnLogEmptyBox("Param failure")
          }

        verifyContentList(results.loggedWarns)
      }

      "log correctly on INFO level" in {
        val results =
          new MockBoxLoggingClass {
            Full("Not empty").infoLogEmptyBox("First")
            Failure("Failed").infoLogEmptyBox("Second")
            Failure("Excepted", Full(new Exception("uh-oh")), Empty).infoLogEmptyBox("Third")
            ParamFailure("ParamFailed", this).infoLogEmptyBox("Fourth")
            ParamFailure(
              "ParamExcepted",
              Full(new Exception("param uh-oh")),
              Empty,
              this
            ).infoLogEmptyBox("Fifth")
            (Empty).infoLogEmptyBox("Sixth")
            Failure(
              "Failure level 1", Full(new NullPointerException), Full(Failure(
                "Failure level 2", Empty, Full(Failure(
                  "Failure level 3", Full(new IllegalArgumentException), Full(Failure(
                    "Failure level 4"
                  )))
                ))
              )
            ).infoLogEmptyBox("Multilevel failure")
            (Failure("Boom") ?~! "Chained failure").infoLogEmptyBox("Chain all failures")
            ParamFailure(
              "Param Failure lvl 1", Full(new IllegalArgumentException), Full(Failure(
                "Failure lvl 2", Empty, Full(ParamFailure(
                  "Param Failure lvl 3", Empty, Full(ParamFailure(
                    "Param Failure lvl 4",
                    "Param 4"
                  )),
                  "Param 3"
                ))
              )),
              "Param 1"
            ).infoLogEmptyBox("Param failure")
          }

        verifyContentList(results.loggedInfos)
      }

      "log correctly on DEBUG level" in {
        val results =
          new MockBoxLoggingClass {
            Full("Not empty").debugLogEmptyBox("First")
            Failure("Failed").debugLogEmptyBox("Second")
            Failure("Excepted", Full(new Exception("uh-oh")), Empty).debugLogEmptyBox("Third")
            ParamFailure("ParamFailed", this).debugLogEmptyBox("Fourth")
            ParamFailure(
              "ParamExcepted",
              Full(new Exception("param uh-oh")),
              Empty,
              this
            ).debugLogEmptyBox("Fifth")
            (Empty).debugLogEmptyBox("Sixth")
            Failure(
              "Failure level 1", Full(new NullPointerException), Full(Failure(
                "Failure level 2", Empty, Full(Failure(
                  "Failure level 3", Full(new IllegalArgumentException), Full(Failure(
                    "Failure level 4"
                  )))
                ))
              )
            ).debugLogFailure("Multilevel failure")
            (Failure("Boom") ?~! "Chained failure").debugLogFailure("Chain all failures")
            ParamFailure(
              "Param Failure lvl 1", Full(new IllegalArgumentException), Full(Failure(
                "Failure lvl 2", Empty, Full(ParamFailure(
                  "Param Failure lvl 3", Empty, Full(ParamFailure(
                    "Param Failure lvl 4",
                    "Param 4"
                  )),
                  "Param 3"
                ))
              )),
              "Param 1"
            ).debugLogFailure("Param failure")
          }

        verifyContentList(results.loggedDebugs)
      }

      "log correctly on TRACE level" in {
        val results =
          new MockBoxLoggingClass {
            Full("Not empty").traceLogEmptyBox("First")
            Failure("Failed").traceLogEmptyBox("Second")
            Failure("Excepted", Full(new Exception("uh-oh")), Empty).traceLogEmptyBox("Third")
            ParamFailure("ParamFailed", this).traceLogEmptyBox("Fourth")
            ParamFailure(
              "ParamExcepted",
              Full(new Exception("param uh-oh")),
              Empty,
              this
            ).traceLogEmptyBox("Fifth")
            (Empty).traceLogEmptyBox("Sixth")
            Failure(
              "Failure level 1", Full(new NullPointerException), Full(Failure(
                "Failure level 2", Empty, Full(Failure(
                  "Failure level 3", Full(new IllegalArgumentException), Full(Failure(
                    "Failure level 4"
                  )))
                ))
              )
            ).traceLogEmptyBox("Multilevel failure")
            (Failure("Boom") ?~! "Chained failure").traceLogEmptyBox("Chain all failures")
            ParamFailure(
              "Param Failure lvl 1", Full(new IllegalArgumentException), Full(Failure(
                "Failure lvl 2", Empty, Full(ParamFailure(
                  "Param Failure lvl 3", Empty, Full(ParamFailure(
                    "Param Failure lvl 4",
                    "Param 4"
                  )),
                  "Param 3"
                ))
              )),
              "Param 1"
            ).traceLogEmptyBox("Param failure")
          }

        verifyContentList(results.loggedTraces)
      }
    }

    "when logging only failures" in {
      def verifyContentList(list: List[(String, Option[Throwable])]) = {
        list must beLike {
          case (fullParamMessage, Some(paramException)) ::
                  (paramMessage, None) ::
                  (exceptedMessage, Some(failureException)) ::
                  (failureMessage, None) ::
                  Nil =>
            (failureMessage must startWith("Second")) and
              (failureMessage must contain("Failed")) and
              (exceptedMessage must startWith("Third")) and
              (exceptedMessage must contain("Excepted")) and
              (failureException.getMessage must_== "uh-oh") and
              (paramMessage must startWith("Fourth")) and
              (paramMessage must contain("ParamFailed")) and
              (paramMessage must contain("BoxLoggingSpec")) and
              (fullParamMessage must startWith("Fifth")) and
              (fullParamMessage must contain("ParamExcepted")) and
              (fullParamMessage must contain("BoxLoggingSpec")) and
              (paramException.getMessage must_== "param uh-oh")
        }
      }

      "log correctly on ERROR level" in {
        val results =
          new MockBoxLoggingClass {
            Full("Not empty").logFailure("First")
            Failure("Failed").logFailure("Second")
            Failure("Excepted", Full(new Exception("uh-oh")), Empty).logFailure("Third")
            ParamFailure("ParamFailed", this).logFailure("Fourth")
            ParamFailure(
              "ParamExcepted",
              Full(new Exception("param uh-oh")),
              Empty,
              this
            ).logFailure("Fifth")
            (Empty).logFailure("Sixth")
          }

        verifyContentList(results.loggedErrors)
      }

      "log correctly on WARN level" in {
        val results =
          new MockBoxLoggingClass {
            Full("Not empty").warnLogFailure("First")
            Failure("Failed").warnLogFailure("Second")
            Failure("Excepted", Full(new Exception("uh-oh")), Empty).warnLogFailure("Third")
            ParamFailure("ParamFailed", this).warnLogFailure("Fourth")
            ParamFailure(
              "ParamExcepted",
              Full(new Exception("param uh-oh")),
              Empty,
              this
            ).warnLogFailure("Fifth")
            (Empty).warnLogFailure("Sixth")
          }

        verifyContentList(results.loggedWarns)
      }

      "log correctly on INFO level" in {
        val results =
          new MockBoxLoggingClass {
            Full("Not empty").infoLogFailure("First")
            Failure("Failed").infoLogFailure("Second")
            Failure("Excepted", Full(new Exception("uh-oh")), Empty).infoLogFailure("Third")
            ParamFailure("ParamFailed", this).infoLogFailure("Fourth")
            ParamFailure(
              "ParamExcepted",
              Full(new Exception("param uh-oh")),
              Empty,
              this
            ).infoLogFailure("Fifth")
            (Empty).infoLogFailure("Sixth")
          }

        verifyContentList(results.loggedInfos)
      }

      "log correctly on DEBUG level" in {
        val results =
          new MockBoxLoggingClass {
            Full("Not empty").debugLogFailure("First")
            Failure("Failed").debugLogFailure("Second")
            Failure("Excepted", Full(new Exception("uh-oh")), Empty).debugLogFailure("Third")
            ParamFailure("ParamFailed", this).debugLogFailure("Fourth")
            ParamFailure(
              "ParamExcepted",
              Full(new Exception("param uh-oh")),
              Empty,
              this
            ).debugLogFailure("Fifth")
            (Empty).debugLogFailure("Sixth")
          }

        verifyContentList(results.loggedDebugs)
      }

      "log correctly on TRACE level" in {
        val results =
          new MockBoxLoggingClass {
            Full("Not empty").traceLogFailure("First")
            Failure("Failed").traceLogFailure("Second")
            Failure("Excepted", Full(new Exception("uh-oh")), Empty).traceLogFailure("Third")
            ParamFailure("ParamFailed", this).traceLogFailure("Fourth")
            ParamFailure(
              "ParamExcepted",
              Full(new Exception("param uh-oh")),
              Empty,
              this
            ).traceLogFailure("Fifth")
            (Empty).traceLogFailure("Sixth")
          }

        verifyContentList(results.loggedTraces)
      }
    }

    "when logging in a Loggable" in {
      import net.liftweb.common.Logger
      import org.slf4j.{Logger => SLF4JLogger}

      "log to the Lift logger" in new MockContext {
        val mockLogger = mock[SLF4JLogger]
        (mockLogger.isErrorEnabled: () => Boolean).expects().returning(true).anyNumberOfTimes()

        class MyLoggable extends LoggableBoxLogging {
          override val logger = new Logger {
            override protected def _logger = mockLogger
          }
        }

        (mockLogger.error(_: String)).expects(*).once()
        (mockLogger.error(_: String, _: Throwable)).expects(*, *).once()

        val result =
          new MyLoggable {
            Failure("Failed").logFailure("Second")
            Failure("Excepted", Full(new Exception("uh-oh")), Empty).logFailure("Third")
          }
      }
    }

    "when logging with in SLF4J context" in {
      import org.slf4j.{Logger => SLF4JLogger}

      "log to the SLF4J logger" in new MockContext {
        val mockLogger = mock[SLF4JLogger]
        (mockLogger.isErrorEnabled: () => Boolean).expects().returning(true).anyNumberOfTimes()

        class TestClass extends SLF4JBoxLogging {
          val logger = mockLogger
        }

        (mockLogger.error(_: String)).expects(*).once()
        (mockLogger.error(_: String, _: Throwable)).expects(*, *).once()

        new TestClass {
          Failure("Failed").logFailure("Second")
          Failure("Excepted", Full(new Exception("uh-oh")), Empty).logFailure("Third")
        }
      }
    }
  }
}
