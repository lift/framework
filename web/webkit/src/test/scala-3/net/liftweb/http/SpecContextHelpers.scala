package net.liftweb
package http

import org.specs2.execute.{Result, AsResult}
import org.specs2.mutable.Specification

import common.{Box, Empty}

/**
 * Helper functions for wrapping test execution with Lift context.
 *
 * These functions properly wrap test code with ThreadLocal state management,
 * ensuring that LiftRules and S (session) scope remain active during test execution.
 */
object SpecContextHelpers {
  /**
   * Wraps test execution with LiftRules context.
   * The rules are active for the duration of the test execution.
   *
   * Example usage:
   * {{{
   * import SpecContextHelpers._
   *
   * "my test" in withLiftRules(testRules) {
   *   // test code here - LiftRules are available
   * }
   * }}}
   */
  def withLiftRules[T: AsResult](rules: LiftRules)(test: =>T): Result = {
    LiftRulesMocker.devTestLiftRulesInstance.doWith(rules) {
      AsResult(test)
    }
  }

  /**
   * Wraps test execution with both LiftRules and S (session) context.
   * Both the rules and S scope are active for the duration of the test execution.
   *
   * Example usage:
   * {{{
   * import SpecContextHelpers._
   *
   * "my test" in withLiftContext(testRules, testSession) {
   *   // test code here - LiftRules and S scope are available
   * }
   * }}}
   */
  def withLiftContext[T: AsResult](
    rules: LiftRules,
    session: LiftSession,
    req: Box[Req] = Empty
  )(test: =>T): Result = {
    LiftRulesMocker.devTestLiftRulesInstance.doWith(rules) {
      S.init(req, session) {
        AsResult(test)
      }
    }
  }
}
