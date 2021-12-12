package net.liftweb
package http

import org.specs2._
import execute.{Result, AsResult}
import mutable.Around

import common.{Box, Empty}

/**
 * Used for stacking other `around` providers like `[[LiftRulesSetup]]`.
 */
trait BaseAround extends Around {
  override def around[T: AsResult](test: =>T): Result = {
    AsResult(test)
  }
}

/**
 * Given an instance method or variable `rules`, wraps the spec in a setup of
 * that rules instance as the one used by Lift for the duration of the spec.
 */
trait LiftRulesSetup extends Around {
  def rules: LiftRules

  abstract override def around[T: AsResult](test: => T): Result = {
    super.around {
      LiftRulesMocker.devTestLiftRulesInstance.doWith(rules) {
        AsResult(test)
      }
    }
  }
}

/**
 * Given an instance method or variable `rules`, wraps the spec in a setup of
 * that rules instance as the one used by Lift for the duration of the spec.
 */
trait SSetup extends Around {
  def session: LiftSession
  def req: Box[Req]

  abstract override def around[T: AsResult](test: => T): Result = {
    super.around {
      S.init(req, session) {
        AsResult(test)
      }
    }
  }
}

/**
 * Wraps a spec in a context where `rules` are the Lift rules in effect.
 */
class WithRules(val rules: LiftRules) extends BaseAround with LiftRulesSetup

/**
 * Wraps a spec in a context where `rules` are the Lift rules in effect, `session`
 * is the current Lift session, and `req`, if specified, is the current request.
 */
class WithLiftContext(val rules: LiftRules, val session: LiftSession, val req: Box[Req] = Empty) extends BaseAround with LiftRulesSetup with SSetup
