package net.liftweb
package http

import org.specs2.execute.{Result, AsResult}
import org.specs2.specification.Scope
import org.specs2.mutable.Specification

import common.{Box, Empty}

/**
 * Used for stacking other providers.
 */
trait BaseScope extends Scope {
  // Base scope does nothing special
}

/**
 * Given an instance method or variable `rules`, wraps the spec in a setup of
 * that rules instance as the one used by Lift for the duration of the spec.
 */
trait LiftRulesSetup extends Scope {
  def rules: LiftRules

  // Initialize rules when this scope is created
  LiftRulesMocker.devTestLiftRulesInstance.doWith(rules) {}
}

/**
 * Given an instance method or variable `session` and `req`, wraps the spec in a setup
 * with S initialized for the duration of the spec.
 */
trait SSetup extends Scope {
  def session: LiftSession
  def req: Box[Req]

  // Initialize S when this scope is created
  def initS[T](test: => T): T = {
    S.init(req, session) {
      test
    }
  }
}

/**
 * Wraps a spec in a context where `rules` are the Lift rules in effect.
 */
class WithRules(val rules: LiftRules) extends BaseScope with LiftRulesSetup

/**
 * Wraps a spec in a context where `rules` are the Lift rules in effect, `session`
 * is the current Lift session, and `req`, if specified, is the current request.
 */
class WithLiftContext(val rules: LiftRules, val session: LiftSession, val req: Box[Req] = Empty)
  extends Scope with LiftRulesSetup with SSetup {

  LiftRulesMocker.devTestLiftRulesInstance.doWith(rules) {
    S.init(req, session) {}
  }
}
