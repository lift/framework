package net.liftweb.json
package converters

import scalajson.ast.unsafe
import scalajson.ast

/**
 * Provides explicit conversion functions for converting between the standard and unsafe Scala
 * AST's. To use this in your code simply add an import to the top of your file.
 *
 * {{{
 * import net.liftweb.json.converters.ScalaJsonConverters._
 * }}}
 *
 * Then the functions provided will be available for all of your AST conversion needs..
 */
object ScalaJsonConverters {
  implicit class LiftJValueWithConverters(liftValue: JValue) {
    def toStandardScalaAST: Option[ast.JValue] =
      toUnsafeScalaAST.map(_.toStandard)

    def toUnsafeScalaAST: Option[unsafe.JValue] =
      LiftToScalaJsonConversions.toScalaAST(liftValue)
  }

  implicit class StandardASTWithConverters(scalaValue: ast.JValue) {
    def toLiftAST: JValue =
      ScalaJsonToLiftConversions.toLiftAST(scalaValue.toUnsafe)
  }

  implicit class UnsafeASTWithConverters(scalaValue: unsafe.JValue) {
    def toLiftAST: JValue =
      ScalaJsonToLiftConversions.toLiftAST(scalaValue)
  }
}
