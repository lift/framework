package net.liftweb.json
package converters

import scalajson.ast.unsafe
import scala.util.control.NonFatal

/**
 * Implements the conversions from the Scala Common JSON's unsafe AST to Lift-Json's AST.
 */
object ScalaJsonToLiftConversions {
  /**
   * Convert an unsafe AST to a Lift-Json AST.
   *
   * Since this conversion is from the unsafe AST, there is some amount of risk in invoking this
   * method since numbers in the unsafe AST are represented as strings. If those strings are
   * invalid numbers, you can expect to receive exceptions when you attempt to run this conversion.
   */
  def toLiftAST(input: unsafe.JValue): JValue = input match {
    case unsafe.JTrue =>
      JBool(true)

    case unsafe.JFalse =>
      JBool(false)

    case unsafe.JNull =>
      JNull

    case unsafe.JString(str) =>
      JString(str)

    case unsafe.JNumber(numberString) =>
      try {
        JInt(numberString.toInt)
      } catch {
        case NonFatal(e) =>
          JDouble(numberString.toDouble)
      }

    case unsafe.JArray(members) =>
      JArray(members.toList.map(toLiftAST))

    case unsafe.JObject(fields) =>
      JObject(convertFields(fields))
  }

  private def convertFields(input: Array[unsafe.JField]): List[JField] = {
    val resultingFields = for {
      scalafield <- input
      scalavalue = toLiftAST(scalafield.value)
    } yield {
      JField(scalafield.field, scalavalue)
    }

    resultingFields.toList
  }
}
