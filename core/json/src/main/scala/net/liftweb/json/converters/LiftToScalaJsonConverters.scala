package net.liftweb.json
package converters

import scalajson.ast.unsafe

/**
 * Implements the conversions from a Lift-Json JValue to the Scala Common JSON AST.
 */
object LiftToScalaJsonConversions {
  /**
   * Convert a Lift-Json JValue to an unsafe JValue in the Scala Common JSON AST.
   *
   * This conversion returns an Option since there is no equivalent for JNothing in the Scala
   * Common JSON AST.
   */
  def toScalaAST(input: JValue): Option[unsafe.JValue] = input match {
    case JString(string) =>
      Some(unsafe.JString(string))

    case JBool(true) =>
      Some(unsafe.JTrue)

    case JBool(false) =>
      Some(unsafe.JFalse)

    case JDouble(number) =>
      Some(unsafe.JNumber(number))

    case JInt(number) =>
      Some(unsafe.JNumber(number))

    case JNull =>
      Some(unsafe.JNull)

    case JArray(items) =>
      Some(unsafe.JArray(items.flatMap(toScalaAST).toArray))

    case JObject(fields) =>
      Some(unsafe.JObject(convertFields(fields)))

    case JNothing =>
      None
  }

  private[this] def convertFields(input: List[JField]): Array[unsafe.JField] = {
    val resultingFields = for {
      liftfield <- input
      liftvalue <- toScalaAST(liftfield.value)
    } yield {
      unsafe.JField(liftfield.name, liftvalue)
    }

    resultingFields.toArray
  }
}
