package net.liftweb.json

import scala.quoted.*

inline def derivedFor[A]: TypeExtractor[A] = ${ TypeExtractorDeriver.deriveImpl[A] }

private object TypeExtractorDeriver {
  def deriveImpl[A: Type](using Quotes): Expr[TypeExtractor[A]] = {
    import quotes.reflect.*

    def normalize(tpe: TypeRepr): TypeRepr = tpe.dealias.simplified

    def splitArgs(tpe: TypeRepr): List[TypeRepr] = normalize(tpe) match {
      case AppliedType(_, args) => args.map(normalize)
      case _                    => Nil
    }

    def runtimeClassExpr(tpe: TypeRepr): Expr[Class[_]] = {
      // Use TypeRepr to get class name and use Class.forName
      val normalized = normalize(tpe)
      val symbol = normalized.typeSymbol

      if (symbol.isClassDef) {
        val className = symbol.fullName
        // Handle some known problematic cases
        className match {
          case "scala.Int" => '{ classOf[Int] }
          case "scala.Long" => '{ classOf[Long] }
          case "scala.Double" => '{ classOf[Double] }
          case "scala.Float" => '{ classOf[Float] }
          case "scala.Boolean" => '{ classOf[Boolean] }
          case "scala.Byte" => '{ classOf[Byte] }
          case "scala.Short" => '{ classOf[Short] }
          case "scala.Char" => '{ classOf[Char] }
          case "java.lang.String" | "scala.Predef.String" => '{ classOf[String] }
          case "scala.Unit" => '{ classOf[scala.runtime.BoxedUnit] }
          case _ =>
            try {
              '{ Class.forName(${ Expr(className) }) }
            } catch {
              case _ => '{ classOf[Object] }
            }
        }
      } else {
        '{ classOf[Object] }
      }
    }

    def deriveArg(tpe: TypeRepr): Expr[TypeExtractor[?]] = tpe.asType match {
      case '[x] => deriveImpl[x].asExprOf[TypeExtractor[?]]
    }

    val tpeA = TypeRepr.of[A]
    val args = splitArgs(tpeA)
    val argExtractors: Expr[List[TypeExtractor[?]]] = Expr.ofList(args.map(deriveArg))

    '{
      new TypeExtractor[A] {
        def runtimeClass: Class[_] = ${ runtimeClassExpr(TypeRepr.of[A]) }
        def typeArguments: List[TypeExtractor[_]] =
          $argExtractors.asInstanceOf[List[TypeExtractor[_]]]
      }
    }
  }
}
