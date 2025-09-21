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
      // Simplified approach: use TypeRepr's direct methods
      val normalized = normalize(tpe)

      // Try the most direct approach first
      def extractClassName(t: TypeRepr): Option[String] = {
        // Extract the type symbol and check if it's a class
        val symbol = t.typeSymbol
        if (symbol.isClassDef) {
          Some(symbol.fullName)
        } else {
          // For applied types, get the constructor symbol
          t match {
            case AppliedType(tycon, _) =>
              val tyconSymbol = tycon.typeSymbol
              if (tyconSymbol.isClassDef) Some(tyconSymbol.fullName) else None
            case _ => None
          }
        }
      }

      extractClassName(normalized) match {
        case Some(name) =>
          // Handle special Scala types that don't map directly to Java classes
          name match {
            case "scala.Any" | "scala.AnyRef" | "scala.AnyVal" | "scala.Nothing" | "scala.Null" =>
              '{ classOf[Object] }.asExprOf[Class[_]]
            case "scala.Unit" =>
              '{ classOf[scala.runtime.BoxedUnit] }.asExprOf[Class[_]]
            case n if n.startsWith("scala.") && n.contains("$") =>
              // Internal Scala types - fallback to Object
              '{ classOf[Object] }.asExprOf[Class[_]]
            case _ =>
              '{ Class.forName(${ Expr(name) }) }.asExprOf[Class[_]]
          }
        case None =>
          '{ classOf[Object] }.asExprOf[Class[_]]
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
