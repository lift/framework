package net.liftweb.json

import scala.quoted.*
import scala.reflect.ClassTag

/** Abstraction for runtime class and type args (Scala 3 variant). */
trait TypeExtractor[A] {
  def runtimeClass: Class[_]
  def typeArguments: List[TypeExtractor[_]]
}

/** Scala 3 implementation - hybrid ClassTag + macro. */
object TypeExtractor extends LowPriorityTypeExtractor {
  // High priority hybrid implementation
  inline given hybridTypeExtractor[A](using ct: ClassTag[A]): TypeExtractor[A] = {
    // Always use ClassTag for runtime class, try macro for type arguments
    val typeArgs = try {
      val derived = derivedFor[A]
      derived.typeArguments
    } catch {
      case _: Exception => Nil
    }

    new TypeExtractor[A] {
      def runtimeClass: Class[_] = ct.runtimeClass
      def typeArguments: List[TypeExtractor[_]] = typeArgs
    }
  }
}

trait LowPriorityTypeExtractor {
  // This should rarely be used due to higher priority macro version
  given fromClassTag[A](using ct: ClassTag[A]): TypeExtractor[A] = new TypeExtractor[A] {
    def runtimeClass: Class[_] = ct.runtimeClass
    def typeArguments: List[TypeExtractor[_]] = Nil
  }
}

