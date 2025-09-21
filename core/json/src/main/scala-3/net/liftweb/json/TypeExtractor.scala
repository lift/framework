package net.liftweb.json

import scala.quoted.*

/** Abstraction for runtime class and type args (Scala 3 variant). */
trait TypeExtractor[A] {
  def runtimeClass: Class[_]
  def typeArguments: List[TypeExtractor[_]]
}

/** Scala 3 implementation - hybrid approach using ClassTag + macro type arguments. */
object TypeExtractor extends LowPriorityTypeExtractor {
  import scala.reflect.ClassTag
  // High priority hybrid implementation
  given hybridTypeExtractor[A](using ct: ClassTag[A]): TypeExtractor[A] =
    HybridTypeExtractor.create[A](ct)
}

trait LowPriorityTypeExtractor {
  import scala.reflect.ClassTag
  // This should rarely be used due to higher priority hybrid version
  given fromClassTag[A](using ct: ClassTag[A]): TypeExtractor[A] = new TypeExtractor[A] {
    def runtimeClass: Class[_] = ct.runtimeClass
    def typeArguments: List[TypeExtractor[_]] = Nil
  }
}

/** Hybrid implementation that uses ClassTag for runtime class and tries macro for type args */
object HybridTypeExtractor {
  import scala.reflect.ClassTag

  inline def create[A](ct: ClassTag[A]): TypeExtractor[A] = new TypeExtractor[A] {
    def runtimeClass: Class[_] = ct.runtimeClass

    def typeArguments: List[TypeExtractor[_]] = {
      // Try to derive type arguments using macro, fall back to empty list
      try {
        val derived = derivedFor[A]
        derived.typeArguments
      } catch {
        case _: Exception => Nil
      }
    }
  }
}

