package net.liftweb.json

import scala.reflect.Manifest

/** Abstraction for runtime class and type args (Scala 2.13 variant). */
trait TypeExtractor[A] {
  def runtimeClass: Class[_]
  def typeArguments: List[TypeExtractor[_]]
}


/** Scala 2.13 implementation backed by Manifest. */
object TypeExtractor extends LowPriorityTypeExtractor {
  implicit def fromManifest[A](implicit mf: Manifest[A]): TypeExtractor[A] = new TypeExtractor[A] {
    def runtimeClass: Class[_] = mf.runtimeClass
    def typeArguments: List[TypeExtractor[_]] = mf.typeArguments.map(arg => fromManifest(arg))
  }
}

/** Low-priority fallback kept for compatibility; ClassTag has no type args. */
trait LowPriorityTypeExtractor {
  import scala.reflect.ClassTag
  implicit def fromClassTag[A](implicit ct: ClassTag[A]): TypeExtractor[A] = new TypeExtractor[A] {
    def runtimeClass: Class[_] = ct.runtimeClass
    def typeArguments: List[TypeExtractor[_]] = Nil
  }
}


