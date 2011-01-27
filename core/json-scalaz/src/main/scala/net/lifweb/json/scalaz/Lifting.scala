package net.liftweb.json.scalaz

import scalaz._
import Scalaz._
import net.liftweb.json._

trait Lifting { this: Types =>
  implicit def Func2ToJSON[A: JSONR, B: JSONR, R](z: (A, B) => R) = new {
    def applyJSON(a: JValue => Result[A], b: JValue => Result[B]): JValue => Result[R] = 
        (json: JValue) => (a(json) |@| b(json))(z)
  }

  implicit def Func3ToJSON[A: JSONR, B: JSONR, C: JSONR, R](z: (A, B, C) => R) = new {
    def applyJSON(a: JValue => Result[A], b: JValue => Result[B], c: JValue => Result[C]): JValue => Result[R] = 
      (json: JValue) => (a(json) |@| b(json) |@| c(json))(z)
  }

  implicit def Func4ToJSON[A: JSONR, B: JSONR, C: JSONR, D: JSONR, R](z: (A, B, C, D) => R) = new {
    def applyJSON(a: JValue => Result[A], b: JValue => Result[B], c: JValue => Result[C], d: JValue => Result[D]): JValue => Result[R] = 
      (json: JValue) => (a(json) |@| b(json) |@| c(json) |@| d(json))(z)
  }

  implicit def Func5ToJSON[A: JSONR, B: JSONR, C: JSONR, D: JSONR, E: JSONR, R](z: (A, B, C, D, E) => R) = new {
    def applyJSON(a: JValue => Result[A], b: JValue => Result[B], c: JValue => Result[C], d: JValue => Result[D], e: JValue => Result[E]): JValue => Result[R] = 
      (json: JValue) => (a(json) |@| b(json) |@| c(json) |@| d(json) |@| e(json))(z)
  }

  implicit def Func6ToJSON[A: JSONR, B: JSONR, C: JSONR, D: JSONR, E: JSONR, F: JSONR, R](z: (A, B, C, D, E, F) => R) = new {
    def applyJSON(a: JValue => Result[A], b: JValue => Result[B], c: JValue => Result[C], d: JValue => Result[D], e: JValue => Result[E], f: JValue => Result[F]): JValue => Result[R] = 
      (json: JValue) => (a(json) |@| b(json) |@| c(json) |@| d(json) |@| e(json) |@| f(json))(z)
  }

  implicit def Func7ToJSON[A: JSONR, B: JSONR, C: JSONR, D: JSONR, E: JSONR, F: JSONR, G: JSONR, R](z: (A, B, C, D, E, F, G) => R) = new {
    def applyJSON(a: JValue => Result[A], b: JValue => Result[B], c: JValue => Result[C], d: JValue => Result[D], e: JValue => Result[E], f: JValue => Result[F], g: JValue => Result[G]): JValue => Result[R] = 
      (json: JValue) => (a(json) |@| b(json) |@| c(json) |@| d(json) |@| e(json) |@| f(json) |@| g(json))(z)
  }

  implicit def Func8ToJSON[A: JSONR, B: JSONR, C: JSONR, D: JSONR, E: JSONR, F: JSONR, G: JSONR, H: JSONR, R](z: (A, B, C, D, E, F, G, H) => R) = new {
    def applyJSON(a: JValue => Result[A], b: JValue => Result[B], c: JValue => Result[C], d: JValue => Result[D], e: JValue => Result[E], f: JValue => Result[F], g: JValue => Result[G], h: JValue => Result[H]): JValue => Result[R] = 
      (json: JValue) => (a(json) |@| b(json) |@| c(json) |@| d(json) |@| e(json) |@| f(json) |@| g(json) |@| h(json))(z)
  }
}
