package net.liftweb
package mapper

import org.specs2.mutable.Specification

class NoNoArgCtorModel(val x: Int) extends LongKeyedMapper[NoNoArgCtorModel] with IdPK {
  def getSingleton = NoNoArgCtorModel
}
object NoNoArgCtorModel extends NoNoArgCtorModel(1) with LongKeyedMetaMapper[NoNoArgCtorModel]

/**
 * Pins the reflection behavior of MetaMapper.createInstance after the
 * Class.newInstance -> Constructor.newInstance modernization.
 *
 * A model without a no-arg constructor must fail with NoSuchMethodException
 * (previously InstantiationException on some JDK paths); the test asserts
 * the exact failure type so a future JDK or refactor drift is caught.
 */
class MetaMapperCreateInstanceSpec extends Specification {

  "MetaMapper.createInstance" should {
    "throw NoSuchMethodException for a model without a no-arg constructor" in {
      NoNoArgCtorModel.createInstance must throwA[NoSuchMethodException]
    }
  }
}
